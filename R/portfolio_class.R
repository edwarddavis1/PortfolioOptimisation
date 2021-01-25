#' Reference class for portfolio object
#' 
#' @field assets n-list of asset symbols e.g. c("AAPL", "KO")
#' @field data A single data frame containing each asset
#' @field shares n-vector of numbers of shares for each asset
#' @field holdings n-vector of holdings for each asset
#' @field time Current time of the portfolio (time points from start)
#' @field value Total value of the portfolio
#' @field weights n-vector of asset weightings
#' @field T Number of data points for each asset
#' @field investment Amount invested in the portfolio
#' @field returns_matrix Matrix of assets by asset percentage returns  
#' @field asset_summary Summary statistics for assets
#' @method initialize Default constructor
#' @method set_time Selects the current time of the portfolio
#' @method set_weights Setter for the weights field
#' @method ls_optimize Optimises the portfolio using least squares
#' @method mean_variance_optimsize Optimises the portfolio using the mean-variance model
#' @method mean_semivariance_optimize Optimises the portfolio using the mean-semivariance model 
#' @method plot_assets Plots asset prices over time
#' @method plot_value Plots the value of the assets and the portfolio
#'
#' @export
portfolio = setRefClass("portfolio",
                        fields=c(
                          assets = "character",         
                          data = "data.frame",          
                          shares = "matrix",            
                          holdings = "matrix",          
                          time = "numeric",             
                          value = "numeric",
                          weights = "matrix",           
                          T = "numeric",                
                          investment = "numeric",       
                          returns_matrix = "matrix",    
                          asset_summary = "data.frame"  
                        ))
portfolio$methods(

  #' @param from First date of the portfolio range
  #' @param to Last date of the portfolio range
  initialize = function(assets=sp500_names, weights=NULL, investment=1e4, 
                        from="2019-11-27", to = Sys.Date()) {
    
    .self$value = investment
    .self$investment = investment
    .self$assets = sort(assets)
    .self$time = 1
    n = length(.self$assets)  # Number of assets
    .self$holdings = as.matrix(rep(.self$value, n))

    # Check that the selected weighting vector is okay
    if (is.null(weights)) const_weight = TRUE
    else {
      const_weight = FALSE
      if (abs(sum(weights) - 1) >= 1e-10) stop("Error: Weights do not add to 1")
      if (length(weights) != n) stop("Error: Weights vector and assets vector are different lengths")
    }

    # By default have a constant weight portfolio
    if (const_weight) .self$weights = as.matrix(rep(1/n, n))
    else .self$weights = as.matrix(weights)

    # Create a data frame containing stock data for each asset within the defined date range
    .self$data = get_sp500_data(.self$assets, from=from, to=to, bind=TRUE)
    .self$T = n_distinct(.self$data$date)

    # Calculate the initial shares based on the investment
    temp_shares = c()
    for (i in 1:n) {
      
      # Get initial number of shares from the initial stock prices
      initial_price = (.self$data %>% filter(symbol == .self$assets[i] & date == date[1]))$adjusted
      if (initial_price > 0) temp_shares[i] = .self$holdings[i] / initial_price
      # If the stock price is zero throw a warning
      else {
        temp_shares[i] = 0
        warning("Warning: Asset does not yet exist!")
      }
      
    }
    
    # Save shares to member data
    .self$shares = as.matrix(temp_shares)
    
    # Asset returns using adjusted price (making sure the first value for an asset is 0)
    .self$data %<>% 
        mutate(asset_return = ifelse(as.Date(date) > as.Date(lag(date, 1)), adjusted - lag(adjusted, 1), 0),
               asset_percentage_return = ifelse(as.Date(date) > as.Date(lag(date, 1)), (adjusted - lag(adjusted, 1))/lag(adjusted, 1) * 100, 0))
    .self$data$asset_return[1] = 0
    .self$data$asset_percentage_return[1] = 0
    
    
    # Trading periods per year
    # P = (.self$data %>% filter(symbol == .self$assets[1],
    #                     grepl("2019", date)) %>% dim)[1]
    P = 252  # Hard coded in case the user doesn't select a whole year
    
    # Create asset summary statistics
    .self$asset_summary = .self$data %>% 
      group_by(symbol) %>% 
      summarise(annualised_return = P * mean(asset_return),
                annualised_risk = P * sd(asset_return),
                avg_return = mean(asset_return),
                avg_percentage_return = mean(asset_percentage_return),
                risk = sd(asset_percentage_return),
                .groups="drop")
    

    # Calculate the returns matrix 
    .self$returns_matrix = .self$data %>% 
                              select(symbol, asset_percentage_return) %>%
                              unstack(asset_percentage_return ~ symbol) %>%
                              na.omit() %>%
                              as.matrix()
    
    # Calculate member data at the end of the time period
    set_time(.self$T)
    
  },

  #' @param update Boolean, if true the member data is updated according to the time, otherwise the value for that time is returned only
  set_time = function(time, update=TRUE) {

    # Must be within time range
    if (time < 1 | time > .self$T) stop("Error: Time not in range")
    # Must be integer
    if (time%%1 != 0) stop("Error: Time must be an integer value")

    # Update temporary variables 
    temp_time = time
    new_date = unique(.self$data["date"])[[1]][time]

    new_prices = as.matrix((.self$data %>% filter(date == new_date))$adjusted)
    temp_holdings = new_prices * .self$shares

    temp_value = (t(.self$weights) %*% temp_holdings)[1]

    # Update member data if specified
    if (update) {
      .self$time = temp_time
      .self$holdings = temp_holdings
      .self$value = temp_value
    } else return(temp_value)

  },
  
  #' @param new_weights Weights vector to be set as member data
  set_weights = function(new_weights) {
    # Check weights are okay
    if (sum(new_weights) != 1) stop("Error: Weights do not add to 1")
    if (length(new_weights) != length(.self$assets)) stop("Error: Weights vector and assets vector are different lengths")
    .self$weights = as.matrix(new_weights)
  },
  
  #' @param desired.return The return that the optimization will optimize for 
  ls_optimize = function(desired.return) {
    # mean returns
    mu <- matrix(apply(.self$returns_matrix, MARGIN = 2, FUN = mean))
    # vector of ones
    one <- matrix(rep(1, ncol(.self$returns_matrix)))
    # number of investment periods
    tee <- nrow(.self$returns_matrix)
    # desired return
    rho <- desired.return/tee
    
    # need to work out how to make the big matrix out of the above constituents
    rhs.vec <- as.matrix(c(2*rho*tee*mu, 1, rho), ncol = 1)
    
    # Making the larger matrix
    top.left <- 2*t(.self$returns_matrix)%*%.self$returns_matrix
    n.stocks <- ncol(.self$returns_matrix)
    bottom.right <- matrix(rep(0,4), nrow = 2)
    big.matrix <- matrix(rep(0,(n.stocks + 2)^2), ncol = (n.stocks + 2))
    big.matrix[1:n.stocks,1:n.stocks] <- top.left
    big.matrix[,(n.stocks + 2)-1] <- c(one,0,0)
    big.matrix[(n.stocks + 2)-1,] <- c(one,0,0)
    big.matrix[,(n.stocks + 2)] <- c(mu,0,0)
    big.matrix[(n.stocks + 2),] <- c(mu,0,0)
    
    # solving
    w_z_z = solve(big.matrix, rhs.vec)
    
    # Ignore z elements in the solution
    .self$weights = head(w_z_z, length(.self$assets))
    
    # Update value 
    set_time(.self$T)
    
    return(.self$weights)
  },
  
  #' @param desired_daily_return A numeric value which is the desired DAILY return for the portfolio
  #' @return A vector containing the weights for each asset in the desired portfolio
  mean_variance_optimize = function(desired_daily_return,
                                      short = FALSE) {
    returns = .self$returns_matrix
    expected_returns <- apply(returns, 2, mean)
    covariance <- cov(returns, returns)
    
    if (short) {
      
      temp_weights = as.matrix(quadprog::solve.QP(covariance,
                                   rep(0, ncol(returns)),
                                   Amat = t(rbind(rep(1, length(expected_returns)),
                                                  expected_returns)),
                                   bvec = c(1,
                                            desired_daily_return),
                                   meq = 2
                                  )$solution)
      .self$weights = temp_weights
      return(temp_weights)
      
    } else {
      temp_weights = as.matrix(quadprog::solve.QP(covariance,
                                   rep(0, ncol(returns)),
                                   Amat = t(rbind(rep(1, length(expected_returns)),
                                                  expected_returns,
                                                  diag(length(expected_returns)))),
                                   bvec = c(1,
                                            desired_daily_return,
                                            rep(0, length(expected_returns))),
                                   meq = 2,
                                  )$solution)
      .self$weights = temp_weights
      return(temp_weights)
    }
    
  },
  
  #' @param desired_daily_return A numeric value which is the desired DAILY return for the portfolio
  #' @return A vector containing the weights for each asset in the desired portfolio
  mean_semivariance_optimize = function(desired_daily_return,
                                          short = FALSE) {
    returns = .self$returns_matrix
    C <- diag(c(rep(0, nrow(returns) + ncol(returns)), rep(1, nrow(returns))))
    m <- c(apply(returns, 2, mean), rep(0, 2 * nrow(returns)))
    a <- c(rep(1, ncol(returns)), rep(0, 2 * nrow(returns)))
    b <- (1 / sqrt(nrow(returns))) * scale(returns)

    if (short) {
      temp_weights = as.matrix(limSolve::lsei(A = C,
                              B = rep(0, ncol(C)),
                              E = rbind(a, m,
                                        cbind(b, diag(x = -1, nrow(returns)), diag(nrow(returns)))),
                              F = c(1, desired_daily_return, rep(0, nrow(b))),
                              )$X[seq_len(ncol(returns))])
      .self$weights = temp_weights
      return(temp_weights)
    } else {
      temp_weights = as.matrix(limSolve::lsei(A = C,
                              B = rep(0, ncol(C)),
                              E = rbind(a, m,
                                        cbind(b, diag(x = -1, nrow(returns)), diag(nrow(returns)))),
                              F = c(1, desired_daily_return, rep(0, nrow(b))),
                              G = cbind(diag(ncol(returns)), matrix(data = 0, ncol = 2 * nrow(returns), nrow = ncol(returns))),
                              H = rep(0, ncol(returns))
                              )$X[seq_len(ncol(returns))])
      .self$weights = temp_weights
      return(temp_weights)
    }
  },

  plot_assets = function() {
    ggplot(.self$data, aes(date, adjusted, col=symbol)) +
      geom_line() +
      ylab("Adjusted Stock Price")
  },
  

  #' @param return_values Boolean, if true returns portfolio value time series data frame
  plot_value = function(inc_portfolio_value=TRUE, return_values=FALSE) {

    if (inc_portfolio_value) {
      # Calculate the portfolio value over time
      values_over_time = c()
      for (i in 1:.self$T) {
        # Don't update member as to not confuse user
        values_over_time[i] = .self$set_time(i, update=FALSE)
      }
      # Create separate data frame for the portfolio value data
      values_over_time_df = data.frame(
        "date"=(.self$data %>% filter(symbol==assets[1]))$date,
        "values"=values_over_time
      )
    }

    # Calculate values for assets over time (in a separate data frame)
    asset_value_data = .self$data
    asset_values = c()
    for (i in 1:length(.self$assets)) {

      prices = (asset_value_data %>% filter(symbol == assets[i]))$adjusted %>% as.matrix
      asset_values = append(asset_values, prices * shares[i])
    }
    asset_value_data$value = asset_values

    # If selected, include the portfolio value in the plot
    value_plot = ggplot()
    asset_alpha = 1
    if (inc_portfolio_value) {
      portfolio_lab = c("Portfolio")
      value_plot = value_plot +
        geom_line(data=values_over_time_df, aes(date, values_over_time, col=portfolio_lab))
      asset_alpha = 0.4
    }

    # Plot the asset values
    value_plot = value_plot +
      geom_line(data=asset_value_data, aes(date, value, col=symbol), alpha=asset_alpha) +
      scale_color_discrete(name="Asset") +
      ylab("Value")
    
    if (return_values) return(values_over_time_df)
    else return(value_plot)
  }
)

