pft <- portfolio$new(assets = c("AAPL", "MSFT", "MMM", "T"), from = "2021-01-01")
pft$mean_variance_optimize(0.0001, short = FALSE)

pft2 <- portfolio$new(assets = c("AAPL", "MSFT", "MMM", "T"), from = "2021-01-01")
pft2$mean_semivariance_optimize(0.0001, short = FALSE)

w1 <- pft$weights
w2 <- pft2$weights

expected_returns <- apply(pft$returns_matrix, 2, mean)

test_that("weights sum to one", {
    expect_equal(sum(w1), 1)
    expect_equal(sum(w2), 1)
})

test_that("weights give expected return", {
    expect_equal(sum(expected_returns * w1), 0.0001, tolerance = 1e-4)
    expect_equal(sum(expected_returns * w2), 0.0001, tolerance = 1e-4)
})

test_that("weights are positive", {
     for (i in 1:length(w1)) {
        expect_gt(w1[i], -1e-4)
        expect_gt(w2[i], -1e-4)
    }
 })
