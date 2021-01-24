# File containing general functions to be used in the portfolio optimisation package

library(tidyquant)
library(tidyverse)
library(magrittr)
library(methods)
library(ggplot2)
library(dplyr)


# To get data for BFK.B and BF.A we need to replace . with -
# otherwise Yahoo Fiance will have a fit
sp500_names <- gsub("[.]", "-", tidyquant::tq_index("sp500")$symbol)

#' @param data List of data frames to search for missing rows in
#' @return Returns symbols that could be missing data
missing_data <- function(data) {
  names(data[lapply(data, nrow) != max(unique(sapply(data, nrow)))])
}

#' @param data Remove missing data from this dataset
#' @return Returns the modified data frame
remove_missing_data <- function(data) {
  names_to_delete <- missing_data(data)
  data <- data[names(data) %in% names_to_delete == FALSE]
  return(data)
}

#' Downloads stock data from yahoo finance
#' @param stock_names Vector including the symbols of stocks we want to retrieve
#' @param from Date to get data after
#' @param to Date to get data until
#' @param remove_missing Removes the data with missing rows
#' @return List of dataframes containg data for each sp500 company
get_sp500_data <- function(stock_names = sp500_names,
                           from = "2019-11-27",
                           to = Sys.Date(), 
                           remove_missing = TRUE,
                           show_output=FALSE,
                           bind=FALSE) {
  data <- list()
  for (i in seq_len(length(stock_names))) {
    if (show_output) print(stock_names[i])
    data[[i]] <- as.data.frame(tidyquant::tq_get(stock_names[i],
                                                 get = "stock.prices",
                                                 from = from,
                                                 to = to))
    names(data)[i] <- stock_names[i]
  }
  
  if (remove_missing) {
    data <- remove_missing_data(data)
  }
  if (bind) data = bind_rows(data)
  
  return(data)
}

#' Saves a list of data frames to csv files
#' @param data List of dataframes to save as csv files
#' @param dir_path Directory that the csv files will be written to
save_sp500_data <- function(data, dir_path) {
  for (i in seq_len(length(data))) {
    write.csv(data[[i]],
              paste(dir_path, names(data)[i], ".csv", sep = ""),
              row.names = FALSE)
  }
}

#' Loads data from a csv file
#' @param dir_path Directory to load data from
#' @return List of dataframes for each file contained in dir_path
load_sp500_data <- function(dir_path) {
  # Loads data from csv files into a list
  file_names <- list.files(path = dir_path,
                           pattern = "*.csv",
                           full.names = TRUE)
  data <- lapply(file_names, read.csv)
  
  # Name list elements by their file names
  names(data) <- sub(pattern = "(.*)\\..*$",
                     replacement = "\\1",
                     basename(file_names))
  
  return(data)
}


























