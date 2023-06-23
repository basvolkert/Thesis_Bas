library(quantmod)
start_date <- "2022-05-11"
end_date <- "2022-12-30"
ranges <- data.frame (
  x = c(1,3,5,10,10,10),
  y = c(1,3,5,10,15,20)
)

getting_data <- function(start_date,end_date){
  
  ff_data <- read.table("ff_data.csv",header=TRUE, sep=",")
  tickers <- c("MSFT", "GOOGL", "VZ", "BIDU")
  returns_table <- data.frame(Date = character(), stringsAsFactors = FALSE)
  
  for (ticker in tickers) { 
    stock_data <- getSymbols(ticker, from = start_date, to = end_date, auto.assign = FALSE)
    daily_returns <- dailyReturn(stock_data, type = "log")
    colnames(daily_returns) <- ticker
    if (nrow(returns_table) == 0) {
      returns_table <- daily_returns
    } else {
      returns_table <- merge(returns_table, daily_returns, all = TRUE)
    }
  }
  combined_df <-cbind(ff_data, returns_table)
  return(combined_df)
}

splitting_data_train <- function(ff_data,e, g, x, y){ 
  N <- 141
  start <- N - x - g -e
  end <- N - x - g 
  train_data <- ff_data[start:end, ]
  return(train_data)
}

splitting_data_test <- function(ff_data, e, g, x , y){ 
  N <- 141
  start <- N - x
  end <- N + y
  test_data <- ff_data[start:end, ]
  return(test_data)
}

data <- getting_data(start_date,end_date)

Loop_data <- function(ff_data, ranges) {
  train_tables <- list()
  test_tables <- list()
  
  for (i in seq_len(nrow(ranges))) {
    x_val <- ranges$x[i]
    y_val <- ranges$y[i]
    
    train_data <- splitting_data_train(ff_data, 120, 10, x_val, y_val)
    test_data <- splitting_data_test(ff_data, 120, 10, x_val, y_val)
    
    train_tables[[i]] <- train_data
    test_tables[[i]] <- test_data
  }
  
  names(train_tables) <- paste("TrainTable", seq_len(nrow(ranges)), sep = "_")
  names(test_tables) <- paste("TestTable", seq_len(nrow(ranges)), sep = "_")
  
  return(list(train_tables = train_tables, test_tables = test_tables))
}

# Call the function
result <- Loop_data(data, ranges)

# Access the train tables
train_tables <- result$train_tables

# Access the test tables
test_tables <- result$test_tables

# Print the names of the train tables
print(names(train_tables))