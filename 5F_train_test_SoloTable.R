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

ranges <- data.frame(
  x = c(1, 3, 5, 10, 10, 10),
  y = c(1, 3, 5, 10, 15, 20)
)

Loop_data <- function(ff_data, ranges) {
  for (i in seq_len(nrow(ranges))) {
    x_val <- ranges$x[i]
    y_val <- ranges$y[i]
    
    train_data <- splitting_data_train(ff_data, 120, 10, x_val, y_val)
    assign(paste("train_table", i, sep = "_"), train_data)
    
    test_data <- splitting_data_test(ff_data, 120, 10, x_val, y_val)
    assign(paste("test_table", i, sep = "_"), test_data)
  }
}

# Call the function
Loop_data(data, ranges)

# Access the individual train tables
train_table_1
print(train_table_2
# ...
train_table_12

# Access the individual test tables
test_table_1
test_table_2
# ...
test_table_12
