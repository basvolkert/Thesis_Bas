#T0_start_dates <- c("2022-05-24", "2022-05-20", "2022-05-18", "2022-05-11", "2022-05-11", "2022-05-11")
#Tg_end_dates <- c("2022-11-14", "2022-11-10", "2022-11-08", "2022-11-01", "2022-11-01", "2022-11-01")
#T1_start_dates <- c("2022-11-29", "2022-11-25", "2022-11-22", "2022-11-15", "2022-11-15", "2022-11-15")
#T2_end_dates <- c("2022-12-01", "2022-12-05", "2022-12-07", "2022-12-14", "2022-12-21", "2022-12-29")


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
data <- getting_data("2022-05-11","2022-12-30")

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

x <- splitting_data_train(data,120,10,5,5)
y <- splitting_data_test(data, 12 ,10, 5,5)
