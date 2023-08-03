library(quantmod)
start_date <- "2022-05-11"
end_date <- "2022-12-30"
ranges <- data.frame (x = c(1,3,5,10,10,10),y = c(1,3,5,10,15,20))

#1. Getting and sorting the Data ----
# Creating a function that creates a data set with all the factors and all the daily returns of the companies
getting_data <- function(start_date,end_date){
  
  # Calling the ff_data.csv and assigning the tickers
  ff_data <- read.table("ff_data.csv",header=TRUE, sep=",")
  tickers <- c("MSFT", "GOOGL", "VZ", "BIDU")
  
  # Creating a returns table
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
  # Combining Factor data and stock data in to one data frame
  combined_df <-cbind(ff_data, returns_table)
  return(combined_df)
}

# Creating a data frame that includes all the factor and daily return data for the given start and end data.
data <- getting_data(start_date,end_date)

# Making a function in order to split the data, train data
splitting_data_train <- function(ff_data,e, g, x, y){ 
  N <- 141
  start <- N - x - g -e
  end <- N - x - g 
  train_data <- ff_data[start:end, ]
  return(train_data)
}

# Making a function in order to split the data, test data
splitting_data_test <- function(ff_data, e, g, x , y){ 
  N <- 141
  start <- N - x
  end <- N + y
  test_data <- ff_data[start:end, ]
  return(test_data)
}

# Making a function in order to create data sets for the different event windows
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

# Access the train and test tables
train_tables <- result$train_tables
test_tables <- result$test_tables 

# Extracting the data from the train_tables
for (i in seq_along(train_tables)) {
  table <- train_tables[[i]]  #
  table_name <- paste0("train_table", i)  
  assign(table_name, table)
}

# Extracting the data from the test_tables
for (i in seq_along(test_tables)) {
  table <- test_tables[[i]]  #
  table_name <- paste0("test_table", i)  
  assign(table_name, table)
}





#2.1 MSFT 5 Factor lines ----
# Function in order to assign the right factors for the Train data set; MSFT, 5 Factor
fitting_train_factors_MSFT_5F <- function(train_table, test_table){
  train_regression_5F <- lm(MSFT - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[8]
  print(summary(train_regression_5F))
  p <- predict(train_regression, test_table)
  a <- test_table$MSFT
  sum_p <- sum(p)
  sum_a <- sum(test_table$MSFT)
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; MSFT, 4 Factor
fitting_train_factors_MSFT_4F <- function(train_table, test_table){
  train_regression <- lm(MSFT - RF ~ Mkt.RF + SMB + HML + RMW, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[8]
  p <- predict(train_regression, test_table)
  a <- test_table$MSFT
  sum_p <- sum(p)
  sum_a <- sum(test_table$MSFT)
  ap <-cbind(a, p)
  
  return(ap)
}
# Function in order to assign the right factors for the Train data set; MSFT, 3 Factor
fitting_train_factors_MSFT_3F <- function(train_table, test_table){
  train_regression_5F <- lm(MSFT - RF ~ Mkt.RF + SMB + HML, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[8]
  p <- predict(train_regression, test_table)
  a <- test_table$MSFT
  sum_p <- sum(p)
  sum_a <- sum(test_table$MSFT)
  ap <-cbind(a, p)
  
  return(ap)
}
# Function in order to assign the right factors for the Train data set; MSFT, CAPM
fitting_train_factors_MSFT_F <- function(train_table, test_table){
  train_regression_5F <- lm(MSFT - RF ~ Mkt.RF, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[8]
  p <- predict(train_regression, test_table)
  a <- test_table$MSFT
  sum_p <- sum(p)
  sum_a <- sum(test_table$MSFT)
  ap <-cbind(a, p)
  
  return(ap)
}

# Function to calculate AR, CAR, t-value and P-value for 5 Factor
MSFT_data_5F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_MSFT_5F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_MSFT_5F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_MSFT_5F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_MSFT_5F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_MSFT_5F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_MSFT_5F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 5 Factor model")
  
  print(paste("MSFT:p-value 1", pt1))
  print(paste("MSFT:p-value 2", pt2))
  print(paste("MSFT:p-value 3", pt3))
  print(paste("MSFT:p-value 4", pt4))
  print(paste("MSFT:p-value 5", pt5))
  print(paste("MSFT:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 4 Factor
MSFT_data_4F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_MSFT_4F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_MSFT_4F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_MSFT_4F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_MSFT_4F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_MSFT_4F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_MSFT_4F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 4 Factor model")
  print(paste("MSFT:p-value 1", pt1))
  print(paste("MSFT:p-value 2", pt2))
  print(paste("MSFT:p-value 3", pt3))
  print(paste("MSFT:p-value 4", pt4))
  print(paste("MSFT:p-value 5", pt5))
  print(paste("MSFT:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 3 Factor
MSFT_data_3F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_MSFT_3F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_MSFT_3F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_MSFT_3F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_MSFT_3F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_MSFT_3F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_MSFT_3F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 3 Factor model")
  print(paste("MSFT:p-value 1", pt1))
  print(paste("MSFT:p-value 2", pt2))
  print(paste("MSFT:p-value 3", pt3))
  print(paste("MSFT:p-value 4", pt4))
  print(paste("MSFT:p-value 5", pt5))
  print(paste("MSFT:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for CAPM
MSFT_data_F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_MSFT_F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_MSFT_F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_MSFT_F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_MSFT_F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_MSFT_F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_MSFT_F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for Factor model")
  print(paste("MSFT:t-value 1", pt1))
  print(paste("MSFT:t-value 2", pt2))
  print(paste("MSFT:t-value 3", pt3))
  print(paste("MSFT:t-value 4", pt4))
  print(paste("MSFT:t-value 5", pt5))
  print(paste("MSFT:t-value 6", pt6))
}

# Calling all four functions to received the P-values
MSFT_div_5f <- MSFT_data_5F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
MSFT_div_4f <- MSFT_data_4F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
MSFT_div_3f <- MSFT_data_3F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
MSFT_div_F <- MSFT_data_F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

#2.2 GOOGL 5 Factor lines ----

# Function in order to assign the right factors for the Train data set; GOOGL, 5 factor
fitting_train_factors_GOOGL_5F <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$GOOGL
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; GOOGL, 4 factor
fitting_train_factors_GOOGL_4F <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF + SMB + HML + RMW, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$GOOGL
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; GOOGL, 3 factor
fitting_train_factors_GOOGL_3F <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF + SMB + HML, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$GOOGL
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; GOOGL, CAPM
fitting_train_factors_GOOGL_F <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$GOOGL
  ap <-cbind(a, p)
  
  return(ap)
} 

# Function to calculate AR, CAR, t-value and P-value for 5 Factor
GOOGL_data_5F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_GOOGL_5F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_GOOGL_5F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_GOOGL_5F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_GOOGL_5F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_GOOGL_5F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_GOOGL_5F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 5 Factor model")
  
  print(paste("GOOGL:p-value 1", pt1))
  print(paste("GOOGL:p-value 2", pt2))
  print(paste("GOOGL:p-value 3", pt3))
  print(paste("GOOGL:p-value 4", pt4))
  print(paste("GOOGL:p-value 5", pt5))
  print(paste("GOOGL:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 4 Factor
GOOGL_data_4F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_GOOGL_4F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_GOOGL_4F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_GOOGL_4F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_GOOGL_4F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_GOOGL_4F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_GOOGL_4F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 4 Factor model")
  print(paste("GOOGL:p-value 1", pt1))
  print(paste("GOOGL:p-value 2", pt2))
  print(paste("GOOGL:p-value 3", pt3))
  print(paste("GOOGL:p-value 4", pt4))
  print(paste("GOOGL:p-value 5", pt5))
  print(paste("GOOGL:p-value 6", pt6))
  
}
# Function to calculate AR, CAR, t-value and P-value for 3 Factor
GOOGL_data_3F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_GOOGL_3F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_GOOGL_3F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_GOOGL_3F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_GOOGL_3F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_GOOGL_3F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_GOOGL_3F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 3 Factor model")
  print(paste("GOOGL:p-value 1", pt1))
  print(paste("GOOGL:p-value 2", pt2))
  print(paste("GOOGL:p-value 3", pt3))
  print(paste("GOOGL:p-value 4", pt4))
  print(paste("GOOGL:p-value 5", pt5))
  print(paste("GOOGL:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for CAPM
GOOGL_data_F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_GOOGL_F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_GOOGL_F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_GOOGL_F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_GOOGL_F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_GOOGL_F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_GOOGL_F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for Factor model")
  print(paste("GOOGL:p-value 1", pt1))
  print(paste("GOOGL:p-value 2", pt2))
  print(paste("GOOGL:p-value 3", pt3))
  print(paste("GOOGL:p-value 4", pt4))
  print(paste("GOOGL:p-value 5", pt5))
  print(paste("GOOGL:p-value 6", pt6))
}

# Calling all four functions to received the P-values
GOOGL_div_5F <- GOOGL_data_5F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
GOOGL_div_4f <- GOOGL_data_4F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
GOOGL_div_3F <- GOOGL_data_3F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
GOOGL_div_F <- GOOGL_data_F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

#2.3 VZ 5 Factor lines ----
# Function in order to assign the right factors for the Train data set; VZ, 5 factor
fitting_train_factors_VZ_5F <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VZ
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; VZ, 4 factor
fitting_train_factors_VZ_4F <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF + SMB + HML + RMW, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VZ
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; VZ, 3 factor
fitting_train_factors_VZ_3F <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF + SMB + HML, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VZ
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; VZ, CAPM
fitting_train_factors_VZ_F <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VZ
  ap <-cbind(a, p)
  
  return(ap)
} 

# Function to calculate AR, CAR, t-value and P-value for 5 Factor
VZ_data_5F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VZ_5F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_VZ_5F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_VZ_5F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_VZ_5F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_VZ_5F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_VZ_5F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 5 Factor model")
  
  print(paste("VZ:p-value 1", pt1))
  print(paste("VZ:p-value 2", pt2))
  print(paste("VZ:p-value 3", pt3))
  print(paste("VZ:p-value 4", pt4))
  print(paste("VZ:p-value 5", pt5))
  print(paste("VZ:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 4 Factor
VZ_data_4F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VZ_4F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_VZ_4F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_VZ_4F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_VZ_4F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_VZ_4F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_VZ_4F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 4 Factor model")
  print(paste("VZ:p-value 1", pt1))
  print(paste("VZ:p-value 2", pt2))
  print(paste("VZ:p-value 3", pt3))
  print(paste("VZ:p-value 4", pt4))
  print(paste("VZ:p-value 5", pt5))
  print(paste("VZ:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 3 Factor
VZ_data_3F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VZ_3F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_VZ_3F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_VZ_3F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_VZ_3F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_VZ_3F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_VZ_3F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 3 Factor model")
  print(paste("VZ:p-value 1", pt1))
  print(paste("VZ:p-value 2", pt2))
  print(paste("VZ:p-value 3", pt3))
  print(paste("VZ:p-value 4", pt4))
  print(paste("VZ:p-value 5", pt5))
  print(paste("VZ:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for CAPM
VZ_data_F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VZ_F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_VZ_F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_VZ_F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_VZ_F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_VZ_F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_VZ_F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for Factor model")
  print(paste("VZ:t-value 1", pt1))
  print(paste("VZ:t-value 2", pt2))
  print(paste("VZ:t-value 3", pt3))
  print(paste("VZ:t-value 4", pt4))
  print(paste("VZ:t-value 5", pt5))
  print(paste("VZ:t-value 6", pt6))
}

# Calling all four functions to received the P-values
VZ_div_5f <- VZ_data_5F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VZ_div_4f <- VZ_data_4F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VZ_div_3f <- VZ_data_3F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VZ_div_F <- VZ_data_F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)


#2.4 BIDU 5 Factor lines ----
# Function in order to assign the right factors for the Train data set; BIDU, 5 factor
fitting_train_factors_BIDU_5F <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  print(sum_p)
  a <- test_table$BIDU
  ap <-cbind(a, p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; BIDU, 4 factor
fitting_train_factors_BIDU_4F <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF + SMB + HML + RMW, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$BIDU
  ap <-cbind(a, p)
  print(sum_p)
  
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; BIDU, 3 factor
fitting_train_factors_BIDU_3F <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF + SMB + HML, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$BIDU
  ap <-cbind(a, p)
  print(sum_p)
  
  return(ap)
} 
# Function in order to assign the right factors for the Train data set; BIDU, CAPM
fitting_train_factors_BIDU_F <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$BIDU
  ap <-cbind(a, p)
  print(sum_p)
  
  return(ap)
} 

# Function to calculate AR, CAR, t-value and P-value for 5 Factor
BIDU_data_5F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_BIDU_5F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_BIDU_5F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_BIDU_5F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_BIDU_5F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_BIDU_5F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_BIDU_5F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 5 Factor model")
  
  print(paste("BIDU:p-value 1", pt1))
  print(paste("BIDU:p-value 2", pt2))
  print(paste("BIDU:p-value 3", pt3))
  print(paste("BIDU:p-value 4", pt4))
  print(paste("BIDU:p-value 5", pt5))
  print(paste("BIDU:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 4 Factor
BIDU_data_4F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_BIDU_4F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_BIDU_4F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_BIDU_4F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_BIDU_4F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_BIDU_4F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_BIDU_4F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 4 Factor model")
  print(paste("BIDU:p-value 1", pt1))
  print(paste("BIDU:p-value 2", pt2))
  print(paste("BIDU:p-value 3", pt3))
  print(paste("BIDU:p-value 4", pt4))
  print(paste("BIDU:p-value 5", pt5))
  print(paste("BIDU:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for 3 Factor
BIDU_data_3F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_BIDU_3F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_BIDU_3F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_BIDU_3F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_BIDU_3F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_BIDU_3F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_BIDU_3F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for 3 Factor model")
  print(paste("BIDU:p-value 1", pt1))
  print(paste("BIDU:p-value 2", pt2))
  print(paste("BIDU:p-value 3", pt3))
  print(paste("BIDU:p-value 4", pt4))
  print(paste("BIDU:p-value 5", pt5))
  print(paste("BIDU:p-value 6", pt6))
}
# Function to calculate AR, CAR, t-value and P-value for CAPM
BIDU_data_F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_BIDU_F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = FALSE)
  
  R2 <- data.frame(fitting_train_factors_BIDU_F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = FALSE)
  
  R3 <- data.frame(fitting_train_factors_BIDU_F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = FALSE)
  
  R4 <- data.frame(fitting_train_factors_BIDU_F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = FALSE)
  
  R5 <- data.frame(fitting_train_factors_BIDU_F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = FALSE)
  
  R6 <- data.frame(fitting_train_factors_BIDU_F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = FALSE)
  
  print("These are the values for Factor model")
  print(paste("BIDU:p-value 1", pt1))
  print(paste("BIDU:p-value 2", pt2))
  print(paste("BIDU:p-value 3", pt3))
  print(paste("BIDU:p-value 4", pt4))
  print(paste("BIDU:p-value 5", pt5))
  print(paste("BIDU:p-value 6", pt6))
}

# Calling all four functions to received the P-values
BIDU_div_5f <- BIDU_data_5F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
BIDU_div_4f <- BIDU_data_4F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
BIDU_div_3f <- BIDU_data_3F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
BIDU_div_F <- BIDU_data_F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
