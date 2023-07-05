library(quantmod)
start_date <- "2022-05-11"
end_date <- "2022-12-30"
ranges <- data.frame (x = c(1,3,5,10,10,10),y = c(1,3,5,10,15,20))

#1. test function ----
# Creating a function that creates a data set with all the factors and all the daily returns of the companies
getting_data <- function(start_date,end_date){
  
  # Calling the ff_data.csv and assigning the tickers
  ff_data <- read.table("ff_data.csv",header=TRUE, sep=",")
  tickers <- c("MSFT", "GOOGL", "VZ", "BIDU","VTI")
  
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


#2 Bigger lines of code  ----

# Function in order to assign the right factors for the Train data set
fitting_train_factors_MSFT <- function(train_table, test_table){
  train_regression <- lm(MSFT - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[8]
  print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  p <- predict(train_regression, test_table)
  #print(a)
  sum_p <- sum(p)
  #a <- sum(test_table$MSFT)
  #c <- b - sum_p
  
  a <- test_table$MSFT
  print(p)
  print(a)
  ap <-cbind(a, p)
  print(ap)
  #ap$a_p <- ap$a- ap$p
  
  
  
  #print(test_table$MSFT)
  #print(paste("total predicted return of MSFT:", sum_a))
  #print(paste("total actual return of MSFT:", b))
  #print(paste("Actual - predicted return of MSFT:", c))
  #print(" ")
  return(ap)
} 

MSFT_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_MSFT(train_table1, test_table1)
  #fitting_train_factors_MSFT(train_table2, test_table2)
  #fitting_train_factors_MSFT(train_table3, test_table3)
  #fitting_train_factors_MSFT(train_table4, test_table4)
  #fitting_train_factors_MSFT(train_table5, test_table5)
  #fitting_train_factors_MSFT(train_table6, test_table6)
}
MSFT_div <- MSFT_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)









GOOGL_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_GOOGL(train_table1 ,test_table1)
  fitting_train_factors_GOOGL(train_table2, test_table2)
  fitting_train_factors_GOOGL(train_table3, test_table3)
  fitting_train_factors_GOOGL(train_table4, test_table4)
  fitting_train_factors_GOOGL(train_table5, test_table5)
  fitting_train_factors_GOOGL(train_table6, test_table6)
}
GOOGL_div <- GOOGL_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

VZ_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_VZ(train_table1 ,test_table1)
  fitting_train_factors_VZ(train_table2, test_table2)
  fitting_train_factors_VZ(train_table3, test_table3)
  fitting_train_factors_VZ(train_table4, test_table4)
  fitting_train_factors_VZ(train_table5, test_table5)
  fitting_train_factors_VZ(train_table6, test_table6)
}
VZ_div <- VZ_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

BIDU_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_BIDU(train_table1 ,test_table1)
  fitting_train_factors_BIDU(train_table2, test_table2)
  fitting_train_factors_BIDU(train_table3, test_table3)
  fitting_train_factors_BIDU(train_table4, test_table4)
  fitting_train_factors_BIDU(train_table5, test_table5)
  fitting_train_factors_BIDU(train_table6, test_table6)
}
BIDU_div <- BIDU_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

VTI_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_VTI(train_table1 ,test_table1)
  fitting_train_factors_VTI(train_table2, test_table2)
  fitting_train_factors_VTI(train_table3, test_table3)
  fitting_train_factors_VTI(train_table4, test_table4)
  fitting_train_factors_VTI(train_table5, test_table5)
  fitting_train_factors_VTI(train_table6, test_table6)
}
VTI_div <- VTI_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)



# Function in order to assign the right factors for the Train data set
fitting_train_factors_GOOGL <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  a <- predict(train_regression, test_table)
  sum_a <- sum(a)
  b <- sum(test_table$GOOGL)
  c <- b - sum_a 
  print(paste("total predicted return of GOOGL:", sum_a))
  print(paste("total actual return of GOOGL:", b))
  print(paste("Actual - predicted return of GOOGL:", c))
  print(" ")
  return(c)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_VZ <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  a <- predict(train_regression, test_table)
  sum_a <- sum(a)
  b <- sum(test_table$VZ)
  c <- b - sum_a 
  print(paste("total predicted return of VZ:", sum_a))
  print(paste("total actual return of VZ:", b))
  print(paste("Actual - predicted return of VZ:", c))
  print(" ")
  return(c)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_BIDU <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  a <- predict(train_regression, test_table)
  sum_a <- sum(a)
  b <- sum(test_table$BIDU)
  c <- b - sum_a 
  print(paste("total predicted return of BIDU:", sum_a))
  print(paste("total actual return of BIDU:", b))
  print(paste("Actual - predicted return of BIDU:", c))
  print(" ")
  return(c)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_VTI <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  a <- predict(train_regression, test_table)
  sum_a <- sum(a)
  b <- sum(test_table$VTI)
  c <- b - sum_a 
  print(paste("total predicted return of VTI:", sum_a))
  print(paste("total actual return of VTI:", b))
  print(paste("Actual - predicted return of VTI:", c))
  print(" ")
  return(c)
} 



