library(quantmod)
start_date <- "2022-05-11"
end_date <- "2022-12-30"
ranges <- data.frame (
  x = c(1,3,5,10,10,10),
  y = c(1,3,5,10,15,20))

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

# Function in order to assign the right factors for the Train data set

train_rmrf <- train_table1[,2]
train_smb <- train_table1[,3]
train_hml <- train_table1[,4]
train_rmw <- train_table1[,5]
train_cma <- train_table1[,6]
train_rf <- train_table1[,7]
train_fund <- train_table1[,8]
train_Fund.xcess <- train_fund - train_rf
train_regression <- lm(MSFT - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table1)
table_name <- deparse(substitute(x))
column_name <-names(x)[8]
  
print(paste("train data of ",column_name, "of table", table_name))
print(summary(train_regression))
  
predict(train_regression, test_table1)
#print(summary(a))  
#print(a)
  #return(a)
  
  
  
MSFT_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  fitting_train_factors_MSFT(train_table1 ,test_table1)
  #fitting_train_factors_MSFT(train_table2, test_table2)
  #fitting_train_factors_MSFT(train_table3, test_table3)
  #fitting_train_factors_MSFT(train_table4, test_table4)
  #fitting_train_factors_MSFT(train_table5, test_table5)
  #fitting_train_factors_MSFT(train_table6, test_table6)
}
MSFT_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)








# Function in order to assign the right factors for the Train data set
fitting_train_factors_GOOGL <- function(x, test){
  train_rmrf <- x[,2]
  train_smb <- x[,3]
  train_hml <- x[,4]
  train_rmw <- x[,5]
  train_cma <- x[,6]
  train_rf <- x[,7]
  train_fund <- x[,9]
  train_Fund.xcess <- train_fund - train_rf
  train_regression <- lm(train_Fund.xcess ~ train_rmrf + train_smb + train_hml + train_rmw + train_cma)
  table_name <- deparse(substitute(x))
  column_name <-names(x)[9]
  
  print(paste("train data of ",column_name, "of table", table_name))
  print(summary(train_regression))
  print(test)
  predict(train_regression, test)
}
# Function in order to assign the right factors for the Train data set
fitting_train_factors_VZ <- function(x){
  train_rmrf <- x[,2]
  train_smb <- x[,3]
  train_hml <- x[,4]
  train_rmw <- x[,5]
  train_cma <- x[,6]
  train_rf <- x[,7]
  train_fund <- x[,10]
  train_Fund.xcess <- train_fund - train_rf
  train_regression <- lm(train_Fund.xcess ~ train_rmrf + train_smb + train_hml + train_rmw + train_cma)
  table_name <- deparse(substitute(x))
  column_name <-names(x)[10]
  
  print(paste("train data of ",column_name, "of table", table_name))
  print(summary(train_regression))
}
# Function in order to assign the right factors for the Train data set
fitting_train_factors_BIDU <- function(x){
  train_rmrf <- x[,2]
  train_smb <- x[,3]
  train_hml <- x[,4]
  train_rmw <- x[,5]
  train_cma <- x[,6]
  train_rf <- x[,7]
  train_fund <- x[,11]
  train_Fund.xcess <- train_fund - train_rf
  train_regression <- lm(train_Fund.xcess ~ train_rmrf + train_smb + train_hml + train_rmw + train_cma)
  table_name <- deparse(substitute(x))
  column_name <-names(x)[11]
  
  print(paste("train data of ",column_name, "of table", table_name))
  print(summary(train_regression))
  return(train_regression)
}



GOOGL_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6){
  fitting_train_factors_GOOGL(train_table1)
  fitting_train_factors_GOOGL(train_table2)
  fitting_train_factors_GOOGL(train_table3)
  fitting_train_factors_GOOGL(train_table4)
  fitting_train_factors_GOOGL(train_table5)
  fitting_train_factors_GOOGL(train_table6)
  
}
GOOGL_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6)

VZ_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6){
  fitting_train_factors_VZ(train_table1)
  fitting_train_factors_VZ(train_table2)
  fitting_train_factors_VZ(train_table3)
  fitting_train_factors_VZ(train_table4)
  fitting_train_factors_VZ(train_table5)
  fitting_train_factors_VZ(train_table6)
  
  
}
VZ_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6)

BIDU_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6){
  fitting_train_factors_BIDU(train_table1)
  fitting_train_factors_BIDU(train_table2)
  fitting_train_factors_BIDU(train_table3)
  fitting_train_factors_BIDU(train_table4)
  fitting_train_factors_BIDU(train_table5)
  fitting_train_factors_BIDU(train_table6)
}
BIDU_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6)





######### testest #### chapter headers
# header 2 ----
# header 3 ====
# testtest<- lm(train_Fund.xcess ~ train_rmrf + train_smb + train_hml + train_rmw + train_cma)
# header 1 ####
