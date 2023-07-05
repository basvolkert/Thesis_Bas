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
  #print(paste("train data of ",column_name, "of table", table_name))
  #print(summary(train_regression))
  p <- predict(train_regression, test_table)
  #print(summary(p))
  sum_p <- sum(p)
  #a <- sum(test_table$MSFT)
  #c <- b - sum_p
  
  a <- test_table$MSFT
  #print(p)
  #print(a)
  ap <-cbind(a, p)
  #print(ap)
  #ap$a_p <- ap$a- ap$p

  
  
  #print(test_table$MSFT)
  #print(paste("total predicted return of MSFT:", sum_a))
  #print(paste("total actual return of MSFT:", b))
  #print(paste("Actual - predicted return of MSFT:", c))
  #print(" ")
  return(ap)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_GOOGL <- function(train_table, test_table){
  train_regression <- lm(GOOGL - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[9]
  #print(paste("train data of ",column_name, "of table", table_name))
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$GOOGL
  ap <-cbind(a, p)
  
  return(ap)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_VZ <- function(train_table, test_table){
  train_regression <- lm(VZ - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[10]
  #print(paste("train data of ",column_name, "of table", table_name))
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VZ
  ap <-cbind(a, p)
  
  return(ap)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_BIDU <- function(train_table, test_table){
  train_regression <- lm(BIDU - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[11]
  #print(paste("train data of ",column_name, "of table", table_name))
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$BIDU
  ap <-cbind(a, p)
  
  return(ap)
} 

# Function in order to assign the right factors for the Train data set
fitting_train_factors_VTI <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  #print(paste("train data of ",column_name, "of table", table_name))
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VTI
  ap <-cbind(a, p)
  
  return(ap)
} 



MSFT_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_MSFT(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)^2
  sum_a_p <-sum(R1$a_p)
  R1 <- sqrt(sum_a_p/3)
  
  R2 <- data.frame(fitting_train_factors_MSFT(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)^2
  sum_a_p <-sum(R2$a_p)
  R2 <- sqrt(sum_a_p/7)
  
  R3 <- data.frame(fitting_train_factors_MSFT(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)^2
  sum_a_p <-sum(R3$a_p)
  R3 <- sqrt(sum_a_p/11)
  
  R4 <- data.frame(fitting_train_factors_MSFT(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)^2
  sum_a_p <-sum(R4$a_p)
  R4 <- sqrt(sum_a_p/21)
  
  R5 <- data.frame(fitting_train_factors_MSFT(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)^2
  sum_a_p <-sum(R5$a_p)
  R5 <- sqrt(sum_a_p/26)
  
  R6 <- data.frame(fitting_train_factors_MSFT(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)^2
  sum_a_p <-sum(R6$a_p)
  R6 <- sqrt(sum_a_p/31)
  
  print(paste("MSFT:R1", R1))
  print(paste("MSFT:R2", R2))
  print(paste("MSFT:R3", R3))
  print(paste("MSFT:R4", R4))
  print(paste("MSFT:R5", R5))
  print(paste("MSFT:R6", R6))
  }
MSFT_div <- MSFT_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

GOOGL_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_GOOGL(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)^2
  sum_a_p <-sum(R1$a_p)
  R1 <- sqrt(sum_a_p/3)
  
  R2 <- data.frame(fitting_train_factors_GOOGL(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)^2
  sum_a_p <-sum(R2$a_p)
  R2 <- sqrt(sum_a_p/7)
  
  R3 <- data.frame(fitting_train_factors_GOOGL(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)^2
  sum_a_p <-sum(R3$a_p)
  R3 <- sqrt(sum_a_p/11)
  
  R4 <- data.frame(fitting_train_factors_GOOGL(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)^2
  sum_a_p <-sum(R4$a_p)
  R4 <- sqrt(sum_a_p/21)
  
  R5 <- data.frame(fitting_train_factors_GOOGL(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)^2
  sum_a_p <-sum(R5$a_p)
  R5 <- sqrt(sum_a_p/26)
  
  R6 <- data.frame(fitting_train_factors_GOOGL(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)^2
  sum_a_p <-sum(R6$a_p)
  R6 <- sqrt(sum_a_p/31)
  
  print(paste("GOOGL:R1", R1))
  print(paste("GOOGL:R2", R2))
  print(paste("GOOGL:R3", R3))
  print(paste("GOOGL:R4", R4))
  print(paste("GOOGL:R5", R5))
  print(paste("GOOGL:R6", R6))
}
GOOGL_div <- GOOGL_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

VZ_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VZ(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)^2
  sum_a_p <-sum(R1$a_p)
  R1 <- sqrt(sum_a_p/3)
  
  R2 <- data.frame(fitting_train_factors_VZ(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)^2
  sum_a_p <-sum(R2$a_p)
  R2 <- sqrt(sum_a_p/7)
  
  R3 <- data.frame(fitting_train_factors_VZ(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)^2
  sum_a_p <-sum(R3$a_p)
  R3 <- sqrt(sum_a_p/11)
  
  R4 <- data.frame(fitting_train_factors_VZ(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)^2
  sum_a_p <-sum(R4$a_p)
  R4 <- sqrt(sum_a_p/21)
  
  R5 <- data.frame(fitting_train_factors_VZ(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)^2
  sum_a_p <-sum(R5$a_p)
  R5 <- sqrt(sum_a_p/26)
  
  R6 <- data.frame(fitting_train_factors_VZ(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)^2
  sum_a_p <-sum(R6$a_p)
  R6 <- sqrt(sum_a_p/31)
  
  print(paste("VZ:R1", R1))
  print(paste("VZ:R2", R2))
  print(paste("VZ:R3", R3))
  print(paste("VZ:R4", R4))
  print(paste("VZ:R5", R5))
  print(paste("VZ:R6", R6))
}
VZ_div <- VZ_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

BIDU_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_BIDU(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)^2
  sum_a_p <-sum(R1$a_p)
  R1 <- sqrt(sum_a_p/3)
  
  R2 <- data.frame(fitting_train_factors_BIDU(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)^2
  sum_a_p <-sum(R2$a_p)
  R2 <- sqrt(sum_a_p/7)
  
  R3 <- data.frame(fitting_train_factors_BIDU(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)^2
  sum_a_p <-sum(R3$a_p)
  R3 <- sqrt(sum_a_p/11)
  
  R4 <- data.frame(fitting_train_factors_BIDU(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)^2
  sum_a_p <-sum(R4$a_p)
  R4 <- sqrt(sum_a_p/21)
  
  R5 <- data.frame(fitting_train_factors_BIDU(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)^2
  sum_a_p <-sum(R5$a_p)
  R5 <- sqrt(sum_a_p/26)
  
  R6 <- data.frame(fitting_train_factors_BIDU(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)^2
  sum_a_p <-sum(R6$a_p)
  R6 <- sqrt(sum_a_p/31)
  
  print(paste("BIDU:R1", R1))
  print(paste("BIDU:R2", R2))
  print(paste("BIDU:R3", R3))
  print(paste("BIDU:R4", R4))
  print(paste("BIDU:R5", R5))
  print(paste("BIDU:R6", R6))
}
BIDU_div <- BIDU_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)

VTI_data <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VTI(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)^2
  sum_a_p <-sum(R1$a_p)
  R1 <- sqrt(sum_a_p/3)
  
  R2 <- data.frame(fitting_train_factors_VTI(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)^2
  sum_a_p <-sum(R2$a_p)
  R2 <- sqrt(sum_a_p/7)
  
  R3 <- data.frame(fitting_train_factors_VTI(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)^2
  sum_a_p <-sum(R3$a_p)
  R3 <- sqrt(sum_a_p/11)
  
  R4 <- data.frame(fitting_train_factors_VTI(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)^2
  sum_a_p <-sum(R4$a_p)
  R4 <- sqrt(sum_a_p/21)
  
  R5 <- data.frame(fitting_train_factors_VTI(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)^2
  sum_a_p <-sum(R5$a_p)
  R5 <- sqrt(sum_a_p/26)
  
  R6 <- data.frame(fitting_train_factors_VTI(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)^2
  sum_a_p <-sum(R6$a_p)
  R6 <- sqrt(sum_a_p/31)
  
  print(paste("VTI:R1", R1))
  print(paste("VTI:R2", R2))
  print(paste("VTI:R3", R3))
  print(paste("VTI:R4", R4))
  print(paste("VTI:R5", R5))
  print(paste("VTI:R6", R6))
}
VTI_div <- VTI_data(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)















#5 Test field ----
R1 <- data.frame(fitting_train_factors_MSFT(train_table1, test_table1))
R1$a_p <- (R1$a- R1$p)
R1$a_p2 <- (R1$a- R1$p)^2
sum_a_p2 <-sum(R1$a_p2)
R1X <- sqrt(sum_a_p2/3)

R1
R1X

#R6 <- data.frame(fitting_train_factors_MSFT(train_table6, test_table6))
#R6$a_p <- (R6$a- R6$p)^2
#sum_a_p <-sum(R6$a_p)
#test <- sqrt(sum_a_p/31)