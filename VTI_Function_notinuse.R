fitting_train_factors_VTI_5F <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF + SMB + HML + RMW + CMA, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VTI
  ap <-cbind(a, p)
  
  return(ap)
} 
fitting_train_factors_VTI_4F <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF + SMB + HML + RMW, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VTI
  ap <-cbind(a, p)
  
  return(ap)
} 
fitting_train_factors_VTI_3F <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF + SMB + HML, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VTI
  ap <-cbind(a, p)
  
  return(ap)
} 
fitting_train_factors_VTI_F <- function(train_table, test_table){
  train_regression <- lm(VTI - RF ~ Mkt.RF, train_table)
  table_name <- deparse(substitute(train_table))
  column_name <-names(train_table)[12]
  p <- predict(train_regression, test_table)
  sum_p <- sum(p)
  a <- test_table$VTI
  ap <-cbind(a, p)
  
  return(ap)
} 

VTI_data_5F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VTI_5F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR^2/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = TRUE)
  
  R2 <- data.frame(fitting_train_factors_VTI_5F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR^2/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = TRUE)
  
  R3 <- data.frame(fitting_train_factors_VTI_5F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR^2/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = TRUE)
  
  R4 <- data.frame(fitting_train_factors_VTI_5F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR^2/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = TRUE)
  
  R5 <- data.frame(fitting_train_factors_VTI_5F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR^2/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = TRUE)
  
  R6 <- data.frame(fitting_train_factors_VTI_5F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR^2/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = TRUE)
  
  print("These are the values for 5 Factor model")
  
  print(paste("VTI:p-value 1", pt1))
  print(paste("VTI:p-value 2", pt2))
  print(paste("VTI:p-value 3", pt3))
  print(paste("VTI:p-value 4", pt4))
  print(paste("VTI:p-value 5", pt5))
  print(paste("VTI:p-value 6", pt6))
}
VTI_data_4F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VTI_4F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR^2/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = TRUE)
  
  R2 <- data.frame(fitting_train_factors_VTI_4F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR^2/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = TRUE)
  
  R3 <- data.frame(fitting_train_factors_VTI_4F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR^2/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = TRUE)
  
  R4 <- data.frame(fitting_train_factors_VTI_4F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR^2/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = TRUE)
  
  R5 <- data.frame(fitting_train_factors_VTI_4F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR^2/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = TRUE)
  
  R6 <- data.frame(fitting_train_factors_VTI_4F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR^2/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = TRUE)
  
  print("These are the values for 4 Factor model")
  print(paste("VTI:p-value 1", pt1))
  print(paste("VTI:p-value 2", pt2))
  print(paste("VTI:p-value 3", pt3))
  print(paste("VTI:p-value 4", pt4))
  print(paste("VTI:p-value 5", pt5))
  print(paste("VTI:p-value 6", pt6))
}
VTI_data_3F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VTI_3F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR^2/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = TRUE)
  
  R2 <- data.frame(fitting_train_factors_VTI_3F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR^2/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = TRUE)
  
  R3 <- data.frame(fitting_train_factors_VTI_3F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR^2/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = TRUE)
  
  R4 <- data.frame(fitting_train_factors_VTI_3F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR^2/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = TRUE)
  
  R5 <- data.frame(fitting_train_factors_VTI_3F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR^2/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = TRUE)
  
  R6 <- data.frame(fitting_train_factors_VTI_3F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR^2/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = TRUE)
  
  print("These are the values for 3 Factor model")
  print(paste("VTI:p-value 1", pt1))
  print(paste("VTI:p-value 2", pt2))
  print(paste("VTI:p-value 3", pt3))
  print(paste("VTI:p-value 4", pt4))
  print(paste("VTI:p-value 5", pt5))
  print(paste("VTI:p-value 6", pt6))
}
VTI_data_F <-function(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6){
  
  R1 <- data.frame(fitting_train_factors_VTI_F(train_table1, test_table1))
  R1$a_p <- (R1$a- R1$p)
  var_ar <- var(R1$a_p)
  CAR <- sum(R1$a_p)
  t1 <- CAR^2/sqrt(var_ar*3)
  pt1 <- pt(t1,2, lower.tail = TRUE)
  
  R2 <- data.frame(fitting_train_factors_VTI_F(train_table2, test_table2))
  R2$a_p <- (R2$a- R2$p)
  var_ar <- var(R2$a_p)
  CAR <- sum(R2$a_p)
  t2 <- CAR^2/sqrt(var_ar*7)
  pt2 <- pt(t2,6, lower.tail = TRUE)
  
  R3 <- data.frame(fitting_train_factors_VTI_F(train_table3, test_table3))
  R3$a_p <- (R3$a- R3$p)
  var_ar <- var(R3$a_p)
  CAR <- sum(R3$a_p)
  t3 <- CAR^2/sqrt(var_ar*11)
  pt3 <- pt(t3,10, lower.tail = TRUE)
  
  R4 <- data.frame(fitting_train_factors_VTI_F(train_table4, test_table4))
  R4$a_p <- (R4$a- R4$p)
  var_ar <- var(R4$a_p)
  CAR <- sum(R4$a_p)
  t4 <- CAR^2/sqrt(var_ar*21)
  pt4 <- pt(t4,20, lower.tail = TRUE)
  
  R5 <- data.frame(fitting_train_factors_VTI_F(train_table5, test_table5))
  R5$a_p <- (R5$a- R5$p)
  var_ar <- var(R5$a_p)
  CAR <- sum(R5$a_p)
  t5 <- CAR^2/sqrt(var_ar*26)
  pt5 <- pt(t5,25, lower.tail = TRUE)
  
  R6 <- data.frame(fitting_train_factors_VTI_F(train_table6, test_table6))
  R6$a_p <- (R6$a- R6$p)
  var_ar <- var(R6$a_p)
  CAR <- sum(R6$a_p)
  t6 <- CAR^2/sqrt(var_ar*31)
  pt6 <- pt(t6,30, lower.tail = TRUE)
  
  print("These are the values for Factor model")
  print(paste("VTI:t-value 1", pt1))
  print(paste("VTI:t-value 2", pt2))
  print(paste("VTI:t-value 3", pt3))
  print(paste("VTI:t-value 4", pt4))
  print(paste("VTI:t-value 5", pt5))
  print(paste("VTI:t-value 6", pt6))
}

VTI_div_5f <- VTI_data_5F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VTI_div_4f <- VTI_data_4F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VTI_div_3f <- VTI_data_3F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
VTI_div_F <- VTI_data_F(train_table1,train_table2,train_table3,train_table4,train_table5,train_table6,test_table1,test_table2,test_table3,test_table4,test_table5,test_table6)
s