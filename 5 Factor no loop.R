# load CSV file in to R
ff_data <- read.table("ff_data.csv",header=TRUE, sep=",")

# Spliting the data in Train an Test
estimation_window <- 120
gap <- 10
train_data <- ff_data[1:estimation_window, ]
test_data <- ff_data[(estimation_window + gap +1):nrow(ff_data), ]

# Extracting Fama-French Factors and Returns for the CSV file
train_rmrf <- train_data[,2]
train_smb <- train_data[,3]
train_hml <- train_data[,4]
train_rmw <- train_data[,5]
train_cma <- train_data[,6]
train_rf <- train_data[,7]
train_fund <- train_data[,8]

# Extracting Fama-French Factors and Returns for the CSV file
test_rmrf <- test_data[,2]
test_smb <- test_data[,3]
test_hml <- test_data[,4]
test_rmw <- test_data[,5]
test_cma <- test_data[,6]
test_rf <- test_data[,7]
test_fund <- test_data[,8]

#Calculate Excess Returns of Target fund
train_Fund.xcess <- train_fund - train_rf

#Calculate Excess Returns of Target fund
test_Fund.xcess <- test_fund - test_rf

# Run Fama-French Regression

train_regression <- lm(train_Fund.xcess ~ train_rmrf + train_smb + train_hml + train_rmw + train_cma)

# Run Fama-French Regression
test_regression <- lm(test_Fund.xcess ~ test_rmrf + test_smb + test_hml + test_rmw + test_cma)

# Print summary of the regression results
print(summary(train_regression))

# Print summary of the regression results
print(summary(test_regression))