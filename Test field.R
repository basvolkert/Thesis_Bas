# Load required libraries
library(quantmod)

# Set the start and end dates
start_date <- as.Date("2022-05-11")
end_date <- as.Date("2022-12-29")

# Define the ticker symbol
etf_symbol <- "IWSZ.L"

# Retrieve the historical data for the ETF
etf_data <- getSymbols(etf_symbol, from = start_date, to = end_date, auto.assign = FALSE)

# Extract the adjusted close prices from the data
etf_prices <- Ad(etf_data)

# Calculate the returns for the ETF
etf_returns <- dailyReturn(etf_prices)

# Calculate the SMB factor
smb <- etf_returns

# Print the SMB factor values
print(smb)
