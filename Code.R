# Install and load the required packages
install.packages("quantmod")
library(quantmod)

# Define the list of assets
assets <- c("AAPL", "GOOGL", "MSFT", "AMZN")  # Add more tickers if needed

# Set the date range
start_date <- "2022-05-11"
end_date <- "2022-12-29"

# Create an empty data frame to store the daily returns
returns_table <- data.frame(Date = character(), stringsAsFactors = FALSE)

# Loop through each asset
for (asset in assets) {
  # Get the stock data
  getSymbols(asset, from = start_date, to = end_date)
  
  # Calculate the daily returns
  asset_returns <- dailyReturn(Cl(get(asset)), type = "log")
  
  # Add the returns to the returns_table
  returns_table <- merge(returns_table, asset_returns, by = "Date", all = TRUE)
}

# Rename the columns with asset tickers
colnames(returns_table)[-1] <- assets

# Print the returns table
print(returns_table)