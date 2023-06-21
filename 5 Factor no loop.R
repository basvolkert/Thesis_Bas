library(quantmod)

# Definieer de assets en factor tickers
asset <- c("MSFT")
factor_tickers <- c("IWSZ.L", "IWVL.L", "IWMO.L", "IWQU.L", "VT")

# Definieer de start- en einddatums als een `Date`-object
start_date <- as.Date("2022-05-11")
end_date <- as.Date("2022-12-30")

# Functie om dagelijkse rendementen te berekenen en naam te geven
calculate_daily_returns <- function(symbol, start_date, end_date) {
  # Haal de historische prijsgegevens op
  data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  
  # Bereken dagelijkse rendementen
  returns <- dailyReturn(data, type = "log")
  
  # Geef het resultaat een naam met de ticker
  colnames(returns) <- paste0("daily_return_", symbol)
  
  # Geef het resultaat terug
  return(returns)
}

# Bereken dagelijkse rendementen voor de asset
asset_returns <- calculate_daily_returns(asset, start_date, end_date)

# Bereken dagelijkse rendementen voor de factor tickers
factor_returns <- lapply(factor_tickers, function(ticker) {
  calculate_daily_returns(ticker, start_date, end_date)
})

# Combineer de dagelijkse rendementen in één dataset
returns_data <- cbind(asset_returns, do.call(merge, factor_returns))

# Bekijk de resulterende dataset
print(returns_data)