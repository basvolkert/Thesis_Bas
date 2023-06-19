library(quantmod)

# Functie om excess returns te berekenen
calculate_excess_returns <- function(asset_returns, benchmark_returns) {
  excess_returns <- asset_returns - benchmark_returns
  return(excess_returns)
}

# Functie om Fama-French 5-factor model toe te passen
apply_fama_french_model <- function(asset_returns, factor_data) {
  model_formula <- as.formula("Excess_Returns ~ Mkt_RF + SMB + HML + RMW + CMA")
  model_data <- merge(asset_returns, factor_data)
  model_fit <- lm(model_formula, data = model_data)
  return(model_fit)
}

# Lijst met assetnamen en bijbehorende ETF-tickers voor de factoren
assets <- c("MSFT", "GOOGL", "VZ", "BIDU")
factor_tickers <- c("IWVL.L", "IWSZ.L", "IWMO.L", "IWQU.L")

# Gegevensperiode
start_date <- "2011-05-22"
end_date <- "2022-12-30"

# Lijst met startdata voor T0, Tg, T1 en T2
T0_start_dates <- c("2022-05-24", "2022-05-20", "2022-05-18", "2022-05-11", "2022-05-11", "2022-05-11")
Tg_end_dates <- c("2022-11-14", "2022-11-10", "2022-11-08", "2022-11-01", "2022-11-01", "2022-11-01")
T1_start_dates <- c("2022-11-29", "2022-11-25", "2022-11-22", "2022-11-15", "2022-11-15", "2022-11-15")
T2_end_dates <- c("2022-12-01", "2022-12-05", "2022-12-07", "2022-12-14", "2022-12-21", "2022-12-29")

# Lijst om resultaten op te slaan
fama_french_results <- list()

# Loop over de assets
for (i in 1:length(assets)) {
  asset <- assets[i]
  factor_ticker <- factor_tickers[i]
  
  # Haal prijsgegevens op van de asset
  getSymbols(asset, from = start_date, to = end_date)
  
  # Bereken dagelijkse rendementen van de asset
  asset_returns <- dailyReturn(Cl(get(asset)))
  
  # Haal prijsgegevens op van de factor ETF
  getSymbols(factor_ticker, from = start_date, to = end_date)
  
  # Bereken dagelijkse rendementen van de factor ETF
  factor_returns <- dailyReturn(Cl(get(factor_ticker)))
  
  # Loop over de verschillende startdata voor T0
  for (j in 1:length(T0_start_dates)) {
    T0_start <- T0_start_dates[j]
    Tg_end <- Tg_end_dates[j]
    T1_start <- T1_start_dates[j]
    T2_end <- T2_end_dates[j]
    
    # Subset van rendementen voor estimation window
    estimation_returns <- asset_returns[T0_start:Tg_end]
    
    # Subset van rendementen voor event window
    event_returns <- asset_returns[T1_start:T2_end]
    
    # Bereken excess returns
    excess_returns <- calculate_excess_returns(estimation_returns, factor_returns)
    
    # Pas Fama-French 5-factor model toe
    model_fit <- apply_fama_french_model(excess_returns, factor_returns)
    
    # Sla de resultaten op
    fama_french_results[[paste(asset, j, sep = "_")]] <- model_fit
  }
}