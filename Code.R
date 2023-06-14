# Laden van benodigde pakketten
library(quantmod)

# Functie om excess returns te berekenen
calculate_excess_returns <- function(asset_returns, benchmark_returns) {
  excess_returns <- asset_returns - benchmark_returns
  return(excess_returns)
}

# Functie om Fama-French 5-factor model te passen
apply_fama_french_model <- function(asset_returns, factor_data) {
  model_formula <- as.formula("Excess_Returns ~ Mkt_RF + SMB + HML + RMW + CMA")
  model_data <- merge(asset_returns, factor_data)
  model_fit <- lm(model_formula, data = model_data)
  return(model_fit)
}

# Lijst met assetnamen
assets <- c("MSFT", "GOOGL", "VZ", "BIDU")

# Gegevensperiode
start_date <- "2011-05-22"
end_date <- "2022-12-30"

# Lijst om resultaten op te slaan
fama_french_results <- list()

# Loop over de assets
for (asset in assets) {
  # Haal prijsgegevens op
  getSymbols(asset, from = start_date, to = end_date)
  
  # Bereken dagelijkse rendementen
  asset_returns <- dailyReturn(Cl(get(asset)))
  
  # Haal factorgegevens op
  # Voeg hier de code toe om de factorgegevens op te halen en aan te passen aan je specifieke bron
  
  # Bereken excess returns
  excess_returns <- calculate_excess_returns(asset_returns, benchmark_returns)
  
  # Pas het Fama-French model toe
  fama_french_model <- apply_fama_french_model(excess_returns, factor_data)
  
  # Sla de resultaten op
  fama_french_results[[asset]] <- fama_french_model
}

# Bekijk de resultaten
for (asset in assets) {
  cat("Resultaten voor", asset, ":\n")
  print(fama_french_results[[asset]])
  cat("\n")
}