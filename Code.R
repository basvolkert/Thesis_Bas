# Laden van benodigde pakketten
library(quantmod)

# Functie om excess returns te berekenen
calculate_excess_returns <- function(asset_returns, benchmark_returns) {
  excess_returns <- asset_returns - benchmark_returns
  return(excess_returns)
}

# Functie om Fama-French 5-factor model toe te passen
apply_fama_french_model <- function(asset_returns, factor_data) {
  model_formula <- as.formula("Excess_Returns ~ VT + SMB + HML + WML + RMW + CMA")
  model_data <- merge(asset_returns, factor_data)
  model_fit <- lm(model_formula, data = model_data)
  return(model_fit)
}

# Lijst met assetnamen en bijbehorende ETF-tickers voor de factoren
assets <- c("MSFT", "GOOGL", "VZ", "BIDU")
factor_tickers <- c("IWSZ.L", "IWVL.L", "IWMO.L", "IWQU.L")

# Gegevensperiode
start_date <- "2011-05-11"
end_date <- "2022-12-30"

# Haal prijsgegevens op van de VT (Vanguard Total World Stock ETF)
getSymbols("VT", from = start_date, to = end_date)

# Bereken dagelijkse rendementen van de VT
vt_returns <- dailyReturn(Cl(VT))

# Lijst met startdata voor T0, Tg, T1 en T2
T0_start_dates <- c("2022-05-24", "2022-05-20", "2022-05-18", "2022-05-11", "2022-05-11", "2022-05-11")
Tg_end_dates <- c("2022-11-14", "2022-11-10", "2022-11-08", "2022-11-01", "2022-11-01", "2022-11-01")
T1_start_dates <- c("2022-11-29", "2022-11-25", "2022-11-22", "2022-11-15", "2022-11-15", "2022-11-15")
T2_end_dates <- c("2022-12-01", "2022-12-05", "2022-12-07", "2022-12-14", "2022-12-21", "2022-12-29")

# Lees het CSV-bestand in
Cma_data <- read.csv("data_CMA.csv")

# Zet de kolom "Date" om naar datums
Cma_data$Date <- as.Date(as.character(Cma_data$Date), format = "%Y%m%d")

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
    
    # Subset van CMA-factorgegevens
    cma_factor <- cma_data[cma_data$Date >= T0_start & cma_data$Date <= Tg_end, "CMA"]
    
    # Maak een data.frame met alle gegevens
    model_data <- data.frame(
      Excess_Returns = calculate_excess_returns(estimation_returns, factor_returns),
      VT = coredata(vt_returns[T0_start:Tg_end]),
      SMB = coredata(dailyReturn(Cl(get("IWSZ.L")))[T0_start:Tg_end]),
      HML = coredata(dailyReturn(Cl(get("IWVL.L")))[T0_start:Tg_end]),
      WML = coredata(dailyReturn(Cl(get("IWMO.L")))[T0_start:Tg_end]),
      RMW = coredata(dailyReturn(Cl(get("IWQU.L")))[T0_start:Tg_end]),
      CMA = coredata(cma_factor)
    )
    
    # Pas Fama-French 5-factor model toe
    model_fit <- apply_fama_french_model(model_data, factor_returns)
    
    # Sla de resultaten op
    fama_french_results[[paste(asset, j, sep = "_")]] <- model_fit
  }
}

# Bekijk de opgeslagen resultaten
for (result in fama_french_results) {
  print(summary(result))
}