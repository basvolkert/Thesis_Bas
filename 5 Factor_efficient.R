library(zoo)
library(xts)
library(TTR)
library(quantmod)

# Lijst met assetnamen en bijbehorende benchmarknaam
assets <- c("MSFT", "GOOGL", "VZ", "BIDU")
benchmark <- "VT"

# Een lege lijst om de berekende excess returns op te slaan
excess_returns_list <- list()

# Loop over de assets
for (i in 1:length(assets)) {
  # Gebruik quantmod om de prijsgegevens van de asset en benchmark op te halen
  getSymbols(assets[i], from = "2011-05-22", to = "2022-12-30")
  getSymbols(benchmark, from = "2011-05-22", to = "2022-12-30")
  
  # Bereken de dagelijkse rendementen van de asset en benchmark
  returns_asset <- dailyReturn(Cl(eval(parse(text = assets[i]))))
  returns_benchmark <- dailyReturn(Cl(eval(parse(text = benchmark))))
  
  # Bereken de excess returns door de rendementen van de benchmark af te trekken van de rendementen van de asset
  excess_returns <- returns_asset - returns_benchmark
  
  # Voeg de berekende excess returns toe aan de lijst
  excess_returns_list[[i]] <- excess_returns
}

# Bekijk de resultaten
for (i in 1:length(assets)) {
  cat("Excess returns voor", assets[i], "ten opzichte van", benchmark, ":\n")
  print(excess_returns_list[[i]])
  cat("\n")
}