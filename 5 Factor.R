
# Getting the data of the competing companies
getSymbols('AMZN')
getSymbols('VZ')
getSymbols('MSFT')
getSymbols('GOOGL')

# Getting the data of the factors
# iShares Edge MSCI World Value Factor UCITS ETF USD , HML
getSymbols('IWVL.L')
# iShares Edge MSCI World Size Factor UCITS ETF USD , SMB
getSymbols('IWSZ.L')
# iShares Edge MSCI World Momentum Factor UCITS ETF USD , WML
getSymbols('IWMO.L')
#iShares Edge MSCI World Quality Factor UCITS ETF USD , RMW
getSymbols('IWQU.L')
# CMA factor
getSymbols('')


# selecting the data
start_date <- as.Date("2022/05/11") 

seq(start_date, by ="day",length.out= 5)