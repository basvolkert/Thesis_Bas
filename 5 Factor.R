library(zoo)
library(xts)
library(TTR)
library(quantmod)

list_assets <- c


# Getting the data of the competing companies
getSymbols('AMZN')
AMZN <- AMZN["2022-05-11/2022-12-31", ]
getSymbols('VZ')
VZ <- VZ["2022-05-11/2022-12-31", ]
getSymbols('MSFT')
MSFT <- MSFT["2022-05-11/2022-12-31", ]
getSymbols('GOOGL')
GOOGL <- GOOGL["2022-05-11/2022-12-31", ]
getSymbols('VT')
VT <- VT["2022-05-11/2022-12-31", ]



# Getting the data of the factors
# iShares Edge MSCI World Value Factor UCITS ETF USD , HML
getSymbols('IWVL.L')
IWVL.L <- IWVL.L["2022-05-11/2022-12-31", ]
# iShares Edge MSCI World Size Factor UCITS ETF USD , SMB
getSymbols('IWSZ.L')
IWSZ.L <- IWSZ.L["2022-05-11/2022-12-31", ]
# iShares Edge MSCI World Momentum Factor UCITS ETF USD , WML
getSymbols('IWMO.L')
IWMO.L <- IWMO.L["2022-05-11/2022-12-31", ]
#iShares Edge MSCI World Quality Factor UCITS ETF USD , RMW
getSymbols('IWQU.L')
IWQU.L <- IWQU.L["2022-05-11/2022-12-31", ]
# CMA factor
getSymbols('')


#Calculating the daily returns
returns_AMZN <- periodReturn(AMZN,period='daily')
returns_GOOGL <- periodReturn(GOOGL,period='daily')
returns_VZ <- periodReturn(VZ,period='daily')
returns_MSFT <- periodReturn(MSFT,period='daily')
returns_VT <- periodReturn(VT,period='daily')

#Calculating the daily excess returns
xcess_returns_AMZN <- returns_AMZN - returns_VT
xcess_returns_GOOGL <- returns_GOOGL - returns_VT
xcess_returns_VZ <- returns_VZ - returns_VT
xcess_returns_MSFT <- returns_MSFT - returns_VT



# selecting the data
start_date <- as.Date("2022/05/11") 

seq(start_date, by ="day",length.out= 5)