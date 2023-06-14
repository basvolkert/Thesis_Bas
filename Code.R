
# loading the data
ff_data <- read.table("data/ff_data.csv",header=TRUE,sep=",")

rmrf <- ff_data[,2]
smb <- ff_data[,3]
hml <- ff_data[,4]
rmw <- ff_data[,5]
cma <- ff_data[,6]
rf <- ff_data[,7]
fund <- ff_data[,8]

fund.xcess <- fund - rf

ffregression <- lm(find.xcess ~ rmrf + smb + hml + rmw + cma)

print(summary(ffregression))

?quantmod()
install.packages("zoo")
install.packages("xts")
install.packages("quantmod")

getSymbols('AAPL')
library(quantmod)
library(zoo)
library(xts)
head(AAPL)
