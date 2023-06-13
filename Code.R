

ff_data <- read.table("ff_data.csv",header=TRUE,sep=",")

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
