library('xts')

returns <- read.csv('Fama-French-Three-Factors-Model-Data.txt',header=T)
returns <- xts(returns)
