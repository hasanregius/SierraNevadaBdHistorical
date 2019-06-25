setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
mue <- read.csv('finaldata.csv')

# Let's do a bit of data exploration of the environmental variables
par(mfrow=c(2,2))
plot(density(mue$precip), main = "Precipitation Spread")
plot(density(mue$tmin), main = "Min Temperature Spread")
plot(density(mue$tmax), main = "Max Temperature Spread")
plot(density(mue$tmean), main = "Mean Temperature Spread")

# Let's do more exploration of human footprint data
par(mfrow=c(4,2)) #29 to 36
plot(density(mue[,29]), main = "HFP Index Spread")
plot(density(mue[,30]), main = "Croplands Spread")
plot(density(mue[,31]), main = "Built Environment Spread")
plot(density(mue[,32]), main = "Navigable Waters Spread")
plot(density(mue[,33]), main = "Pop. Density Spread")
plot(density(mue[,34]), main = "Road Spread")
plot(density(mue[,35]), main = "Railways Spread")
plot(density(mue[,36]), main = "Pasture Spread")
# I argue only HFP Index, Roads, and pop density should be used
library(Hmisc)
ctest = data[,18:19]
factor.cor = rcorr(as.matrix(ctest))
factor.cor$r
# T mean can be used in place of both tmin and tmax

write.csv(factor.cor$r, "correlations.csv")

visitor = read.csv("SEKI.YOSE.visitors.csv")
