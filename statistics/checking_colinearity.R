######################################################
# Checking for colinearity in the predictive variables
# Hasan Sulaeman
######################################################

# Dependencies ----
#  Packages ----
library(Hmisc)

#  Datasets ----
# Analytic dataset with predictor variables linked in
mue = read.csv('finaldata.csv')
# Visitor data from US Fish and Wildlife Service for Yosemite and Sequoia
visitor = read.csv("SEKI.YOSE.visitors.csv")

# Checking the bioclimactic variables for colinearity ----
par(mfrow = c(2,2))
plot(density(mue$precip), main = "Precipitation Spread")
plot(density(mue$tmin), main = "Min Temperature Spread")
plot(density(mue$tmax), main = "Max Temperature Spread")
plot(density(mue$tmean), main = "Mean Temperature Spread")

# Checking the anthropogenic data for colinearity ----
par(mfrow = c(4,2)) 
plot(density(mue[,29]), main = "HFP Index Spread")
plot(density(mue[,30]), main = "Croplands Spread")
plot(density(mue[,31]), main = "Built Environment Spread")
plot(density(mue[,32]), main = "Navigable Waters Spread")
plot(density(mue[,33]), main = "Pop. Density Spread")
plot(density(mue[,34]), main = "Road Spread")
plot(density(mue[,35]), main = "Railways Spread")
plot(density(mue[,36]), main = "Pasture Spread")

# Perform the formal correlation test ----
ctest = data[,18:19]
factor.cor = rcorr(as.matrix(ctest))
factor.cor$r

# Exporting the formal correlations for review ----
write.csv(factor.cor$r, "correlations.csv")
