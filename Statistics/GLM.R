############################################################
# Binary Logistic Regression for Sierra Nevada Historical Bd
############################################################

# Library and data setup ----
library(Hmisc)
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
mue = read.csv('samsara.csv')

# Finding highly correlated variables
factors = mue[,c(6,29:41)]
factor.cor = rcorr(as.matrix(factors))
## NOTE: tmean is highly correlated with tmin, tmax, and elevation
## Scaling the numeric data so that they can be more evenly compared
mue[,c(29:36,38,40:41)] = scale(mue[,c(29:36,38,40:41)])

# The tmin, tmean, and tmax are highly correlated with one another
lr = glm(BdStatus ~ distwater + factor(catwater) + Roads + ppt
     + tmean + Popdensity2010 + croplands2005 + Built2009 + Species +
     Popdensity2010 + Railways + Pasture2009 + HFP2009, data=mue)
ayo = step(lr)
winner = lm(BdStatus ~ Roads + ppt + tmean + Popdensity2010 + croplands2005 + 
            Built2009 + Species + Railways + Pasture2009 + HFP2009, data = mue)
summary(winner)
# Positive relationship = Roads, tmean, FirstPosDist (p<0.01, p<0.01, p<0.01; respectively)
# Negative relationship = HFP2009 (p<0.01)
sink(file="All_Years_GLM_Results.txt")
summary(winner) 
sink()

# Pre-1970 Data and LM
pre70=subset(mue, Year < 1970)
lr = lm(BdStatus ~ dist.closest.wb + factor(cat.closest.wb) + Roads
        + tmean + elevation + Popdensity2010 + HFP2009, data=pre70)
ayo = step(lr, direction="both")
pre70.lm = lm(BdStatus ~ factor(cat.closest.wb) + tmean + HFP2009, data=pre70)
summary(pre70.lm)
# Positive relationship = NA
# Negative relationship = Dist to intermittent stream, dist to lake/pond, dist to swamp, and dist to first pos
# p-values are p=0.01, p=0.03, p<0.01, and p<0.01 respectively
sink(file="Pre70_LM_Results.txt")
summary(pre70.lm) 
sink()

post70=subset(mue, Year >= 1970)
lr = lm(BdStatus ~ dist.closest.wb + factor(cat.closest.wb) + Roads
        + tmean + Elevation.m. + Popdensity2010 + HFP2009 + FirstPosDist, data=post70)
ayo = step(lr)
post70.lm = lm(BdStatus ~ dist.closest.wb + Roads + Elevation.m. + HFP2009 + FirstPosDist, data=post70)
summary(post70.lm)
# Positive relationship = Distance to closest water body, roads (p=0.03, p<0.01; respectively)
# Negative relationship = NA
sink(file="Post70_LM_Results.txt")
summary(post70.lm) 
sink()

# Let's try out zeroinflated model, which might be better for us
library(pscl)

# Running the same model but zeroinflated
zeroinf = zeroinfl(BdStatus ~ Roads + tmean + HFP2009 + FirstPosDist, data=mue, dist = "negbin", link= "logit")
summary(zeroinf) 

# Let's save the result
sink(file="zeroinflated_LM_Results.txt")
summary(zeroinf) 
sink()

# Can't run GLM on the Zscore with too many zeros 
zscore = subset(mue, ZEScore > 0)
hist(zscore$ZEScore)

lm = glm(ZEScore ~ bio1 + bio2 + bio3 + bio4 + bio12 + bio14 + 
           bio15 + HFP2009 + Popdensity2010 + Roads + 
           dist.closest.wb + cat.closest.wb, data=zscore, family=poisson)
lm
zs = step(lm)
