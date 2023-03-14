###################################################
# Bayesian MCMC for Bd Arrival in the Sierra Nevada
# Hasan Sulaeman
###################################################

# Setup & Libraries ----
## Libraries
library(runjags)
library(rjags)
library(reshape2)

# Dataset ----
filedir = "/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/Samrawdata_final.csv"
MuseumEverything = read.csv(filedir, header = TRUE)

# Data preparation & cleanup
mu.yr = dcast(data = MuseumEverything, Year ~ `BdStatus`, value.var = 'Decade', fun.aggregate = length)
names(mu.yr)[c(2,3)] = c('neg','pos')
mu.yr$tot = mu.yr$neg + mu.yr$pos
# 1900 is the first year a sample was taken in the study
mu.yr$yrc = mu.yr$Year - 1900 

# Establish the variables
inf = mu.yr$pos
n = mu.yr$tot
time = mu.yr$yrc
nrows = nrow(mu.yr)

# Bayesian analysis ----
# String to specify the modeling approach
model_string = 'model{
  for(ii in 1:nrows){
    inf[ii] ~ dbin(p[ii], n[ii])
    p[ii] = ppres[ii]*mu
    ppres[ii] = step(tst[ii])
    tst[ii] = time[ii] - arr.time
  }
  arr.time ~ dunif(-48,60) mu~dbeta(1,1)
}'

# Run the model using the string and jags
arrival_time = jags.model(textConnection(model_string), 
                          data = list('inf'= inf, 'n'= n, 'time' = time, 'nrows' = nrows), 
                          n.chains = 3, n.adapt = 10000)
update(ArrivalTime, 10000) # This step and the extra number of iterations is optional
ArrivalTimeSamples = coda.samples(ArrivalTime, c('arr.time','mu'), n.iter = 20000)

# Plot the results
plot(ArrivalTimeSamples)
# summary(ArrivalTimeSamples)
