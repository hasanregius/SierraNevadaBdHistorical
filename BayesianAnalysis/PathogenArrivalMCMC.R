###################################################
# Bayesian MCMC for Bd Arrival in the Sierra Nevada
###################################################

# Setup & Libraries ----
## Libraries
library(runjags)
library(rjags)
library(reshape2)

## Working Directory
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/")

## Reading Data File
MuseumEverything = read.csv("/Users/hasansulaeman/Dropbox/
                            Sam's Sierra Bd work/Data/
                            Samrawdata_final.csv", header=TRUE)

## Data Preparation & Cleanup
mu.yr = dcast(data = MuseumEverything, Year~`BdStatus`,
              value.var='Decade',fun.aggregate=length)
names(mu.yr)[c(2,3)] = c('neg','pos')
mu.yr$tot = mu.yr$neg+mu.yr$pos
mu.yr$yrc = mu.yr$Year-1900

## Establish Variables
inf = mu.yr$pos
n = mu.yr$tot
time = mu.yr$yrc
nrows = nrow(mu.yr)

# Bayesian Analysis ----
## Create Model
ModelString = 'model{ 
                  for(ii in 1:nrows){ 
                    inf[ii]~dbin(p[ii], n[ii]) 
                    p[ii]<-ppres[ii]*mu 
                    ppres[ii]<-step(tst[ii]) 
                    tst[ii]<-time[ii]-arr.time} 
                  arr.time~dunif(-48,60) mu~dbeta(1,1)}'

## Run the model
ArrivalTime = jags.model(textConnection(ModelString), 
                      data = list('inf'=inf, 'n'=n, 
                                  'time'=time, 'nrows'=nrows), 
                      n.chains = 3, n.adapt = 10000)

update(ArrivalTime,10000)
ArrivalTimeSamples = coda.samples(ArrivalTime, c('arr.time','mu'),
                                  n.iter=20000)

## Plot and Summarize
plot(ArrivalTimeSamples)
summary(ArrivalTimeSamples)
