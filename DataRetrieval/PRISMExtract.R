setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/")
data=read.csv("pruneit.csv")
data2=read.csv("Samrawdata_final.csv")

install.packages("dplyr")
library(dplyr)
test = inner_join(data, data2)


prismfinal = data.frame(matrix(nrow=nrow(data), ncol=8))
names(prismfinal) = c("longitude", "latitude", "elevation", "year", "ppt", "tmin", "tmean", "tmax")

for (i in 1:nrow(prismdata)) {
  if (prismdata[i,]$sample.year == prismdata[i,]$year) {
    prismfinal[i,1:8] <- prismdata[i,2:9]
    print(prismdata[i,2:9])}
}
