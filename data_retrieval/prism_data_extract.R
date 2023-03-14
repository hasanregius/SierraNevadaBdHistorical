################################################
# Extracting PRISM data from Oregon University's 
# historical weather data repository
# Hasan Sulaeman
################################################

# Dependencies ----
#  Packages ----
require(dplyr)

#  Datasets ----
# PRISM data exported as a flat file
data = read.csv("pruneit.csv")
# Study dataset
data2 = read.csv("Samrawdata_final.csv")

# Joining the study and PRISM datasets ----
# Not all records can be associated with PRISM's data so we want an inner join
test = inner_join(data, data2)
prismfinal = data.frame(matrix(nrow=nrow(data), ncol=8))
names(prismfinal) = c("longitude", "latitude", "elevation", "year", "ppt", "tmin", "tmean", "tmax")

# Standardize the entries based on year of observation
for (i in 1:nrow(prismdata)) {
  if (prismdata[i,]$sample.year == prismdata[i,]$year) {
    prismfinal[i,1:8] = prismdata[i,2:9]
    print(prismdata[i,2:9])}
}
