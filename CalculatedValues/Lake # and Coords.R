
# Assigning coordinates based on lake number

# Libraries and Dataset ----
library("rgdal")
library("sp")
library("rworldmap")
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
coords = read.csv("sekilakecoords.csv")
dat = read.csv("barrett.data.csv")
dat2 = read.delim("Vance.data.txt")

# Prepping the two datasets into one dataset ---- 
# NOTE: The column #s were different so we couldn't use rbind
data = data.frame(matrix(nrow=(nrow(dat)+nrow(dat2)), ncol=4))
colnames(data) = c("lake.id", "year", "Bd.Status", "Zscore")
data[1:nrow(dat),]$lake.id = dat$lake_id
data[1:nrow(dat),]$year = dat$year
data[1:nrow(dat),]$Zscore = dat$Zscore
data[(nrow(dat)+1):(nrow(dat2)+nrow(dat)),]$lake.id = dat2$lake_id
data[(nrow(dat)+1):(nrow(dat2)+nrow(dat)),]$year = dat2$year
data[(nrow(dat)+1):(nrow(dat2)+nrow(dat)),]$Zscore = dat2$Zscore
write.csv(data, "sixtylakedata.csv") # Got rid of the .1s next to the lake id
data = read.csv("sixtylakedata.csv")

# Assigning Bd infection values ----
for (i in 1:nrow(data)) {
  if (data[i,]$Zscore > 0) {data[i,]$Bd.Status = 1}  
  else {data[i,]$Bd.Status = 0}
}

# Converting UTM to Decimal Degrees ----
lake.coords = SpatialPoints(cbind(coords$East.UTM, coords$North.UTM), 
                            proj4string = CRS("+proj=utm  +zone=11"))
longlat = as.data.frame(spTransform(lake.coords, CRS("+proj=longlat")))

# Assigning Long Lat Values to Individual Data ----
names(coords) = c("basin.name","lake.id","longitude","latitude")
coords$longitude = longlat$coords.x1
coords$latitude = longlat$coords.x2
data$longitude = 0                          # Making a new column
data$latitude = 0                           # Making a new column

for (i in 1:nrow(coords)) {
  for (j in 1:nrow(data)) {
    if (coords[i,]$lake.id == data[j,]$lake.id) {
      data[j,]$longitude = coords[i,]$longitude
      data[j,]$latitude = coords[i,]$latitude
    }
  }
}

write.csv(data, "sekidata.csv")

# Plotting the map out ----
library(rworldmap)
sekimap = getMap(resolution = "low")
plot(sekimap, xlim = c(-120,-115), ylim = c(30, 40), asp = 1)
points(data$longitude, data$latitude, col = "red", cex = .6)
