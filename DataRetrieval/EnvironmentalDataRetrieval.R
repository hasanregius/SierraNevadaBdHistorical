#################################################
# Retrieving Environmental Data for Sierra Nevada
# Hasan Sulaeman
#################################################
 
# Library & Setup ----
## Loading Libraries
library(FedData)                                        # Package to get data from USGS national hydrography dataset
library(raster)                                         # Package to get data from worldclim
library(rgeos)                                          # Package to work with geospatial data
library(measurements)                                   # Package to convert measurements
library(dplyr)                                          # For tidying up

## Working Directory and Main Dataset 
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/")
data = read.csv("rawdata.csv", header=TRUE)

## Loading BioClim Data
bio = getData("worldclim", var="bio", res=10)           # We used the function getData to fetch "bio" variables from worldclim                   
bio = bio[[c(1:19)]]                                    # We're fetching 19 bio variables for temperature and precipitation
colnames(x)=c("longitude", "latitude")

## Loading Anthropogenic Data
HFP = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/HFP2009.tif")
crop = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Croplands2005.tif")
built = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Built2009.tif")
navwat = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Navwater2009.tif")
PopDen = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/PopDensity2010.tif")
road = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Roads.tif")
rail = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Railways.tif")
pasture = raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Pasture2009.tif")
anthro = stack(HFP, crop, built, navwat, PopDen, road, rail, pasture)

## convert latlong from decimal minutes to decimal degrees (Optional) 
data$Latitude = measurements::conv_unit(data$Latitude, from = 'deg_dec_min', to = 'dec_deg')
data$Longitude = measurements::conv_unit(data$Longitude, from = 'deg_dec_min', to = 'dec_deg')
### NOTE:
### US Federal Agencies use ESGS 4269;
### +init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0 

## Convert coordinates to spatial object
x = data.frame(x=data$decimalLongitude, y=data$decimalLatitude)   # We're converting the coordinates into a spatial information
sp = SpatialPointsDataFrame(coords = x, data = x,
     proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

## Loading NHD Data
WaterBody = nhd$`_Waterbody`
Flowline=nhd$`_Flowline`

### CRS Matching for NHD Data
nhd = get_nhd(template=sp, label="distance to waterbody", raw.dir="/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
crs(nhd$`_Waterbody`)=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
crs(nhd$`_Flowline`)=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
crs(sp) = CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")

# Extracting climate Data from WorldClim (data excluded by reviewer request) ----
WorldClim = (extract(bio,x))                               # We're saving the values
data = cbind.data.frame(data, WorldClim)
# write.csv(data, "rawdata.csv"), indented for non-use

# Extracting PRISM Data for Historical Climatic Data ----
prismfinal = data.frame(matrix(nrow=nrow(data), ncol=8))
names(prismfinal) = c("longitude", "latitude", "elevation", "year", "ppt", "tmin", "tmean", "tmax")

for (i in 1:nrow(prismdata)) {
  if (prismdata[i,]$sample.year == prismdata[i,]$year) {
    prismfinal[i,1:8] <- prismdata[i,2:9]
    print(prismdata[i,2:9])}
}

# Extracting Anthropogenic Data from Venter et al. (2016)  ----
## NOTE: "Lights" was excluded as a variable. Not biologically relevant.
StackdRaster = extract(anthro, sp)
data =  cbind.data.frame(data, StackedRaster) # Adding the retrieved data into the data frame

# Extracting NHD Data ----
## Break Down Each Water Body and Flowline Based on FCode (determined by NHD)
### Dummy Variables
NamesWB = c("playa", "icemass", "LakePond", "reservoir",
            "swamp", "estuary", "CanalDitch", "IntermittentStream",
            "PerennialStream", "EphemeralStream")

for (i in 1:length(NamesWB)) {
  i = matrix(ncol = ncol(WaterBody),
             dimnames = c(rownames(WaterBody), colnames(WaterBody)))
}

### Separating Flowline and Water Body Data
for (i in 1:nrow(WaterBody)) {
  # Water Bodies
  if (WaterBody[i,]$FCode == 36100) {
    playa = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode == 37800) {
    IceMass = rbind(WaterBody[i,])
  } 
  if (WaterBody[i,]$FCode %in% c(39000:39012)) {
    LakePond = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode %in% c(43600:43626)) {
    reservoir = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode %in% c(46600:46601)) {
    swamp = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode == 49300) {
    estuary = rbind(WaterBody[i,])
  }
  # Flowlines
  if (WaterBody[i,]$FCode  %in% c(33600:3363)) {
    CanalDitch = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode == 46003) {
    IntermittentStream = rbind(WaterBody[i,])
  }
  if (WaterBody[i,]$FCode == 46006) {
    PerennialStream = rbind(WaterBody[i,])
  }
  else {
    EphemeralStream = rbind(WaterBody[i,])
  }
}

## Calulating Distances to the Closest Water Body Based on Lat Long
### Function to Calculate Distance Based on Spatial DF and NHD Data
DistToWB = function (WB, sp) {
  DistWB = apply(gDistance(spgeom1=sp, spgeom2=WB, byid=T), 2, min)
  DistWB = DistWB*1000
}

### Distance to nearest Playa
DistPlaya = DistToWB(playa, sp)
data = cbind(data, DistPlaya)

#### Distance to nearest Ice Mass
DistIceMass = DistToWB(IceMass, sp)
data = cbind(data, DistIceMass)

# Distance to nearest Lake/Pond
DistLakePond = DistToWB(lakepond, sp)
data = cbind(data, DistLakePond)

# Distance to nearest Reservoir
DistReservoir = DistToWB(reservoir, sp)
data = cbind(data, DistReservoir)

## Distance to Swamp
DistSwamp = DistToWB(swamp, sp)
data = cbind(data, DistSwamp)

# Distance to Estuary
DistEstuary = DistToWB(estuary, sp)
data = cbind(data, DistEstuary)

# Distance to Canal/Ditch
DistCanalDitch = DistToWB(CanalDitch, sp)
data = cbind(data, DistCanalDitch)

# Distance to Intermittent Streams
DistIntermittentStream = DistToWB(IntermittentStream, sp)
data = cbind(data, DistIntermittentStream)

# Distance to Perennial Streams
DistPerennialStream = DistToWB(PerennialStream, sp)
data = cbind(data, DistPerennialStream)

# Distance to Ephemeral Streams
DistEphemeralStream = DistToWB(EphemeralStream, sp)
data = cbind(data, DistEphemeralStream)