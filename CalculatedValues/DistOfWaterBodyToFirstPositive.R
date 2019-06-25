# Calculating Distances to First Positive 

# We'll be using the package sp, which is standard for R spatial analyses
library(sp)

# Setting our working directory and fetching our data
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
data <- read.csv('finaldata.csv')
data = data[675:nrow(data),]              # First positive definition changed to 1972, row 713

# Let's first find the first positive and define it
positives=subset(data, BdStatus == 1)      # Subsetting the first positives
b = 1                                      # Looks like our first positive is 1907, row 1
firstpos = positives[b,]

# Convert the data frame into a spatial point data frame for all of our data
coordinates(data) <- ~Longitude+Latitude                    # Use whatever you name your columns for lat and long
proj4string(data) <- CRS("+proj=longlat +datum=WGS84")      # Setting the projection and datum
coordinates(firstpos) <- ~Longitude+Latitude                # Do it for your first positive too
proj4string(firstpos) <- CRS("+proj=longlat +datum=WGS84")   

# Calculating Distances
mue <- read.csv('finaldata.csv')
data = data[675:nrow(data),]              # First positive definition changed to 1972, row 713
mue = subset(data, BdStatus == 1)
mue$FirstPosDist = "tb filled"
for (i in 1:nrow(mue)) {
  mue[i,]$FirstPosDist = spDistsN1(pts = data[i,], pt = firstpos, longlat = TRUE)
}

write.csv(mue, "finaldata.csv")
mue = read.csv("dist.csv")
p = ggplot(data=mue, aes(Year, FirstPosDist)) 
p1 = p + geom_point(aes(colour = factor(SpeciesName)), data=mue)
p2 = p1 + geom_smooth(method = "lm")      # for the lm trendline
p3 = p2 + labs(y="Distance to First Positive (km)", colour = "Species")
p4 = p3 + scale_x_continuous(breaks=c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010))
p5 = p4 + scale_y_continuous(breaks=c(0,100,200,300,400,500)) 
p6 = p5 + geom_label(aes(x=1975, y=165), label="R-squared: 0.039 \n p-value: 0.014", colour = "black", fontface = "bold")
p6

p2 = p1 + geom_smooth(aes(group = 1))

# Getting the R2 value
r2 = lm(FirstPosDist ~ Year, data=mue)
summary(r2) #R2 = 0.03861
