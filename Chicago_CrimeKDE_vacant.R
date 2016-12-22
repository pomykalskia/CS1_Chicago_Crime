# SYS 6018 - Case Study 1 

setwd('C:/Users/Andrew Pomykalski/Dropbox/CaseStudy1')

library(MASS)
library(RColorBrewer)
library(maptools)
library(stringr)
library(reshape2)
library(lubridate)
library(rgdal)

# Set plot colors
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)

# Import city boundary data
city.boundary = readShapePoly("City_Boundary/geo_export_8e9e40f9-be43-489a-a8a7-64eb8423712c")

# Import and clean vacant lot data
vacant_full <- read.csv("311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported.csv")
vacant <- na.omit(vacant_full) # Remove observations with missing data - 265 observations

# Visualize vacant buildings data
plot(vacant$LONGITUDE, vacant$LATITUDE) # there is a funky data point here, omit below
vacant <- vacant[vacant$LATITUDE>=40.5,] # omit outlier - bad data

# Split date column to limit by year
vacant <- data.frame(vacant, colsplit(vacant$DATE.SERVICE.REQUEST.WAS.RECEIVED, pattern = "/", names=c("month","date","year")))
vacant <- vacant[vacant$year==2015,] # 5288 observations

#### PROJECTIONS
## Project vacant plot data to meters
coord <- data.frame('long'=vacant$LONGITUDE, 'lat'=vacant$LATITUDE)
coordinates(coord) <- ~ long+lat
# set projection to 3435 - measures distance in feet.
proj4string(coord) <- CRS("+init=epsg:3435")
# reproject to Coordinate Reference System (CRS) 26971 - measures distance in meters.
xycoord = data.frame(spTransform(coord, CRS("+init=epsg:26971")))
colnames(xycoord) <- c("x_meter","y_meter")
meters <- cbind(vacant, xycoord)

## Project city boundary data to meters
# set projection to 3435 - measures distance in feet.
proj4string(city.boundary) = "+init=epsg:3435"
# reproject to Coordinate Reference System (CRS) 26971 - measures distance in meters.
city.boundary = spTransform(city.boundary, CRS("+init=epsg:26971"))

# Run KDE plot
est = kde2d(meters[,'x_meter'], meters[,'y_meter'], n=c(300,300))
image(est, col = heat.colors, useRaster=TRUE, asp=1)
# Add city boundary
plot(city.boundary, axes=TRUE, add=TRUE, border="black", asp=1)


train <- cbind(meters$x_meter, meters$y_meter, )



