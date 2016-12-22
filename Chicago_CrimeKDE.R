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

# Import city boundary
city.boundary = readShapePoly("City_Boundary/geo_export_8e9e40f9-be43-489a-a8a7-64eb8423712c")

# Import crime data
crime_full <- read.csv("Crimes_-_One_year_prior_to_present.csv")
crime <- na.omit(crime_full) # Remove observations with missing data - 265 observations
rm(crime_full) # Remove full dataset to clear workspace

# Look at types of crime
sort(table(crime$PRIMARY.DESCRIPTION), decreasing = TRUE)

##### TIME #####
# convert strings to dates
crimes.dates = strptime(crime[,"DATE..OF.OCCURRENCE"],"%m/%d/%Y %I:%M:%S %p")
# reassemble more friendly crimes.sample matrix
crime <- data.frame(cbind(crime,
                          format(crimes.dates),         # crime date/time
                          hour(crimes.dates),           # crime hour
                          wday(crimes.dates),           # crime day of week
                          month(crimes.dates)))         # crime month

#### PROJECTIONS
## Project crime data to meters
coord <- data.frame('long'=crime$LONGITUDE, 'lat'=crime$LATITUDE)
coordinates(coord) <- ~ long+lat
# set projection to 3435 - measures distance in feet.
proj4string(coord) <- CRS("+init=epsg:3435")
# reproject to Coordinate Reference System (CRS) 26971 - measures distance in meters.
xycoord = data.frame(spTransform(coord, CRS("+init=epsg:26971")))
colnames(xycoord) <- c("x_meter","y_meter")
meters <- cbind(crime, xycoord)

## Project Chicago boundary data to meters
# set projection to 3435 - measures distance in feet.
proj4string(city.boundary) = "+init=epsg:3435"
# reproject to Coordinate Reference System (CRS) 26971 - measures distance in meters.
city.boundary = spTransform(city.boundary, CRS("+init=epsg:26971"))

# Subset data set for KDE plotting
meters.plot<-meters#[meters$hour<=4,]
# Run KDE plot
est = kde2d(meters.plot[,'x_meter'], meters.plot[,'y_meter'], n=c(300,300)) 
image(est, col = heat.colors, useRaster=TRUE, asp=1)
# Add city boundary
plot(city.boundary, axes=TRUE, add=TRUE, border="black", asp=1)

### RESOURCES
## Projection
# http://stackoverflow.com/questions/18706369/convert-latitude-longitude-to-state-plane-coordinates
# http://prj2epsg.org/epsg/2263