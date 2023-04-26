# Load required packages
library(maptools)
library(rgdal)
library(raster)

# Set working directory to where your data is stored
setwd("path/to/working/directory")

# Read in shapefiles
alexandra <- readShapeSpatial("alexandra.shp")
points_of_interest <- readShapeSpatial("points_of_interest.shp")

# Read in raster layer
raster_layer <- raster("raster_layer.tif")

# Define Lambert projection
lambert <- CRS("+proj=lcc +lat_1=70 +lat_2=80 +lat_0=56.6666666666667 +lon_0=-101.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# Project shapefiles and raster layer to Lambert projection
alexandra_lambert <- spTransform(alexandra, lambert)
points_of_interest_lambert <- spTransform(points_of_interest, lambert)
raster_layer_lambert <- projectRaster(raster_layer, crs=lambert)

# Create base map
plot(alexandra_lambert, col="lightblue", xlim=c(-1000000, 1000000), ylim=c(-1000000, 1000000), axes=FALSE, ann=FALSE)
box()

# Add raster layer
plot(raster_layer_lambert, col=rev(terrain.colors(255)), add=TRUE)

# Add shapefiles
plot(points_of_interest_lambert, add=TRUE, pch=19, col="red")
legend("bottomleft", legend="Points of Interest", pch=19, col="red", bty="n", cex=0.8)

# Add inset map
par(fig=c(0, 0.25, 0, 0.25), new=TRUE)
plot(alexandra_lambert, col="lightblue", xlim=c(-3500000, 3500000), ylim=c(-3500000, 3500000), axes=FALSE, ann=FALSE)
plot(points_of_interest_lambert, add=TRUE, pch=19, col="red")
box()

# Add Canada outline to inset map
canada <- readOGR("path/to/canada.shp")
canada_lambert <- spTransform(canada, lambert)
plot(canada_lambert, add=TRUE, border="black", lwd=1.5)

# Reset plotting parameters
par(fig=c(0, 1, 0, 1), new=TRUE)

# Add title and labels
title(main="Map of Alexandra Fiord, Nunavut, Canada", cex.main=1.2)
title(xlab="Easting (m)", ylab="Northing (m)")

# Save plot as a PDF file
dev.copy(pdf, "alexandra_map.pdf")
dev.off()
