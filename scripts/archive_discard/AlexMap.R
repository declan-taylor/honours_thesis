# Followed the tutorial from https://www.zoology.ubc.ca/~kgilbert/mysite/Miscellaneous_files/R_MakingMaps.pdf

# libraries
library(sp)
library(maptools)
gpclibPermit()
library(maps)
library(mapdata)
library(mapproj)
library(rgdal)
library(scales)

sites = read.delim("sites.txt", sep = "\t", header = T)

png(here("figures/AlexMap.png"), 1000, 1000, "px")

maps::map(database = "world",
          projection = "lambert",
          parameters = c(49,79),
          boundary = TRUE,
          xlim = c(-140, -40),
          ylim = c(46, 90),
          col = "gray80",
          fill = TRUE, 
          orientation = c(90,0,260))

points(78.882, 75.803, pch = 19, col = "red", cex = 1)

dev.off()

maps::map("worldHires",
          xlim = c(-77, -59),
          ylim = c(37,50),
          col = "gray80", 
          fill = TRUE)

colours = c("Robbinston" = "#264D59", "Kettle Cove" = "#43978D","Beverly" = "#F9E07F", "Newport" = "#F9AD6A","Cape May" = "#D46C4E")


points(sites$Long, sites$Lat, pch = 19, col = colours)


