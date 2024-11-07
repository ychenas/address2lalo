
# import the data from dataframe 

addr="高鐵桃園站_0.15_"

df <- read.table(file=paste(addr,"df_out.txt",sep=""))

#aa <- TRUE
#while (aa==TRUE) {
#detach("package:jsonlite")
#detach("package:rgeos")
#detach("package:rgdal")
#detach("package:sf")
#detach("package:proj4")
#detach("package:stars")

#create a raster map 
# Load libraries
library(raster)
library(sp)
library(terra)
#



coordinates(df) <- ~lon+lat
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
# Create empty raster with specified extent and resolution 500m
r <- raster(extent(df), resolution = 0.005)
proj4string(r) <- CRS("+proj=longlat +datum=WGS84")
# Rasterize the data
r <- rasterize(df, r, field = "urban_trend", fun = mean, na.rm = TRUE)
# Plot
plot(r, main = "Raster Map from Data Frame Points")


#outprint the raster obj as Geotif formate  
writeRaster(r, filename=paste(addr,"urban_trend.tif",sep=""), format="GTiff", overwrite=TRUE)
#
