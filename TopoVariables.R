# Setup
setwd("U:/Refugia/Persistance/")
library(raster)
library(rgdal)

# Load data
dem <- raster("DEM/dem_perim_alb.tif")
#fireperim <- readOGR("Data/perims_dissolve.shp")

# Clip DEM to buffered raster
#perim.buff <- gBuffer(fireperim, width = 90)
#dem.mask <- mask(dem, perim.buff)

# Calculate variables
width <- 7 # 90 meter
med <- median(1:width^2)
f <- matrix(1, nrow = width, ncol = width)

TPI <- terrain(dem, opt="TPI", filename = "Intermediates/Variables/TPI.tif") #Topographic Position Index: Weiss, 2001
TPI90 <- focal(dem, w=f, fun=function(x, ...) x[med] - mean(x[-med]), pad=TRUE, padValue=NA)
writeRaster(TPI90, "Intermediates/Variables/TPI_90", "GTiff", overwrite = T)
TRI <- terrain(dem, opt="TRI", filename = "Intermediates/Variables/TRI.tif") #Topographic Ruggedness Index: Riley et al., 1999
TRI90 <- focal(dem, w=f, fun=function(x, ...) sum(abs(x[-med]-x[med]))/(width^2-1), pad=TRUE, padValue=NA)
writeRaster(TRI90, "Intermediates/Variables/TRI_90", "GTiff", overwrite = T)
rough <- terrain(dem, opt="roughness", filename = "Intermediates/Variables/rough.tif") #Topographic Roughness
rough90 <- focal(dem, w=f, fun=function(x, ...) max(x) - min(x), pad=TRUE, padValue=NA, na.rm=TRUE)
writeRaster(rough90, "Intermediates/Variables/rough_90", "GTiff", overwrite = T)
slope <- terrain(dem, opt="slope", unit="degrees", filename = "Intermediates/Variables/slope.tif")
aspect <- terrain(dem, opt="aspect", unit="degrees", filename = "Intermediates/Variables/aspect.tif")
cos.asp <- cos(aspect * pi/180)
writeRaster(cos.asp, "Intermediates/Variables/CosAsp", "GTiff", overwrite = T)
TRASP <- (-1 * cos((aspect - 30) * pi/180) + 1)/2 #Transformed aspect: Roberts & Cooper, 1989
writeRaster(TRASP, "Intermediates/Variables/TRASP", "GTiff", overwrite = T)
SWASP <- cos((45 - aspect) * pi/180) + 1 #Southwest aspect: Ohmann & Spies, 1998
writeRaster(SWASP, "Intermediates/Variables/SWASP", "GTiff", overwrite = T)
SCOSA <- slope * cos.asp #Slope cosine aspect interaction: Stage, 1976
writeRaster(SCOSA, "Intermediates/Variables/SCOSA", "GTiff", overwrite = T)

# Write intermediates
writeOGR(as(perim.buff, 'SpatialPolygonsDataFrame'), dsn = "U:/Refugia/Persistance/Intermediates", layer = "perim_buff", driver = "ESRI Shapefile")
