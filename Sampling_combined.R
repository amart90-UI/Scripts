library(raster)
library(sp)
library(rgdal)
setwd("S:/COS/PyroGeog/amartinez/Persistance/Sample/")

######
pers.1 <- raster("Working/TPI_persist2.tif")
burn.1 <- raster("Working/TPI_burn2.tif")

pers.nona <- Which(!is.na(pers.1), cells = T)
pers.samp <- sample(pers.nona, size = 32000, replace = F)
save(pers.samp, file = "pers_samp")

burn.nona <- Which(!is.na(burn.1), cells = T)
burn.samp <- sample(burn.nona, size = 32000, replace = F)
save(burn.samp, file = "burn_samp")

######
load("pers_samp")
load("burn_samp")

setwd("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/Variables/")
LndCvr  <- raster("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/LndCvr_mask2.tif")
FRG <- raster("FRG2.tif")
TPI <- raster("TPI_90_2.tif")
TWI <- raster("TWI.tif")
TRI <- raster("TRI_90_2.tif")
slope <- raster("slope2.tif")
cos.asp <- raster("CosAsp2.tif")
TRASP <- raster("TRASP2.tif")
SWASP <- raster("SWASP2.tif")
SCOSA <- raster("SCOSA2.tif")

topo.stack <- stack(LndCvr, FRG, TPI, TWI, TRI, slope, cos.asp, TRASP, SWASP, SCOSA)
sample.pers <- topo.stack[pers.samp]
write.csv(sample.pers, file = "S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_pers.csv", row.names = F)
sample.burn <- topo.stack[burn.samp]
write.csv(sample.pers, file = "S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_pers.csv", row.names = F)

#####
sample.pers <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_pers.csv")
sample.burn <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_pers.csv")

sample.pers.nona <- na.omit(sample.pers)
sample.burn.nona <- na.omit(sample.burn)


#####
#UIoverlap <- readOGR("S:/COS/PyroGeog/amartinez/Persistance/Data/ui_overlap.shp")
#proj4string(UIoverlap)
#proj4string(TPI)
#writeOGR(obj = UIoverlap.proj, dsn = "S:/COS/PyroGeog/amartinez/Persistance/Data", layer = "ui_overlap_proj", driver="ESRI Shapefile")
###

UI <- as(readOGR("S:/COS/PyroGeog/amartinez/Persistance/Data/ui_overlap_proj.shp"), "SpatialPolygons")
Mode <- function(x, ...){as.numeric(names(which.max(table(x))))}
topo.stack.1 <- stack(LndCvr, FRG)
topo.stack.2 <- stack(TPI, TWI, TRI, slope, cos.asp, TRASP, SWASP, SCOSA)
UI.ext.1 <- extract(topo.stack.1, UI, fun = Mode, df = T)
write.csv(UI.ext.1, file = "S:/COS/PyroGeog/amartinez/Persistance/Outputs/UI_vars_1.csv", row.names = F)
UI.ext.2 <- extract(topo.stack.2, UI, fun = mean, na.rm = T, df = T)
write.csv(UI.ext.2, file = "S:/COS/PyroGeog/amartinez/Persistance/Outputs/UI_vars_2.csv", row.names = F)

UI.ext <- cbind(UI.ext.1, UI.ext.2)
write.csv(UI.ext, file = "S:/COS/PyroGeog/amartinez/Persistance/Outputs/UI_vars.csv", row.names = F)
