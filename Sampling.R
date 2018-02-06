# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(raster)

# Load data
sample.comb <- raster("Intermediates/SamplRas.tif")
TPI <- raster("Intermediates/Variables/TPI.tif")
TPI_90 <- raster("Intermediates/Variables/TPI_90.tif")
TRI <- raster("Intermediates/Variables/TRI.tif")
TRI_90 <- raster("Intermediates/Variables/TRI_90.tif")
rough <- raster("Intermediates/Variables/rough.tif")
rough_90 <- raster("Intermediates/Variables/rough_90.tif")
slope <- raster("Intermediates/Variables/slope2.tif")
aspect <- raster("Intermediates/Variables/aspect2.tif")
cos.asp <- raster("Intermediates/Variables/CosAsp2.tif")
TRASP <- raster("Intermediates/Variables/TRASP2.tif")
SWASP <- raster("Intermediates/Variables/SWASP2.tif")
SCOSA <- raster("Intermediates/Variables/SCOSA2.tif")
LndCvr  <- raster("Intermediates/LndCvr_mask2.tif")
load("Intermediates/nonNA.RData")

# Prep and stack rasters
#topo <- stack(LndCvr, TPI, TRI, rough, slope, cos.asp, TRASP, SWASP, SCOSA)
#topo.mask <- mask(topo, sample.comb)
#input.stack <- stack(sample.comb, topo.mask)
input.stack <- stack(sample.comb, LndCvr, TPI, TPI_90, TRI, TRI_90, rough, rough_90, slope, cos.asp, TRASP, SWASP, SCOSA)

# Extracct to table
#nonNa <- !is.na(as.numeric(sample.comb))
valuetable <- data.frame(input.stack[nonNa])
colnames(valuetable) <- c("Unburned", "LndCvr", "TPI", "TPI_90", "TRI", "TRI_90", "rough", "rough_90", "slope", "cos.asp", "TRASP", "SWASP", "SCOSA")
valuetable$Unburned[valuetable$Unburned == 2] <- 0
valuetable$Unburned[valuetable$Unburned == 5] <- 1
write.csv(valuetable, "valuetable.csv", row.names = F)
value.NA.rows <- valuetable[rowSums(is.na(valuetable)) > 0,]
valuetable <- na.exclude(valuetable)
