# Setup
setwd("U:/Refugia/Persistance/")
library(raster)
library(rgdal)

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
cos.asp <- raster("Intermediates/Variables/CosAsp.tif")
TRASP <- raster("Intermediates/Variables/TRASP.tif")
SWASP <- raster("Intermediates/Variables/SWASP.tif")
SCOSA <- raster("Intermediates/Variables/SCOSA.tif")
LndCvr  <- raster("Intermediates/LndCvr_mask2.tif")
load("Intermediates/nonNA.RData")

# Prep and stack rasters
topo <- stack(LndCvr, TPI, TRI, rough, slope, cos.asp, TRASP, SWASP, SCOSA)
#topo.mask <- mask(topo, sample.comb)
#input.stack <- stack(sample.comb, topo.mask)
input.stack <- stack(sample.comb, LndCvr, TPI, TPI_90, TRI, TRI_90, rough, rough_90, slope, cos.asp, TRASP, SWASP, SCOSA)

# Extracct to table
#nonNa <- !is.na(as.numeric(sample.comb))
valuetable <- data.frame(input.stack[nonNa])
colnames(valuetable) <- c("Unburned", "LndCvr", "TPI", "TPI_90", "TRI", "TRI_90", "rough", "rough_90", "slope", "cos.asp", "TRASP", "SWASP", "SCOSA")
valuetable <- na.exclude(valuetable)
valuetable$Unburned[valuetable$Unburned == 2] <- 0
valuetable$Unburned[valuetable$Unburned == 5] <- 1
write.csv(valuetable, "valuetable.csv", row.names = F)

# Random Forest Model
rfm <- randomForest(Unburned~., data = valuetable3, importance = T)
save(rfm, "rfm.RData")
varImpPlot(rfm)


writeRaster(sample.comb, "sample_comb", "GTiff")
save(nonNa, file = "Intermediates/nonNA.RData")
save(topo.mask, file = "Intermediates/topo_mask.RData")
write.csv(valuetable, "valuetable.csv", row.names = F)


#
colnames(valuetable3) <- c("Unburned", "TPI", "TRI", "rough", "slope", "cos.asp", "TRASP", "SWASP", "SCOSA")
valuetable3 <- na.exclude(valuetable3)
valuetable3$Unburned[valuetable3$Unburned == 2] <- 0
valuetable3$Unburned[valuetable3$Unburned == 5] <- 1


#
valuetable <- read.csv("valuetable.csv")
v <- na.exclude(valuetable)
valuetable[,9] <- LndCvr[nonNa]
v <- LndCvr[nonNa]
