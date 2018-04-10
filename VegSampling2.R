setwd("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/VegSampling")

# Define vegetation sampling Function
# Inputs files: UI, fuel model, landcover, and gap land cover
# Input type: text files (ArcGIS: Raster to ASCII tool)
# User input: Number of split file (name_#.txt; ArcGIS Split Raster tool)
VegSample <- function(n){
  samp <- read.table(paste0("samp_", n, ".txt"), fill = T)
  samp <- samp[-c(1,2,3,4,5,6),]
  samp[,1] <- as.numeric(as.character(samp[,1]))
  samp <- c(samp[,1], samp[,2])
  no.na <- which(samp != -9999)
  samp <- samp[no.na]
  veg <- read.table(paste0("veg_", n, ".txt"), fill = T)
  veg <- veg[-c(1,2,3,4,5,6),]
  veg[,1] <- as.numeric(veg[,1])
  veg <- c(veg[,1], veg[,2])
  veg <- veg[no.na]
  lnd <- read.table(paste0("lcvr_", n, ".txt"), fill = T)
  lnd <- lnd[-c(1,2,3,4,5,6),]
  lnd[,1] <- as.numeric(lnd[,1])
  lnd <- c(lnd[,1], lnd[,2])
  lnd <- lnd[no.na]
  gap <- read.table(paste0("gap_", n, ".txt"), fill = T)
  gap <- gap[-c(1,2,3,4,5,6),]
  gap[,1] <- as.numeric(gap[,1])
  gap <- c(gap[,1], gap[,2])
  gap <- gap[no.na]
  vegsamp <- data.frame(UI = samp, LCvr.3 = lnd, LCvr.Gap = gap, FBFM = veg)
  write.csv(vegsamp, paste0("vegsamp", n, ".csv"), row.names = F)
  return(vegsamp)
}

# Permorm function on all spit raster tables (x4)
v0 <- VegSample(0)
v1 <- VegSample(1)
v2 <- VegSample(2)
v3 <- VegSample(3)

# Merge tables and write to CSV
VegSamp <- data.frame(rbind(v0, v1, v2, v3))
write.csv(VegSamp, paste0("VegSamp.csv"), row.names = F)

######
