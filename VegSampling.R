setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(raster)
fbfm <- raster("Intermediates/FBFM_sample.tif")
samp <- raster("Intermediates/SamplRas.tif")
load("Intermediates/nonNA1.RData")
input.stack <- stack(samp,fbfm)
valuetable <- data.frame(input.stack[nonNa1])
write.csv(valuetable, "vegvalue.csv")

####
a <- read.table("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/vegval.TXT")
dim(a)
head(a)
levels(a$V1)
no.data <- a$V2[a$V1 == "-9999"]
head(no.data)
a[1:20,]
b <- a[1:10,]
b$V2[b$V2 == -9999] <- NA
c <- na.exclude(b)
d <- na.omit(b)

####
samp <- read.table("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/sampval.TXT")
veg <- read.table("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/vegval.TXT")
veg.samp <- cbind(samp, veg)
rm(samp, veg)
gc()
veg.samp[,c(2,4)][veg.samp[,c(2,4)] == -9999] <- NA
veg.samp.narm <- na.omit(veg.samp[1:20000000])
####
samp <- read.table("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/sampval.TXT")
samp <- samp[, 1]
no.na <- which(samp != -9999)
samp2 <- samp[no.na]
samp3 <- as.numeric(samp2[-c(1,2,3,4,5,6)])
write.csv(samp3, "S:/COS/PyroGeog/amartinez/Persistance/Intermediates/sampval2.csv")
rm(samp, samp2)
gc()

veg <- read.table("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/vegval.TXT")
veg <- veg[, 1]
veg2 <- veg[no.na]
veg3 <- as.numeric(veg2[-c(1,2,3,4,5,6)])
write.csv(samp3, "S:/COS/PyroGeog/amartinez/Persistance/Intermediates/vegval2.csv")

sampveg <- cbind(samp3, veg3)
colnames(sampveg) <- c("Sample", "FBFM.13")
write.csv(sampveg, "S:/COS/PyroGeog/amartinez/Persistance/Intermediates/sampveg.csv", row.names = F)
object.size(samp3)
summary(samp3)
table(samp3)
sum(is.na(sampveg))
a <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/sampveg.csv")
head(a)
