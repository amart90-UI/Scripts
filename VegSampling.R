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
###########
setwd("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/VegSampling")
samp0 <- read.table("samp_0.txt", fill = T)
samp0 <- samp0[-c(1,2,3,4,5,6),]
samp0[,1] <- as.numeric(samp0[,1])
samp0 <- c(samp0[,1], samp0[,2])
no.na0 <- which(samp0 != -9999)
samp0 <- samp0[no.na0]

veg0 <- read.table("veg_0.txt", fill = T)
veg0 <- veg0[-c(1,2,3,4,5,6),]
veg0[,1] <- as.numeric(veg0[,1])
veg0 <- c(veg0[,1], veg0[,2])
veg0 <- veg0[no.na0]

vegsamp <- data.frame(UI = samp0, FBFM = veg0)

write.csv("vegsamp0.csv")

VegSample <- function(s){
  assign(paste0("s", s), read.table("samp_", s, ".txt", fill = T))
  assign(paste0("s", s), get(paste0("s", s))[-c(1,2,3,4,5,6),])
  assign(paste0("s", s)[ ,1], as.numeric(get(paste0("s", s))[, 1]))
  assign(paste0("s", s), c(get(paste0("s", s))[, 1], get(paste0("s", s))[, 2])
  assign(paste0("no.na", s), which(get(paste0("s", s)) != -9999))
  assign(paste0("s", s), get(paste0("s", s))[get(paste0("no.na", s))])
  
  assign(paste0("v", s), read.table("veg_", s, ".txt", fill = T))
  assign(paste0("v", s), get(paste0("v", s))[-c(1,2,3,4,5,6),])
  assign(paste0("v", s)[ ,1], as.numeric(get(paste0("v", s))[, 1]))
  assign(paste0("v", s), c(get(paste0("v", s))[, 1], get(paste0("v", s))[, 2])
  assign(paste0("v", s), get(paste0("v", s))[get(paste0("no.na", s))])
  
  veg0 <- veg0[no.na0]
}


####
VegSample <- function(n){
  samp <- read.table(paste0("samp_", n, ".txt"), fill = T)
  samp <- samp[-c(1,2,3,4,5,6),]
  samp[,1] <- as.numeric(samp[,1])
  samp <- c(samp[,1], samp[,2])
  no.na <- which(samp != -9999)
  samp <- samp[no.na0]
  veg <- read.table(paste0("veg_", n, ".txt"), fill = T)
  veg <- veg[-c(1,2,3,4,5,6),]
  veg[,1] <- as.numeric(veg[,1])
  veg <- c(veg[,1], veg[,2])
  veg <- veg[no.na]
  vegsamp <- data.frame(UI = samp, FBFM = veg)
  write.csv(vegsamp, paste0("vegsamp", n, ".csv"))
}
##
VegSample(0)
VegSample(1)
VegSample(2)
VegSample(3)
