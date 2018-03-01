# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(raster)
library(dplyr)

# Load data
study <- read.csv("Intermediates/SpatialDensity/StudyArea_6km.csv")
fire1 <- read.csv("Intermediates/SpatialDensity/fire1.csv")
ui1 <- read.csv("Intermediates/SpatialDensity/ui1.csv")
ras <- raster("Intermediates/SpatialDensity/StudyArea6ID.tif")

# Calculate areas (including overlap)
fire1$AREA2 <- fire1$AREA * fire1$overlap
ui1$AREA2 <-  ui1$AREA * ui1$overlap

# Summarize data
fire2 <- fire1[fire1$overlap > 1,]
ui2 <- ui1[ui1$overlap > 1,]

study.summary <- summarize(group_by(study, FID_fishne), Area_study = sum(Area))
fire1.summary <- summarize(group_by(fire1, FID_fishne), Area_fire1 = sum(AREA2))
fire2.summary <- summarize(group_by(fire2, FID_fishne), Area_fire2 = sum(AREA2))
ui1.summary <- summarize(group_by(ui1, FID_fishne), Area_ui1 = sum(AREA2))
ui2.summary <- summarize(group_by(ui2, FID_fishne), Area_ui2 = sum(AREA2))

Areas <- join_all(list(study.summary, fire1.summary, fire2.summary, ui1.summary, ui2.summary), by = "FID_fishne")

# Calculate proportions
Proportion <-data.frame(ID = Areas$FID_fishne,
                        Fire1 = Areas$Area_fire1 / Areas$Area_study,
                        Fire2 = Areas$Area_fire2 / Areas$Area_study,
                        UI1 = Areas$Area_ui1 / Areas$Area_study,
                        UI2 = Areas$Area_ui2 / Areas$Area_study)
Proportion <- Proportion[rowSums(is.na(Proportion[,-1])) < 4,]
Proportion[is.na(Proportion)] <- 0

# Build Proportion (Density) Rasters
Fire1 <- subs(ras, Proportion, by = 1, which = 2,  filename = "Intermediates/SpatialDensity/Fire1.tif", overwrite = T)
Fire2 <- subs(ras, Proportion, by = 1, which = 3,  filename = "Intermediates/SpatialDensity/Fire2.tif", overwrite = T)
UI1 <- subs(ras, Proportion, by = 1, which = 4,  filename = "Intermediates/SpatialDensity/UI1.tif", overwrite = T)
UI2 <- subs(ras, Proportion, by = 1, which = 5,  filename = "Intermediates/SpatialDensity/UI2.tif", overwrite = T)


# Plot Maps
plot(Fire1)
plot(Fire2)
plot(UI1)
plot(UI2)
