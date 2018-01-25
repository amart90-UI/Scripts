# Setup
setwd("U:/Refugia/Persistance/")
library(ggplot2)
library(reshape2)
#library(dunn.test)

# Load data
sampl.data <- read.csv("valuetable.csv")
sampl.data$Unburned <- factor(sampl.data$Unburned)


# Prep data (recode)
LookUp <- data.frame(old = c(2,3,4,5,7,8,9,10,11,12), 
                       new = c(1,2,2,2,2,3,2,2,3,3))
#"Forest & Woodland", "Shrubland & Grassland", "Semi-Desert", 
         #"Polar & High Montane Vegetation", "Nonvascular & Sparse Vascular Rock Vegetation", 
         #"Agricultural Vegetation", "Introduced & Semi Natural Vegetation", 
         #"Recently Disturbed or Modified", "Open Water", "Developed & Other Human Use"
for(i in 1:nrow(LookUp)){
  sampl.data$Cvr[sampl.data$LndCvr %in% LookUp$old[i]] <- LookUp$new[i]}
sampl.data$Cvr[sampl.data$Cvr == 1] <- "Forest"
sampl.data$Cvr[sampl.data$Cvr == 2] <- "Range"
sampl.data$Cvr[sampl.data$Cvr == 3] <- "Other"
pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
sampl.melt <- melt(sampl.data, id.vars = c("Unburned", "LndCvr"))

# Kernel density estimation plots
ggplot(sampl.data, aes(x=TPI, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Topographic position index (TPI; 3x3 widow) distribution by land cover", x = "TPI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=TPI_90, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Topographic position index (TPI; 7x7 widow) distribution by land cover", x = "TPI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=TRI, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Terrain roughness index (TRI; 3x3 widow) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=TRI_90, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Terrain roughness index (TRI; 7x7 widow) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=rough, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Topographic roughness (3x3 window) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=rough_90, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Topographic roughness (7x7 window) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Slope distribution by land cover", x = "Slope (°)", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=cos.asp, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Cosine of the aspect distribution by land cover", x = "Cosine of the Aspect", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Transformed aspect (TRASP) distribution by land cover", x = "TRASP", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=SWASP, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Southwest aspect (SWASP) distribution by land cover", x = "SWASP", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

ggplot(sampl.data, aes(x=SCOSA, colour=Unburned)) + 
  geom_density() +
  facet_wrap(~Cvr)+
  labs(title = "Slope cosine aspect interaction (SCOSA) distribution by land cover", x = "SCOSA", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

# K-S Tests
ks.test(pub$TPI, burn$TPI)
ks.test(pub$TRI, burn$TRI)
ks.test(pub$slope, burn$slope)
ks.test(pub$cos.asp, burn$cos.asp)
ks.test(pub$TRASP, burn$TRASP)
ks.test(pub$SWASP, burn$SWASP)
ks.test(pub$SCOSA, burn$SCOSA)

# K-W
sampl.data
kruskal.test(TPI ~ Unburned, data = sampl.data) # 0.02
kruskal.test(TRI ~ Unburned, data = sampl.data)
kruskal.test(slope ~ Unburned, data = sampl.data)
kruskal.test(cos.asp ~ Unburned, data = sampl.data)
kruskal.test(TRASP ~ Unburned, data = sampl.data)
kruskal.test(SWASP ~ Unburned, data = sampl.data) # 0.2256
kruskal.test(SCOSA ~ Unburned, data = sampl.data) # 0.9113

# CDF
ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  stat_ecdf() +
  facet_wrap(~Cvr)+
  labs(title = "Slope cosine aspect interaction (SCOSA) distribution by land cover", x = "SCOSA", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

