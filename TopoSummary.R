# Setup
setwd("U:/Refugia/Persistance/")
library(ggplot2)
library(reshape2)
library(plyr)
#library(dunn.test)

# Load data
sampl.data <- read.csv("valuetable.csv")
sampl.data <- na.exclude(sampl.data)
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
sampl.melt <- melt(sampl.data, id.vars = c("Unburned", "LndCvr"))
Forest <- sampl.data[sampl.data[, "Cvr"] == "Forest",]
Range <- sampl.data[sampl.data[, "Cvr"] == "Range",]
sampl.data <- rbind(Forest, Range)
sampl.data$Cvr <- as.factor(sampl.data$Cvr)

# Group by unburned and cover type
sampl.data$Group[sampl.data$Unburned == 1] <- "Unburned"
sampl.data$Group[sampl.data$Unburned == 0] <- "Burned"
sampl.data$Group <- paste(sampl.data$Group, sampl.data$Cvr)

# Median values by group
med <- ddply(sampl.data, "Group", summarise, # calculate medians
             TPI.med = median(TPI), TPI90.med = median(TPI_90), TRI.med = median(TRI), TRI90.med = median(TRI_90),
             rough.med = median(rough), rough90.med = median(rough_90), slope.med = median(slope), cosasp.med = median(cos.asp),
             TRASP.med = median(TRASP), SWASP.med = median(SWASP), SCOSA.med = median(SCOSA))

# Kernel density estimation plots
col <- c("#D55E00", "#0072B2", "#E69F00", "#56B4E9")

ggplot(sampl.data, aes(x=TPI, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TPI.med, colour=Group), linetype = "dashed") +
  labs(title = "Topographic position index (TPI; 3x3 widow) distribution by land cover", x = "TPI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=TPI_90, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TPI90.med, colour=Group), linetype = "dashed") +
  labs(title = "Topographic position index (TPI; 7x7 widow) distribution by land cover", x = "TPI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=TRI, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TRI.med, colour=Group), linetype = "dashed") +
  labs(title = "Terrain roughness index (TRI; 3x3 widow) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=TRI_90, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TRI90.med, colour=Group), linetype = "dashed") +
  labs(title = "Terrain roughness index (TRI; 7x7 widow) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=rough, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = rough.med, colour=Group), linetype = "dashed") +
  labs(title = "Topographic roughness (3x3 window) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=rough_90, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = rough90.med, colour=Group), linetype = "dashed") +
  labs(title = "Topographic roughness (7x7 window) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=slope, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = slope.med, colour=Group), linetype = "dashed") +
  labs(title = "Slope distribution by land cover", x = "Slope (°)", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=cos.asp, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = cosasp.med, colour=Group), linetype = "dashed") +
  labs(title = "Cosine of the aspect distribution by land cover", x = "Cosine of the Aspect", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=TRASP, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TRASP.med, colour=Group), linetype = "dashed") +
  labs(title = "Transformed aspect (TRASP) distribution by land cover", x = "TRASP", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=SWASP, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = SWASP.med, colour=Group), linetype = "dashed") +
  labs(title = "Southwest aspect (SWASP) distribution by land cover", x = "SWASP", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

ggplot(sampl.data, aes(x=SCOSA, colour=Group)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = SCOSA.med, colour=Group), linetype = "dashed") +
  labs(title = "Slope cosine aspect interaction (SCOSA) distribution by land cover", x = "SCOSA", y = "Density") +
  scale_color_manual("Dashed line \nas median", values = col)

# Example of faceted topo density plot
ggplot(sampl.data, aes(x=SCOSA, colour=Unburned)) + 
  geom_density(size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "Terrain roughness index (TRI; 7x7 widow) distribution by land cover", x = "TRI", y = "Density") +
  scale_color_manual(labels = c("Burned", "Unburned"), values = c("red", "cyan3"))

# Build data frames
TopoSummary <- data.frame(Variable = c("TPI", "TPI_90", "TRI", "TRI_90", "rough", "rough_90", "slope", "cos.asp", "TRASP", "SWASP", "SCOSA"))

# K-S Tests
pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB` <- c(
  ks.test(pub$TPI, burn$TPI)$p.value,
  ks.test(pub$TPI_90, burn$TPI_90)$p.value,
  ks.test(pub$TRI, burn$TRI)$p.value,
  ks.test(pub$TRI_90, burn$TRI_90)$p.value,
  ks.test(pub$rough, burn$rough)$p.value,
  ks.test(pub$rough_90, burn$rough_90)$p.value,
  ks.test(pub$slope, burn$slope)$p.value,
  ks.test(pub$cos.asp, burn$cos.asp)$p.value,
  ks.test(pub$TRASP, burn$TRASP)$p.value,
  ks.test(pub$SWASP, burn$SWASP)$p.value,
  ks.test(pub$SCOSA, burn$SCOSA)$p.value)

pub.f <- Forest[Forest[, "Unburned"] == 1,]
burn.f <- Forest[Forest[, "Unburned"] == 0,]

TopoSummary$`K-S.Burn.vs.PUB.Forest` <- c(
  ks.test(pub.f$TPI, burn.f$TPI)$p.value,
  ks.test(pub.f$TPI_90, burn.f$TPI_90)$p.value,
  ks.test(pub.f$TRI, burn.f$TRI)$p.value,
  ks.test(pub.f$TRI_90, burn.f$TRI_90)$p.value,
  ks.test(pub.f$rough, burn.f$rough)$p.value,
  ks.test(pub.f$rough_90, burn.f$rough_90)$p.value,
  ks.test(pub.f$slope, burn.f$slope)$p.value,
  ks.test(pub.f$cos.asp, burn.f$cos.asp)$p.value,
  ks.test(pub.f$TRASP, burn.f$TRASP)$p.value,
  ks.test(pub.f$SWASP, burn.f$SWASP)$p.value,
  ks.test(pub.f$SCOSA, burn.f$SCOSA)$p.value)

pub.r <- Range[Range[, "Unburned"] == 1,]
burn.r <- Range[Range[, "Unburned"] == 0,]

TopoSummary$`K-S.Burn.vs.PUB.Range` <- c(
  ks.test(pub.r$TPI, burn.r$TPI)$p.value,
  ks.test(pub.r$TPI_90, burn.r$TPI_90)$p.value,
  ks.test(pub.r$TRI, burn.r$TRI)$p.value,
  ks.test(pub.r$TRI_90, burn.r$TRI_90)$p.value,
  ks.test(pub.r$rough, burn.r$rough)$p.value,
  ks.test(pub.r$rough_90, burn.r$rough_90)$p.value,
  ks.test(pub.r$slope, burn.r$slope)$p.value,
  ks.test(pub.r$cos.asp, burn.r$cos.asp)$p.value,
  ks.test(pub.r$TRASP, burn.r$TRASP)$p.value,
  ks.test(pub.r$SWASP, burn.r$SWASP)$p.value,
  ks.test(pub.r$SCOSA, burn.r$SCOSA)$p.value)

TopoSummary$`K-S.Forest.vs.Range` <- c(
  ks.test(Forest$TPI, Range$TPI)$p.value,
  ks.test(Forest$TPI_90, Range$TPI_90)$p.value,
  ks.test(Forest$TRI, Range$TRI)$p.value,
  ks.test(Forest$TRI_90, Range$TRI_90)$p.value,
  ks.test(Forest$rough, Range$rough)$p.value,
  ks.test(Forest$rough_90, Range$rough_90)$p.value,
  ks.test(Forest$slope, Range$slope)$p.value,
  ks.test(Forest$cos.asp, Range$cos.asp)$p.value,
  ks.test(Forest$TRASP, Range$TRASP)$p.value,
  ks.test(Forest$SWASP, Range$SWASP)$p.value,
  ks.test(Forest$SCOSA, Range$SCOSA)$p.value)

# K-W Tests
TopoSummary$`K-W.Burn.vs.PUB` <- c(
  kruskal.test(TPI ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TPI_90 ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TRI ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(rough ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(rough_90 ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(slope ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(cos.asp ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TRASP ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(SWASP ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = sampl.data)$p.value)

TopoSummary$`K-W.Burn.vs.PUB.Forest` <- c(
  kruskal.test(TPI ~ Unburned, data = Forest)$p.value,
  kruskal.test(TPI_90 ~ Unburned, data = Forest)$p.value,
  kruskal.test(TRI ~ Unburned, data = Forest)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = Forest)$p.value,
  kruskal.test(rough ~ Unburned, data = Forest)$p.value,
  kruskal.test(rough_90 ~ Unburned, data = Forest)$p.value,
  kruskal.test(slope ~ Unburned, data = Forest)$p.value,
  kruskal.test(cos.asp ~ Unburned, data = Forest)$p.value,
  kruskal.test(TRASP ~ Unburned, data = Forest)$p.value,
  kruskal.test(SWASP ~ Unburned, data = Forest)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = Forest)$p.value)

TopoSummary$`K-W.Burn.vs.PUB.Range` <- c(
  kruskal.test(TPI ~ Unburned, data = Range)$p.value,
  kruskal.test(TPI_90 ~ Unburned, data = Range)$p.value,
  kruskal.test(TRI ~ Unburned, data = Range)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = Range)$p.value,
  kruskal.test(rough ~ Unburned, data = Range)$p.value,
  kruskal.test(rough_90 ~ Unburned, data = Range)$p.value,
  kruskal.test(slope ~ Unburned, data = Range)$p.value,
  kruskal.test(cos.asp ~ Unburned, data = Range)$p.value,
  kruskal.test(TRASP ~ Unburned, data = Range)$p.value,
  kruskal.test(SWASP ~ Unburned, data = Range)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = Range)$p.value)

TopoSummary$`K-W.Forest.vs.Range` <- c(
  kruskal.test(TPI ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TPI_90 ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TRI ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TRI_90 ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(rough ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(rough_90 ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(slope ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(cos.asp ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TRASP ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(SWASP ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(SCOSA ~ Cvr, data = sampl.data)$p.value)

TopoSummary[,-1] <- round(TopoSummary[,-1], digits = 4)

