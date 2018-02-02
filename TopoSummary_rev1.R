# Setup
setwd("U:/Refugia/Persistance/")
library(ggplot2)
library(plyr)

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
Forest <- sampl.data[sampl.data[, "Cvr"] == "Forest",]
Range <- sampl.data[sampl.data[, "Cvr"] == "Range",]
sampl.data <- rbind(Forest, Range)
sampl.data$Cvr <- as.factor(sampl.data$Cvr)

# Group by unburned and cover type

# Median values by group
med <- ddply(sampl.data, c("Unburned", "Cvr"), summarise, # calculate medians
             TPI.med = median(TPI), TPI90.med = median(TPI_90), TRI.med = median(TRI), TRI90.med = median(TRI_90),
             rough.med = median(rough), rough90.med = median(rough_90), slope.med = median(slope), cosasp.med = median(cos.asp),
             TRASP.med = median(TRASP), SWASP.med = median(SWASP), SCOSA.med = median(SCOSA))
ddply(sampl.data, c("Cvr"), summarise, sd = sd(TPI_90))

# Faceted kernel density estimation plots
col <- c("#D55E00", "cyan3")

ggplot(sampl.data, aes(x=TPI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TPI90.med, colour=Unburned), linetype = "dashed", size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "TPI", y = "Density") +
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(-30, 30)) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom")
ggplot(sampl.data, aes(x=TRI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TRI90.med, colour=Unburned), linetype = "dashed", size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "TRI", y = "Density") +
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom")
ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = slope.med, colour=Unburned), linetype = "dashed", size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Slope (°)", y = "Density") +
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom")
ggplot(sampl.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = TRASP.med, colour=Unburned), linetype = "dashed", size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "TRASP", y = "Density") +
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom")
ggplot(sampl.data, aes(x=SCOSA, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(data = med, aes(xintercept = SCOSA.med, colour=Unburned), linetype = "dashed", size = 1) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "SCOSA", y = "Density") +
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom")

# Build data frames
TopoSummary <- data.frame(Variable = c("TPI_90", "TRI_90", "slope", "TRASP", "SCOSA"))

# K-S Tests
pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB` <- c(
  ks.test(pub$TPI_90, burn$TPI_90)$p.value,
  ks.test(pub$TRI_90, burn$TRI_90)$p.value,
  ks.test(pub$slope, burn$slope)$p.value,
  ks.test(pub$TRASP, burn$TRASP)$p.value,
  ks.test(pub$SCOSA, burn$SCOSA)$p.value)

pub.f <- Forest[Forest[, "Unburned"] == 1,]
burn.f <- Forest[Forest[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Forest` <- c(
  ks.test(pub.f$TPI_90, burn.f$TPI_90)$p.value,
  ks.test(pub.f$TRI_90, burn.f$TRI_90)$p.value,
  ks.test(pub.f$slope, burn.f$slope)$p.value,
  ks.test(pub.f$TRASP, burn.f$TRASP)$p.value,
  ks.test(pub.f$SCOSA, burn.f$SCOSA)$p.value)

pub.r <- Range[Range[, "Unburned"] == 1,]
burn.r <- Range[Range[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Range` <- c(
  ks.test(pub.r$TPI_90, burn.r$TPI_90)$p.value,
  ks.test(pub.r$TRI_90, burn.r$TRI_90)$p.value,
  ks.test(pub.r$slope, burn.r$slope)$p.value,
  ks.test(pub.r$TRASP, burn.r$TRASP)$p.value,
  ks.test(pub.r$SCOSA, burn.r$SCOSA)$p.value)

TopoSummary$`K-S.Forest.vs.Range` <- c(
  ks.test(Forest$TPI_90, Range$TPI_90)$p.value,
  ks.test(Forest$TRI_90, Range$TRI_90)$p.value,
  ks.test(Forest$slope, Range$slope)$p.value,
  ks.test(Forest$TRASP, Range$TRASP)$p.value,
  ks.test(Forest$SCOSA, Range$SCOSA)$p.value)

# K-W Tests
TopoSummary$`K-W.Burn.vs.PUB` <- c(
  kruskal.test(TPI_90 ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(slope ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(TRASP ~ Unburned, data = sampl.data)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = sampl.data)$p.value)

TopoSummary$`K-W.Burn.vs.PUB.Forest` <- c(
  kruskal.test(TPI_90 ~ Unburned, data = Forest)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = Forest)$p.value,
  kruskal.test(slope ~ Unburned, data = Forest)$p.value,
  kruskal.test(TRASP ~ Unburned, data = Forest)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = Forest)$p.value)

TopoSummary$`K-W.Burn.vs.PUB.Range` <- c(
  kruskal.test(TPI_90 ~ Unburned, data = Range)$p.value,
  kruskal.test(TRI_90 ~ Unburned, data = Range)$p.value,
  kruskal.test(slope ~ Unburned, data = Range)$p.value,
  kruskal.test(TRASP ~ Unburned, data = Range)$p.value,
  kruskal.test(SCOSA ~ Unburned, data = Range)$p.value)

TopoSummary$`K-W.Forest.vs.Range` <- c(
  kruskal.test(TPI_90 ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TRI_90 ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(slope ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(TRASP ~ Cvr, data = sampl.data)$p.value,
  kruskal.test(SCOSA ~ Cvr, data = sampl.data)$p.value)

TopoSummary[,-1] <- round(TopoSummary[,-1], digits = 4)

# col <- c("#D55E00", "#0072B2", "#E69F00", "#56B4E9")
