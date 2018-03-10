# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
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
sampl.data$Cvr[sampl.data$Cvr == 2] <- "Rangeland"
sampl.data$Cvr[sampl.data$Cvr == 3] <- "Other"
Forest <- sampl.data[sampl.data[, "Cvr"] == "Forest",]
Rangeland <- sampl.data[sampl.data[, "Cvr"] == "Rangeland",]
sampl.data <- rbind(Forest, Rangeland)
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

p_TPI_90 <- ggplot(sampl.data, aes(x=TPI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TPI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.3, x = -20, label = paste("\n", sprintf('\u2190'), "Valley"), size = 10/2.5) +
  annotate("text", y = 0.3, x = 0, label = "Flat", size = 10/2.5) +
  annotate("text", y = 0.3, x = 20, label = paste("\nRidge", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Topographic Posistion Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(-30, 30)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 1, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))
ggsave(plot = p_TPI_90, filename = "Topo_TPI90.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

p_TRI_90 <- ggplot(sampl.data, aes(x=TRI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.1135, x = 18, label = paste(sprintf('\u2190'), "Less rugged"), size = 10/2.5) +
  annotate("text", y = 0.1135, x = 48, label = paste("More rugged", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Terrain Ruggedness Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(0, 60)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top",plot.margin = margin(t= 0, l = 1, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))
ggsave(plot = p_TRI_90, filename = "Topo_TRI90.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

p_slope_90 <- ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = slope.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Slope (°)", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 1, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))
ggsave(filename = "Topo_slope.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

p_TRASP_90 <- ggplot(sampl.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRASP.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 1.765, x = 0.25, label = paste(sprintf('\u2190'), "NNE"), size = 10/2.5) +
  annotate("text", y = 1.765, x = 0.75, label = paste("SSW", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Transformed Aspect", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 1, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))
ggsave(filename = "Topo_TRASP.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

p_SCOSA_90 <- ggplot(sampl.data, aes(x=SCOSA, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = SCOSA.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "SCOSA", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(-40, 40)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 1, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))
ggsave(plot = p_SCOSA_90, filename = "Topo_SCOSA.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

# Build data frames
TopoSummary <- data.frame(Variable = c("TPI_90", "", "TRI_90", "", "slope", "", "TRASP", "", "SCOSA", "", "n1 =", "n2 ="))
TopoSummary$text <- c(rep(c("D =", "p <"), length = 10), "", "")

# K-S Tests
pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB` <- c(
  ks.test(pub$TPI_90, burn$TPI_90)[c(1,2)],
  ks.test(pub$TRI_90, burn$TRI_90)[c(1,2)],
  ks.test(pub$slope, burn$slope)[c(1,2)],
  ks.test(pub$TRASP, burn$TRASP)[c(1,2)],
  ks.test(pub$SCOSA, burn$SCOSA)[c(1,2)],
  nrow(pub), nrow(burn))

pub.f <- Forest[Forest[, "Unburned"] == 1,]
burn.f <- Forest[Forest[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Forest` <- c(
  ks.test(pub.f$TPI_90, burn.f$TPI_90)[c(1,2)],
  ks.test(pub.f$TRI_90, burn.f$TRI_90)[c(1,2)],
  ks.test(pub.f$slope, burn.f$slope)[c(1,2)],
  ks.test(pub.f$TRASP, burn.f$TRASP)[c(1,2)],
  ks.test(pub.f$SCOSA, burn.f$SCOSA)[c(1,2)],
  nrow(pub.f), nrow(burn.f))

pub.r <- Rangeland[Rangeland[, "Unburned"] == 1,]
burn.r <- Rangeland[Rangeland[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Rangeland` <- c(
  ks.test(pub.r$TPI_90, burn.r$TPI_90)[c(1,2)],
  ks.test(pub.r$TRI_90, burn.r$TRI_90)[c(1,2)],
  ks.test(pub.r$slope, burn.r$slope)[c(1,2)],
  ks.test(pub.r$TRASP, burn.r$TRASP)[c(1,2)],
  ks.test(pub.r$SCOSA, burn.r$SCOSA)[c(1,2)],
  nrow(pub.r), nrow(burn.r))

TopoSummary$`K-S.Forest.vs.Rangeland` <- c(
  ks.test(Forest$TPI_90, Rangeland$TPI_90)[c(1,2)],
  ks.test(Forest$TRI_90, Rangeland$TRI_90)[c(1,2)],
  ks.test(Forest$slope, Rangeland$slope)[c(1,2)],
  ks.test(Forest$TRASP, Rangeland$TRASP)[c(1,2)],
  ks.test(Forest$SCOSA, Rangeland$SCOSA)[c(1,2)],
  nrow(Forest), nrow(Rangeland))

TopoSummary[1:10,3:6] <- round(unlist(TopoSummary[1:10,3:6]), digits = 4)
TopoSummary[,3] <- paste(TopoSummary[,2], TopoSummary[,3])
TopoSummary[,4] <- paste(TopoSummary[,2], TopoSummary[,4])
TopoSummary[,5] <- paste(TopoSummary[,2], TopoSummary[,5])
TopoSummary[,6] <- paste(TopoSummary[,2], TopoSummary[,6])
TopoSummary <- TopoSummary[,-2]
write.csv(TopoSummary, file = "TopoStatTable.csv")

######

p_TPI_90 <- ggplot(sampl.data, aes(x=TPI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TPI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.3, x = -20, label = paste("\n", sprintf('\u2190'), "Valley"), size = 10/2.5) +
  annotate("text", y = 0.3, x = 0, label = "Flat", size = 10/2.5) +
  annotate("text", y = 0.3, x = 20, label = paste("\nRidge", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Topographic Posistion Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(-30, 30)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 10, r = 1, unit = "mm"),
        legend.margin = margin(t= 0, l = 5, b = -2, unit = "mm"))

p_TRI_90 <- ggplot(sampl.data, aes(x=TRI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.1135, x = 18, label = paste(sprintf('\u2190'), "Less rugged"), size = 10/2.5) +
  annotate("text", y = 0.1135, x = 48, label = paste("More rugged", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Terrain Ruggedness Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(0, 60)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top",plot.margin = margin(t= 0, l = 10, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))

p_slope <- ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = slope.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Slope (°)", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 10, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))

p_TRASP <- ggplot(sampl.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRASP.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 1.765, x = 0.25, label = paste(sprintf('\u2190'), "NNE"), size = 10/2.5) +
  annotate("text", y = 1.765, x = 0.75, label = paste("SSW", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Transformed Aspect", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= 0, l = 10, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -4, unit = "mm"))

p_Topo_L <- get_legend(p_TPI_90 + theme(legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="bottom",
                            legend.margin = margin(t= -6, b = -6, l = 63, unit = "mm")))

p <- plot_grid(p_TPI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_slope + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRASP + theme(legend.position="none", axis.title.y = element_blank()),
               p_Topo_L,
               ncol = 1, nrow = 5, rel_heights = c(1,1,1,1,.2), labels = c("A", "B", "C", "D"), vjust = 3, hjust = -3.5)

pp <- ggdraw(p) + annotate("text", y = .5, x = 0.013, label = "Density function (# of pixels)", angle = 90, size = 12/2.5)
ggsave(plot = pp, filename = "Topo_combined.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 225, units = "mm", dpi = 300)
