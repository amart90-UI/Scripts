# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(ggplot2)
library(plyr)
library(cowplot)
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
col <- c("#d8b365", "cyan3")

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
TopoSummary <- data.frame(Variable = c("", "TPI_90", "TRI_90", "slope", "cos.asp", "TRASP", "SCOSA", "n1 =", "n2 ="))

# K-S Tests
ks <- function(df1, df2, stat){
  a <- c( stat, round(unlist(c(
    ks.test(df1$TPI_90, df2$TPI_90)[stat],
    ks.test(df1$TRI_90, df2$TRI_90)[stat],
    ks.test(df1$slope, df2$slope)[stat],
    ks.test(df1$cos.asp, df2$cos.asp)[stat],
    ks.test(df1$TRASP, df2$TRASP)[stat],
    ks.test(df1$SCOSA, df2$SCOSA)[stat])), 4), 
    nrow(df1), nrow(df2))
  return(a)
}

pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
TopoSummary$Burn.Vs.PUB.d <- ks(pub, burn, "statistic")
TopoSummary$Burn.Vs.PUB.p <- ks(pub, burn, "p.value")

pub.f <- Forest[Forest[, "Unburned"] == 1,]
burn.f <- Forest[Forest[, "Unburned"] == 0,]
TopoSummary$Burn.Vs.PUB.For.d <- ks(pub.f, burn.f, "statistic")
TopoSummary$Burn.Vs.PUB.For.p <- ks(pub.f, burn.f, "p.value")

pub.r <- Rangeland[Rangeland[, "Unburned"] == 1,]
burn.r <- Rangeland[Rangeland[, "Unburned"] == 0,]
TopoSummary$Burn.Vs.PUB.Ran.d <- ks(pub.r, burn.r, "statistic")
TopoSummary$Burn.Vs.PUB.Ran.p <- ks(pub.r, burn.r, "p.value")

TopoSummary$ForestVs.Range.d <- ks(Forest, Rangeland, "statistic")
TopoSummary$ForestVs.Range.p <- ks(Forest, Rangeland, "p.value")

write.csv(TopoSummary, file = "TopoStatTable.csv", row.names = F)

######

p_TPI_90 <- ggplot(sampl.data, aes(x=TPI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TPI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.3, x = -20, label = paste("\n", sprintf('\u2190'), "Valley"), size = 9/2.5) +
  annotate("text", y = 0.3, x = 0, label = "Flat", size = 9/2.5) +
  annotate("text", y = 0.3, x = 20, label = paste("\nRidge", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Topographic Posistion Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(-30, 30)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=0, b=-6)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)), legend.position="top", plot.margin = margin(t= -6, l = 10, r = 1, b=1, unit = "mm"),
        legend.margin = margin(t= 0, l = 5, b = -2, unit = "mm"))

p_TRI_90 <- ggplot(sampl.data, aes(x=TRI_90, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.1135, x = 18, label = paste(sprintf('\u2190'), "Less rugged"), size = 9/2.5) +
  annotate("text", y = 0.1135, x = 48, label = paste("More rugged", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Terrain Ruggedness Index", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(0, 60)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=0, b=-6)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)), legend.position="top",plot.margin = margin(t= -6, l = 6, r = 1, b=1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))

p_slope <- ggplot(sampl.data, aes(x=slope, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = slope.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Slope (°)", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=0, b=-5)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)), legend.position="top", plot.margin = margin(t= -6, l = 7, r = 1, b=1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))

p_TRASP <- ggplot(sampl.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = TRASP.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 1.765, x = 0.25, label = paste(sprintf('\u2190'), "NNE"), size = 9/2.5) +
  annotate("text", y = 1.765, x = 0.75, label = paste("SSW", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Transformed Aspect", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=0, b=-4)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)), legend.position="top", plot.margin = margin(t= -6, l = 9, r = 1, b=0, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -4, unit = "mm"))

p_Topo_L <- get_legend(p_TPI_90 + theme(legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="bottom",
                            legend.margin = margin(t= -6, b = -6, l = 63, unit = "mm")))

p <- plot_grid(p_TPI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_TWI + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_slope + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRASP + theme(legend.position="none", axis.title.y = element_blank()),
               p_Topo_L, 
               ncol = 1, nrow = 6, rel_heights = c(1,1,1,1,1,.2), 
               labels = c("a", "b", "c", "d", "e"), vjust = c(1,1.1,1,1.2,1), hjust = c(-4.5, -3.8, -4, -3.8, -4.2))
pp <- ggdraw(p) + 
  annotate("text", y = .5, x = 0.013, label = "Density function (% relative frequency)", angle = 90, size = 12/2.5) +
  annotate("segment", x = 0.05, xend = 1, y = 0.808, yend = 0.808) +
  annotate("segment", x = 0.05, xend = 1, y = 0.614, yend = 0.614) +
  annotate("segment", x = 0.05, xend = 1, y = 0.423, yend = 0.423) +
  annotate("segment", x = 0.05, xend = 1, y = 0.23, yend = 0.23)

ggsave(plot = pp, filename = "Topo_combined.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 180, units = "mm", dpi = 300)
