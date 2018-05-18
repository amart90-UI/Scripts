##
library(ggplot2)
library(plyr)
TWI <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/Variables/Iterators/TWI/TWI_Sample.csv", na.strings = -9999)
vegsamp <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/VegSampling/VegSamp.csv", na.strings = -9999)
TWI <- cbind(TWI, vegsamp$LCvr.Gap)
colnames(TWI) <- c("Unburned", "TWI", "GAP")
TWI$Unburned[TWI$Unburned == 2] <- 0
TWI$Unburned[TWI$Unburned == 5] <- 1
TWI$Unburned <- as.factor(TWI$Unburned)
LookUp2 <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/LandCover/GapAttrib.csv")
Reclass2 <- data.frame(old = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                      new.c = c(1,2,2,2,3,2,3,3,2,3,3,2),
                      new = c("Forest","Rangeland","Rangeland","Rangeland","Other","Rangeland","Other","Other","Rangeland","Other","Other","Rangeland"))
TWI$ForRan <- LookUp2$CL[match(unlist(TWI$GAP), LookUp2$Value)]
TWI$ForRan <- Reclass2$new[match(unlist(TWI$ForRan), Reclass2$old)]
TWI <- TWI[TWI$ForRan %in% c("Forest", "Rangeland"),]

med2 <- ddply(na.omit(TWI), c("Unburned", "ForRan"), summarise, TWI.med = median(TWI))
col <- c("#d8b365", "cyan3")

p_TWI <- ggplot(TWI, aes(x=TWI, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(data = med2, aes(xintercept = TWI.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 250, x = 0.005, label = paste(sprintf('\u2190'), "Less runoff"), size = 9/2.5) +
  annotate("text", y = 250, x = 0.020, label = paste("More runoff", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~ForRan)+
  labs(title = "", x = "Topographic Wetness Index", y = "") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  coord_cartesian(xlim = c(0, .025)) +
  scale_x_continuous(breaks = c(0, 0.01, 0.02)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=0, b=-6)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)), legend.position="top", plot.margin = margin(t= -6, l = 7, r = 1, b=1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -2, unit = "mm"))

pub2 <- TWI[TWI[, "Unburned"] == 1,]
burn2 <- TWI[TWI[, "Unburned"] == 0,]
pub2.f <- pub2[pub2[, "ForRan"] == "Forest",]
burn2.f <- burn2[burn2[, "ForRan"] == "Forest",]
pub2.r <- pub2[pub2[, "ForRan"] == "Range",]
burn2.r <- burn2[burn2[, "ForRan"] == "Range",]
for2 <- TWI[TWI[, "ForRan"] == "Forest",]
ran2 <- TWI[TWI[, "ForRan"] == "Range",]

ks.test(pub2$TWI, burn2$TWI)[c(1,2)]
ks.test(pub2.f$TWI, burn2.f$TWI)[c(1,2)]
ks.test(pub2.r$TWI, burn2.r$TWI)[c(1,2)]
ks.test(for2$TWI, ran2$TWI)[c(1,2)]

##

# Build data frames
TopoSummary <- data.frame(Variable = c("TPI_90", "", "TRI_90", "", "slope", "", "cos.asp", "", "TRASP", "", "SCOSA", "", "n1 =", "n2 ="))
TopoSummary$text <- c(rep(c("D =", "p <"), length = 12), "", "")

# K-S Tests
pub <- sampl.data[sampl.data[, "Unburned"] == 1,]
burn <- sampl.data[sampl.data[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB` <- c(
  ks.test(pub$TPI_90, burn$TPI_90)[c(1,2)],
  ks.test(pub2$TWI, burn2$TWI)[c(1,2)],
  ks.test(pub$TRI_90, burn$TRI_90)[c(1,2)],
  ks.test(pub$slope, burn$slope)[c(1,2)],
  ks.test(pub$cos.asp, burn$cos.asp)[c(1,2)],
  ks.test(pub$TRASP, burn$TRASP)[c(1,2)],
  ks.test(pub$SCOSA, burn$SCOSA)[c(1,2)],
  nrow(pub), nrow(burn))

pub.f <- Forest[Forest[, "Unburned"] == 1,]
burn.f <- Forest[Forest[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Forest` <- c(
  ks.test(pub.f$TPI_90, burn.f$TPI_90)[c(1,2)],
  ks.test(pub.f$TRI_90, burn.f$TRI_90)[c(1,2)],
  ks.test(pub.f$slope, burn.f$slope)[c(1,2)],
  ks.test(pub.f$cos.asp, burn.f$cos.asp)[c(1,2)],
  ks.test(pub.f$TRASP, burn.f$TRASP)[c(1,2)],
  ks.test(pub.f$SCOSA, burn.f$SCOSA)[c(1,2)],
  nrow(pub.f), nrow(burn.f))

pub.r <- Rangeland[Rangeland[, "Unburned"] == 1,]
burn.r <- Rangeland[Rangeland[, "Unburned"] == 0,]
TopoSummary$`K-S.Burn.vs.PUB.Rangeland` <- c(
  ks.test(pub.r$TPI_90, burn.r$TPI_90)[c(1,2)],
  ks.test(pub.r$TRI_90, burn.r$TRI_90)[c(1,2)],
  ks.test(pub.r$slope, burn.r$slope)[c(1,2)],
  ks.test(pub.r$cos.asp, burn.r$cos.asp)[c(1,2)],
  ks.test(pub.r$TRASP, burn.r$TRASP)[c(1,2)],
  ks.test(pub.r$SCOSA, burn.r$SCOSA)[c(1,2)],
  nrow(pub.r), nrow(burn.r))

TopoSummary$`K-S.Forest.vs.Rangeland` <- c(
  ks.test(Forest$TPI_90, Rangeland$TPI_90)[c(1,2)],
  ks.test(Forest$TRI_90, Rangeland$TRI_90)[c(1,2)],
  ks.test(Forest$slope, Rangeland$slope)[c(1,2)],
  ks.test(Forest$cos.asp, Rangeland$cos.asp)[c(1,2)],
  ks.test(Forest$TRASP, Rangeland$TRASP)[c(1,2)],
  ks.test(Forest$SCOSA, Rangeland$SCOSA)[c(1,2)],
  nrow(Forest), nrow(Rangeland))

TopoSummary[1:12,3:6] <- round(unlist(TopoSummary[1:12,3:6]), digits = 4)
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
  geom_vline(data = med2, aes(xintercept = TPI90.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
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
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= -6, l = 10, r = 1, unit = "mm"),
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
        strip.text.x = element_text(size = 12), legend.position="top",plot.margin = margin(t= -6, l = 6, r = 1, unit = "mm"),
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
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= -6, l = 7, r = 1, unit = "mm"),
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
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= -6, l = 9, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -4, unit = "mm"))

p_Topo_L <- get_legend(p_TPI_90 + theme(legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="bottom",
                            legend.margin = margin(t= -6, b = -6, l = 63, unit = "mm")))

p <- plot_grid(p_TPI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_TWI + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRI_90 + theme(legend.position="none", axis.title.y = element_blank()),
               p_slope + theme(legend.position="none", axis.title.y = element_blank()),
               p_TRASP + theme(legend.position="none", axis.title.y = element_blank()),
               p_Topo_L, ncol = 1, nrow = 5, rel_heights = c(1,1,1,1,.2), 
               labels = c("a", "b", "c", "d", "e"), vjust = 1, hjust = c(-4.5, -3.8, -3.8, -3.8, -3.8))

pp <- ggdraw(p) + annotate("text", y = .5, x = 0.013, label = "Density function (% relative frequency)", angle = 90, size = 12/2.5)
ggsave(plot = pp, filename = "Topo_combined.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 200, units = "mm", dpi = 300)
#####
ggplot(sampl.data, aes(x=cos.asp, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  geom_vline(data = med, aes(xintercept = cosasp.med, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  #annotate("text", y = 1.765, x = 0.25, label = paste(sprintf('\u2190'), "NNE"), size = 10/2.5) +
  #annotate("text", y = 1.765, x = 0.75, label = paste("SSW", sprintf('\u2192')), size = 10/2.5) +
  facet_wrap(~Cvr)+
  labs(title = "", x = "Transformed Aspect", y = "Density function (# of pixels)") +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   ", "Persistent Unburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12), legend.position="top", plot.margin = margin(t= -6, l = 9, r = 1, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -4, unit = "mm"))
