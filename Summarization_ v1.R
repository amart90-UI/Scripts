library(ggplot2)
library(plyr)
library(cowplot)


sample.pers <- na.omit(read.csv("S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_pers.csv"))
sample.burn <- na.omit(read.csv("S:/COS/PyroGeog/amartinez/Persistance/Sample/sample_burn.csv"))

sample.pers <- subset(sample.pers, FRG2 < 6)[1:30000,]
sample.pers$Unburned <- 1
sample.burn <- subset(sample.burn, FRG2 < 6)[1:30000,]
sample.burn$Unburned <- 0
sample.comb <- rbind(sample.pers, sample.burn)
sample.comb$Unburned <- as.factor(sample.comb$Unburned)

##
colnames(sample.comb) <- c("Landcover", "FRG", "TPI", "TWI", "TRI", "Slope", "CosAsp", "TRASP", "SWASP", "SCOSA", "Unburned")


# K-S tests for Topographic Sample Significance
ks <- function(df1, df2, stat){
  a <- c( stat, round(unlist(c(
    ks.test(df1$TPI, df2$TPI)[stat],
    ks.test(df1$TWI, df2$TWI)[stat],
    ks.test(df1$TRI, df2$TRI)[stat],
    ks.test(df1$Slope, df2$Slope)[stat],
    ks.test(df1$CosAsp, df2$CosAsp)[stat],
    ks.test(df1$TRASP, df2$TRASP)[stat],
    ks.test(df1$SWASP, df2$SWASP)[stat],
    ks.test(df1$SCOSA, df2$SCOSA)[stat])), 4), 
    nrow(df1), nrow(df2))
  return(a)
}

for(i in 1:5){
  assign(paste0("pers.", i), subset(sample.comb, Unburned == 1 & FRG == paste(i)))
  assign(paste0("burn.", i), subset(sample.comb, Unburned == 0 & FRG == paste(i)))
}

TopoSummary <- data.frame(Variable = c("", "TPI", "TWI", "TRI", "Slope", "CosAsp", "TRASP", "SWASP", "SCOSA", "n1 =", "n2 ="))
TopoSummary$Burn.Vs.PUB.d <- ks(subset(sample.comb, Unburned == 1), subset(sample.comb, Unburned == 0), "statistic")
TopoSummary$Burn.Vs.PUB.p <- ks(subset(sample.comb, Unburned == 1), subset(sample.comb, Unburned == 0), "p.value")
TopoSummary$Burn.Vs.PUB.FRG1.d <- ks(pers.1, burn.1, "statistic")
TopoSummary$Burn.Vs.PUB.FRG1.p <- ks(pers.1, burn.1, "p.value")
TopoSummary$Burn.Vs.PUB.FRG2.d <- ks(pers.2, burn.2, "statistic")
TopoSummary$Burn.Vs.PUB.FRG2.p <- ks(pers.2, burn.2, "p.value")
TopoSummary$Burn.Vs.PUB.FRG3.d <- ks(pers.3, burn.3, "statistic")
TopoSummary$Burn.Vs.PUB.FRG3.p <- ks(pers.3, burn.3, "p.value")
TopoSummary$Burn.Vs.PUB.FRG4.d <- ks(pers.4, burn.4, "statistic")
TopoSummary$Burn.Vs.PUB.FRG4.p <- ks(pers.4, burn.4, "p.value")
TopoSummary$Burn.Vs.PUB.FRG5.d <- ks(pers.5, burn.5, "statistic")
TopoSummary$Burn.Vs.PUB.FRG5.p <- ks(pers.5, burn.5, "p.value")
write.csv(TopoSummary, file = "S:/COS/PyroGeog/amartinez/Persistance/Outputs/TopoStatTable.csv", row.names = F)

# Plots for Topographic Samples
plot.data <- sample.comb
Lookup.FRG <- data.frame(old = 1:5,
           new = paste("Fire Regime Group", c("I", "II", "III", "IV", "V")))
plot.data$FRG <- Lookup.FRG$new[match(unlist(plot.data$FRG), Lookup.FRG$old)]
Mode <- function(x, ...){as.numeric(names(which.max(table(x))))}
sample.med <- ddply(plot.data, c("Unburned", "FRG"), summarise, # calculate medians
                    Landcover = Mode(Landcover), TPI = median(TPI), TWI = median(TWI), TRI = median(TRI), Slope = median(Slope),
                    CosAsp = median(CosAsp), TRASP = median(TRASP), SWASP = median(SWASP), SCOSA = median(SCOSA))

col <- c("#d8b365", "cyan3")

ks2 <- function(df1, df2){
  a <- unlist(c(ks.test(df1$TPI, df2$TPI)["p.value"],
                ks.test(df1$TWI, df2$TWI)["p.value"],
                ks.test(df1$TRI, df2$TRI)["p.value"],
                ks.test(df1$Slope, df2$Slope)["p.value"],
                ks.test(df1$CosAsp, df2$CosAsp)["p.value"],
                ks.test(df1$TRASP, df2$TRASP)["p.value"],
                ks.test(df1$SWASP, df2$SWASP)["p.value"],
                ks.test(df1$SCOSA, df2$SCOSA)["p.value"]))
  b <- ifelse(a > 0.0001, paste("p =", round(a, 4)), "p < 0.0001")
  
  return(b)
}

m <- matrix(nc = 8, nr = 5)
for(i in 1:5){
  m[i,] <- ks2(get(paste0("pers.",i)), get(paste0("burn.",i)))
}
p.labs <- data.frame(FRG = paste("Fire Regime Group", c("I", "II", "III", "IV", "V")), m)
colnames(p.labs) <- c("FRG", "TPI", "TWI", "TRI", "Slope", "CosAsp", "TRASP", "SWASP", "SCOSA")

p_TPI <- ggplot(plot.data, aes(x=TPI, colour=Unburned)) +
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="black", size=.8) +
  geom_vline(data = sample.med, aes(xintercept = TPI, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.3, x = -20, label = paste("\n", sprintf('\u2190'), "Valley"), size = 9/2.5) +
  annotate("text", y = 0.3, x = 0, label = "Flat", size = 9/2.5) +
  annotate("text", y = 0.3, x = 20, label = paste("\nRidge", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~FRG, ncol = 1) +
  labs(title = "", x = "Topographic Posistion Index", y = "Density function (# of pixels)") +
  geom_text(data = p.labs, mapping = aes(x =20, y = .1, label = TPI), inherit.aes = F) +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   \n", "Persistent\nUnburned"), values = col) +
  coord_cartesian(xlim = c(-25, 25)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=2, b=-4)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)),
        legend.position="bottom", plot.margin = margin(t= -6, l = 0, r = 1, b=4, unit = "mm"), legend.margin = margin(t= -2, l = 15, b = -2, unit = "mm"))
ggsave(plot = p_TPI, filename = "TPI.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots2/",
       width = 85, height = 170, units = "mm", dpi = 600)

p_TWI <- ggplot(plot.data, aes(x=TWI, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="black", size=.8) +
  geom_vline(data = sample.med, aes(xintercept = TWI, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 250, x = 0.005, label = paste(sprintf('\u2190'), "Less runoff"), size = 9/2.5) +
  annotate("text", y = 250, x = 0.020, label = paste("More runoff", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~FRG, ncol = 1) +
  labs(title = "", x = "Topographic Wetness Index", y = "Density function (# of pixels)") +
  geom_text(data = p.labs, mapping = aes(x =10, y = .2, label = TWI), inherit.aes = F) +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   \n", "Persistent\nUnburned"), values = col) +
  coord_cartesian(ylim = c(0, .4), xlim = c(0, 12)) +
  scale_y_continuous(breaks = c(0, 0.20, 0.4)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=2, b=-4)), axis.title=element_text(size=12), 
      legend.text = element_text(size=10),legend.title=element_text(size=12), strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)),
      legend.position="bottom", plot.margin = margin(t= -6, l = 0, r = 1, b=4, unit = "mm"), legend.margin = margin(t= -2, l = 15, b = -2, unit = "mm"))
ggsave(plot = p_TWI, filename = "TWI.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots2/",
       width = 85, height = 170, units = "mm", dpi = 600)

p_TRI <- ggplot(plot.data, aes(x=TRI, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="black", size=.8) +
  geom_vline(data = sample.med, aes(xintercept = TRI, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 0.09, x = 12, label = paste(sprintf('\u2190'), "Less rugged"), size = 9/2.5) +
  annotate("text", y = 0.09, x = 50, label = paste("More rugged", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~FRG, ncol = 1) +
  labs(title = "", x = "Terrain Ruggedness Index", y = "Density function (# of pixels)") +
  geom_text(data = p.labs, mapping = aes(x =53, y = .035, label = TRI), inherit.aes = F) +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   \n", "Persistent\nUnburned"), values = col) +
  coord_cartesian(xlim = c(0, 60)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=2, b=-4)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)),
        legend.position="bottom", plot.margin = margin(t= -6, l = 0, r = 1, b=4, unit = "mm"), legend.margin = margin(t= -2, l = 15, b = -2, unit = "mm"))
ggsave(plot = p_TRI, filename = "TRI.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots2/",
       width = 85, height = 170, units = "mm", dpi = 600)

p_Slope <- ggplot(plot.data, aes(x=Slope, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_hline(yintercept=0, colour="black", size=.8) +
  geom_vline(data = sample.med, aes(xintercept = Slope, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  facet_wrap(~FRG, ncol = 1) +
  labs(title = "", x = "Slope (°)", y = "Density function (# of pixels)") +
  geom_text(data = p.labs, mapping = aes(x =53, y = .03, label = Slope), inherit.aes = F) +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   \n", "Persistent\nUnburned"), values = col) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1)) +
  coord_cartesian(xlim = c(0, 60)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=2, b=-4)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)),
        legend.position="bottom", plot.margin = margin(t= -6, l = 0, r = 1, b=4, unit = "mm"), legend.margin = margin(t= -2, l = 15, b = -2, unit = "mm"))
ggsave(plot = p_Slope, filename = "Slope.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots2/",
       width = 85, height = 170, units = "mm", dpi = 600)

p_TRASP <- ggplot(plot.data, aes(x=TRASP, colour=Unburned)) + 
  geom_density(size = 1) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  geom_hline(yintercept=0, colour="black", size=.8) +
  geom_vline(data = sample.med, aes(xintercept = TRASP, colour=Unburned, linetype = Unburned), size = .5, show.legend = F) +
  annotate("text", y = 1.765, x = 0.25, label = paste(sprintf('\u2190'), "NNE"), size = 9/2.5) +
  annotate("text", y = 1.765, x = 0.75, label = paste("SSW", sprintf('\u2192')), size = 9/2.5) +
  facet_wrap(~FRG, ncol = 1) +
  labs(title = "", x = "Transformed Aspect", y = "Density function (# of pixels)") +
  geom_text(data = p.labs, mapping = aes(x =0.9, y = .2, label = TRASP), inherit.aes = F) +
  scale_linetype_manual(values = c("dashed", "longdash"))+
  scale_color_manual("", labels = c("Burned   \n", "Persistent\nUnburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, margin = margin(t=2, b=-4)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), strip.text.x = element_text(size = 11, margin = margin(t=2, b=4)),
        legend.position="bottom", plot.margin = margin(t= -6, l = 0, r = 1, b=4, unit = "mm"), legend.margin = margin(t= -2, l = 15, b = -2, unit = "mm"))
ggsave(plot = p_TRASP, filename = "TRASP.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots2/",
       width = 85, height = 170, units = "mm", dpi = 600)


####
ks2 <- function(df1, df2){
  a <- unlist(c(ks.test(df1$TPI, df2$TPI)["p.value"],
  ks.test(df1$TWI, df2$TWI)["p.value"],
  ks.test(df1$TRI, df2$TRI)["p.value"],
  ks.test(df1$Slope, df2$Slope)["p.value"],
  ks.test(df1$CosAsp, df2$CosAsp)["p.value"],
  ks.test(df1$TRASP, df2$TRASP)["p.value"],
  ks.test(df1$SWASP, df2$SWASP)["p.value"],
  ks.test(df1$SCOSA, df2$SCOSA)["p.value"]))
  b <- ifelse(a > 0.0001, paste("p =", round(a, 4)), "p < 0.0001")
  
  return(b)
}
ks2(pers.1, burn.1)
df1 <- pers.1
df2 <- burn.1
stat <- "statistic"

View(t(data.frame(TopoSummary[2:9, c(1,5,7,9,11,13)], row.names = 1)))
p.labs <- data.frame(FRG = paste("Fire Regime Group", c("I", "II", "III", "IV", "V")),
                    t(data.frame(TopoSummary[2:9, c(1,5,7,9,11,13)], row.names = 1)), row.names = NULL)
p.labs <- data.frame(t(data.frame(TopoSummary[2:9, c(1,5,7,9,11,13)], row.names = 1)), row.names = NULL)
p.labs[] <- as.numeric(unlist(p.labs[]))
class(p.labs$TPI)



