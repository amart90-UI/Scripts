# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(raster)
library(rgdal)
library(reshape2)
library(ggplot2)
library(plyr)
library(gridExtra)
library(cowplot)

# Load data
#fire <- readOGR("Data/perims_dissolve.shp")
#fire.data <- fire@data
fire.table <- read.csv("Intermediates/fire_area.csv")
#ui <- readOGR("Data/ui_overlap.shp")
#ui.data <- ui@data[,-1]
#write.csv(ui.data, "Intermediates/OverlapData.csv")
#ui.data <- read.csv("Intermediates/OverlapData.csv")
ui.data <- read.csv("Intermediates/OverlapLndCvr.csv")
#ui.data <- ui.data[, c(2:5]
#ui.data$Cvr <- rep(1:2, length.out = 816802)
ui.data$overlap <- factor(ui.data$overlap)
ui.data$lvl[ui.data$overlap == 1] <- "Unburned for 1 fire  "
ui.data$lvl[ui.data$overlap == 2] <- "Unburned for 2 fires  "
ui.data$lvl[ui.data$overlap == 3] <- "Unburned for 3 fires  "
ui.data$lvl[ui.data$overlap == 4] <- "Unburned for 4 fires  "

Forest <- ui.data[ui.data$Cvr == 1,]
Range <- ui.data[ui.data$Cvr == 2,]

# Fire boundary statistics
fire.area <- 82239808708 # sum(fire.data$Area)

# Shape indices
ui.data$FRAC <- 2 * log(.25 * ui.data$Perim) / log(ui.data$AREA) # Fractal dimension index
ui.data$PARA <- ui.data$Perim / ui.data$Area # Perimeter-Area Ratio
# Summary stats: http://www.umass.edu/landeco/research/fragstats/documents/Metrics/Shape%20Metrics/SHAPE%20METRICS.htm

# Area summary
area.stats <- ddply(ui.data, "overlap", summarise,
                    total = sum(AREA), 
                    prop.persist = sum(AREA), 
                    min = min(AREA), 
                    max = max(AREA),
                    mean = mean(AREA), 
                    area.median = median(AREA), 
                    sd = sd(AREA), 
                    se = sd(AREA)/sqrt(length(AREA)))
area.stats$prop.persist <- c(0, area.stats$prop.persist[-1] / sum(area.stats$total[-1]))

# Proportions
prop.stats <- ddply(ui.data, "overlap", summarise,
                    count = length(overlap), 
                    count.persist = length(overlap), 
                    count.unburned = length(overlap)/length(ui.data$AREA), 
                    area.fire = sum(AREA) / fire.area,
                    area.unburned = sum(AREA) / sum(ui.data$AREA),
                    area.persist = sum(AREA),
                    area.persist.2 = sum(AREA))
prop.stats$count.persist <- c(0, prop.stats$count.persist[-1] / sum(prop.stats$count.persist[-1]))
prop.stats$area.persist <- c(0, prop.stats$area.persist[-1] / sum(area.stats$total[-1]))
prop.stats$area.persist.2 <- c(0, 0, prop.stats$area.persist.2[-c(1:2)] / sum(area.stats$total[-c(1:2)]))

# Shape index summary
shape.stats <- ddply(ui.data, "overlap", summarise,
                     FRAC.mean = mean(FRAC), 
                     FRAC.median = median(FRAC), 
                     FRAC.sd = sd(FRAC), 
                     FRAC.AreaWtMean = sum(FRAC * AREA), 
                     PARA.mean = mean(PARA), 
                     PARA.median = median(PARA), 
                     FRAC.sd = sd(PARA), 
                     PARA.AreaWtMean = sum(PARA * AREA))
shape.stats$FRAC.AreaWtMean <- shape.stats$FRAC.AreaWtMean / area.stats$total
shape.stats$PARA.AreaWtMean <- shape.stats$PARA.AreaWtMean / area.stats$total

# Construct dataframe
stats <- data.frame(cbind(prop.stats, area.stats[,-1], shape.stats[,-1]))
stats$overlap <- factor(stats$overlap)
#write.csv(stats, "Persitsent_stats.csv")
#read.csv("Persitsent_stats.csv")

# Proportion by area plot
lvl <- c("Burned, 1 fire", "Burned, 2+ fires  ", "Unburned for 1 fire", "Unburned for 2 fires  ", "Unburned for 3 fires", "Unburned for 4 fires")
col <- c("#a50026", "#f46d43", "#d9ef8b", "#66bd63", "#1a9850", "#006837")
burn.1 <- fire.table$SUM_AREA[1] /sum(fire.table$SUM_AREA)
burn.2 <- sum(fire.table$SUM_AREA[-1]) /sum(fire.table$SUM_AREA)
area.prop <- data.frame(overlap = c(lvl, lvl[3:6],lvl[4:6], lvl[5:6]),
                        values = c(burn.1 * (1-sum(prop.stats$area.fire)), burn.2 * (1-sum(prop.stats$area.fire)), prop.stats[,5], prop.stats[,6], prop.stats[-1,7], prop.stats[3:4,8]),
                        group = factor(c(rep("Within fire\nperimeter", times = 6), rep("Among unburned\nislands", times = 4), 
                                         rep("Among persistant\nunburned (2+ fires)", times = 3), rep("Among persistant\nunburned (3+ fires)", times = 2))))
area.prop$group <- factor(area.prop$group, levels = levels(area.prop$group)[c(4, 3, 1, 2)])
area.prop$values <- 100 * area.prop$values

ggplot(aes(y = values, x = group, fill = overlap), data = area.prop) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  labs(title = "", x = "", y = "Proportion of area") +
  scale_fill_manual("Degree of\npersistence ", values = col) +
  guides(fill=guide_legend(nrow=2,byrow=F))+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(position = "bottom") +
  theme(axis.ticks.x=element_blank(), axis.text.y=element_text(size=10), axis.title=element_text(size=12), axis.text.x = element_text(size = 10), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="top", plot.margin = margin(t= -1, l = 1, r = 1, b= -4, unit = "mm"),
        legend.margin = margin(t= -4, l = 5, b = -4, unit = "mm"))
ggsave(filename = "Area_combined.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

c(fire.table$SUM_AREA[1] / 10000, sum(fire.table$SUM_AREA[-1]) / 10000, area.stats$total / 10000)

# Shape KDE plots
med <- ddply(ui.data, "lvl", summarise, # calculate medians
             AREA.med = median(AREA), PARA.med = median(PARA), FRAC.med = median(FRAC))
med$lvl <- factor(med$lvl)


ggplot(ui.data, aes(x=AREA, colour = lvl)) +
  geom_density(adjust = 4.5, size = .5) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=lvl),  size = .5, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  geom_vline(xintercept=0, colour="white", size=.5) +
  geom_hline(yintercept=0, colour="white", size=.5) +
  scale_x_continuous(breaks = seq(0, 10000, 2500), limits=c(0, 10000)) +
  labs(colour = "Degree of Persistence ", x = expression(paste("Area (", m^2, ")")), y = "Density function (# of pixels)") +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=26),legend.title=element_text(size=28), legend.position="top")
ggsave(filename = "Shape_Area.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

ggplot(ui.data, aes(x=FRAC, colour = lvl)) +
  geom_density(adjust = 3, size = .5) +
  geom_vline(data = med, aes(xintercept = FRAC.med, colour=lvl),  size = .5, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  geom_hline(yintercept=0, colour="white", size=.5) +
  coord_cartesian(xlim = c(1, 1.15))  +
  labs(colour = "Degree of Persistence ", x = "Fractional dimension index", y = "Density function (# of pixels)") +
  annotate("text", y = 33, x = 1.026, label = paste(sprintf('\u2190'), "More simple"), size = 7/2.5) +
  annotate("text", y = 33, x = 1.11, label = paste("More complex", sprintf('\u2192')), size = 7/2.5) +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="top")
ggsave(filename = "Shape_FRAC.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 180, units = "mm", dpi = 300)

# Partition data for testing
ui.1 <- ui.data[ui.data$overlap == 1,]
ui.2 <- ui.data[ui.data$overlap == 2,]
ui.3 <- ui.data[ui.data$overlap == 3,]
ui.4 <- ui.data[ui.data$overlap == 4,]

# K-S test
ShapeSummary <-data.frame("Degree of persistence" = c("1 vs. 2", "", "1 vs. 3", "", "1 vs. 4", "", "2 vs. 3", "", "2 vs. 4", "", "3 vs. 4", ""),
                          text = c(rep(c("D =", "p <"), length = 12)))

ShapeSummary$Area <- c(ks.test(ui.1$AREA, ui.2$AREA)[c(1,2)],
                       ks.test(ui.1$AREA, ui.3$AREA)[c(1,2)],
                       ks.test(ui.1$AREA, ui.4$AREA)[c(1,2)],
                       ks.test(ui.2$AREA, ui.3$AREA)[c(1,2)],
                       ks.test(ui.2$AREA, ui.4$AREA)[c(1,2)],
                       ks.test(ui.3$AREA, ui.4$AREA)[c(1,2)])
ShapeSummary$FRAC <- c(ks.test(ui.1$FRAC, ui.2$FRAC)[c(1,2)],
                       ks.test(ui.1$FRAC, ui.3$FRAC)[c(1,2)],
                       ks.test(ui.1$FRAC, ui.4$FRAC)[c(1,2)],
                       ks.test(ui.2$FRAC, ui.3$FRAC)[c(1,2)],
                       ks.test(ui.2$FRAC, ui.4$FRAC)[c(1,2)],
                       ks.test(ui.3$FRAC, ui.4$FRAC)[c(1,2)])

ShapeSummary[,3:4] <- round(unlist(ShapeSummary[,3:4]), digits = 4)
ShapeSummary[,3] <- paste(ShapeSummary[,2], ShapeSummary[,3])
ShapeSummary[,4] <- paste(ShapeSummary[,2], ShapeSummary[,4])
ShapeSummary <- ShapeSummary[, -2]
write.csv(ShapeSummary, file = "ShapeSummary.csv")

# Compare Observed and Expected
# Expected reburn (km2) Stevens-Rumann et al., 2016
fire.exp1 <- data.frame(x = c(1:6), y = 0.1945461^c(1:6) * (fire.area * 10^-6))
fire.exp2 <- data.frame(x = seq(from = 1, to = 6, by = 0.1),
                        y = (fire.area * 10^-6) * 0.1945461^seq(from = 1, to = 6, by = 0.1))

# observed fire function
fire.obs <- data.frame(x = 0:6, y = rev(cumsum(rev(fire.table$SUM_AREA * 10^-6))))
fire.obs <- fire.obs[-1,]
mod2 <- lm(log(y) ~ x, data = fire.obs)
fire.obs.pred <- data.frame(x = seq(from = 1, to = 6, by = 0.1), 
                            y = exp(predict(mod2, newdata = data.frame(x = seq(from = 1, to = 6, by = 0.1)))))

# Expected ui function (km2) Meddens et al., 2018
ui.exp1 <- data.frame(x = c(1:4), y = 0.09669098^c(1:4) * (fire.area * 10^-6))
ui.exp2 <- data.frame(x = seq(from = 1, to = 4, by = 0.05),
                      y = (fire.area * 10^-6) * 0.09669098^seq(from = 1, to = 4, by = 0.05))

# observed ui function
ui.obs <- data.frame(x = c(1:4), y = area.stats$total * 10^-6)
mod1 <- lm(log(y) ~ x, data = ui.obs)
ui.obs.pred <- data.frame(x = seq(from = 1, to = 4, by = 0.05), 
                          y = exp(predict(mod1, newdata = data.frame(x = seq(from = 1, to = 4, by = 0.05)))))

# Combine expected and observed 
fire.exp.obs1 <- rbind(cbind(fire.obs, data.frame(var = c(rep("Observed", times = 6)))),
                       cbind(fire.exp1, data.frame(var = c(rep("Expected", times = 6)))))
fire.exp.obs2 <- rbind(cbind(fire.obs.pred, data.frame(var = c(rep("Observed", times = 51)))),
                       cbind(fire.exp2, data.frame(var = c(rep("Expected", times = 51)))))
ui.exp.obs1 <- rbind(cbind(ui.obs, data.frame(var = c(rep("Observed", times = 4)))),
                     cbind(ui.exp1, data.frame(var = c(rep("Expected", times = 4)))))
ui.exp.obs2 <- rbind(cbind(ui.obs.pred, data.frame(var = c(rep("Observed", times = 61)))),
                     cbind(ui.exp2, data.frame(var = c(rep("Expected", times = 61)))))

fire.change <- data.frame(x = c(1:6), y = fire.obs$y - fire.exp1$y)
ui.change <- data.frame(x = c(1:4), y = ui.obs$y - ui.exp1$y)

# Plot charts
ggplot(data = fire.obs, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "cyan3", colour = "black") +
  geom_line(data = fire.exp2, aes(x = x, y = y), size = 1, colour = "#D55E00") +
  geom_point(data = fire.exp1, size = 2, colour = "#D55E00") +
  labs(x = "Degree of reburn (number of fires)", y = expression(paste("Area (", km^2, ")"))) +
  scale_x_continuous(breaks = 1:7) +
  geom_tile(aes(x = 5, y = 13750), width = 2.2, height = 3000, colour = "black", fill = "white", size = .5) +
  annotate("text", x = c(5,5), y = c(14500, 13000), label = c("Expected", "Observed"), size = 10 / 2.5, colour = c("#D55E00", "cyan3"), fontface = "bold") +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12),
        plot.margin = margin(l=0, r = 1.5, unit = "mm"))
ggsave(filename = "ObsExp_Fire_Line.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 85, height = 70, units = "mm", dpi = 300)

ggplot(data = fire.change, aes(x = x,  y = y, colour = factor(1), fill = factor(y))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 1) +
  annotate("text", x = 1:6, y = fire.change$y + 30 , label = round(fire.change$y, 1), size = 10 / 2.5) +
  scale_fill_manual(values=c("#543005", "#f5f5f5","#c7eae5", "#80cdc1", "#35978f", "#01665e")) +
  scale_colour_manual(values= rep("black", 6)) +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Degree of reburn (number of fires)", y = expression(paste("Obs. area - Exp. area ( ", km^2, ")"))) +
  guides(colour = F, fill = F) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12))
ggsave(filename = "ObsExp_Fire_Change.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 85, height = 70, units = "mm", dpi = 300)

ggplot(data = ui.obs, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "cyan3", colour = "black") +
  geom_line(data = ui.exp2, aes(x = x, y = y), size = 1.2, colour = "#D55E00") +
  geom_point(data = ui.exp1, size = 4, colour = "#D55E00") +
  geom_tile(aes(x = 3.5, y = 7000), width = 1.45, height = 1450, colour = "black", fill = "white", size = .5) +
  annotate("text", x = c(3.5,3.5), y = c(7350, 6650), label = c("Expected", "Observed"), size = 10 / 2.5, colour = c("#D55E00", "cyan3"), fontface = "bold") +
  labs(x = "Degree of Persistence\n(number of fires)", y = expression(paste("Area (", km^2, ")"))) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12),
        plot.margin = margin(t= 1, l= -1, r= 1, b = 0, unit = "mm"))
ggsave(filename = "ObsExp_UI_Line.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 85, height = 70, units = "mm", dpi = 300)

ggplot(data = ui.change, aes(x = x,  y = y, colour = factor(1), fill = factor(y))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 1) +
  ylim(-590, 50) +
  annotate("text", x = 1:4, y = c(50, -580, -100, -40), label = round(ui.change$y, 1), size = 10 / 2.5) +
  scale_fill_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#01665e")) +
  scale_colour_manual(values= rep("black", 4)) +
  labs(x = "Degree of persistence\n(number of fires)", y = expression(paste("Obs. area - Exp. area ( ", km^2, ")"))) +
  guides(colour = F, fill = F) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12),
        plot.margin = margin(t= 1, l= -1, r= 1, b = 0, unit = "mm"))
ggsave(filename = "ObsExp_UI_Change.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 85, height = 70, units = "mm", dpi = 300)

# Combined Shape plots
p1 <- ggplot(ui.data, aes(x=AREA, colour = lvl)) +
  geom_density(adjust = 4.5, size = .5) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=lvl),  size = .5, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  geom_vline(xintercept=0, colour="white", size=.5) +
  geom_hline(yintercept=0, colour="white", size=.5) +
  scale_x_continuous(breaks = seq(0, 10000, 2500), limits=c(0, 10000)) +
  labs(colour = "Degree of\nPersistence ", x = expression(paste("Area (", m^2, ")")), y = "Density function (# of pixels)") +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12),
        plot.margin = margin(b = 2, l = 8, r = 1, unit = "mm"))

p2 <- ggplot(ui.data, aes(x=FRAC, colour = lvl)) +
  geom_density(adjust = 3, size = .5) +
  geom_vline(data = med, aes(xintercept = FRAC.med, colour=lvl),  size = .5, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  geom_hline(yintercept=0, colour="white", size=.5) +
  coord_cartesian(xlim = c(1, 1.15))  +
  labs(colour = "Degree of Persistence ", x = "Fractional dimension index", y = "Density function (# of pixels)\n") +
  annotate("text", y = 33, x = 1.052, label = paste(sprintf('\u2190'), "More simple"), size = 10/2.5) +
  annotate("text", y = 33, x = 1.092, label = paste("More complex", sprintf('\u2192')), size = 10/2.5) +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12),
        plot.margin = margin(t = 2, l = 8, r = 1, unit = "mm"))

pL <- get_legend(p1 + theme(legend.text = element_text(size=10),legend.title=element_text(size=12), legend.position="bottom",
                            legend.margin = margin(l = 20, unit = "mm")))

p3 <- plot_grid(p1 + theme(legend.position="none", axis.title.y = element_blank()),
                p2 + theme(legend.position="none", axis.title.y = element_blank()),
                pL,
                nrow = 3, rel_heights = c(1,1,.4), labels = c("A", "B", ""), vjust = 0, hjust = -4)

p4 <- ggdraw(p3) + annotate("text", y = .5, x = 0.018, label = "Density function (# of pixels)", angle = 90, size = 12/2.5)

ggsave(plot = p4, filename = "Shape_combined2.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 130, units = "mm", dpi = 300)


ggsave(plot = pL, filename = "pL.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 130, units = "mm", dpi = 300)
