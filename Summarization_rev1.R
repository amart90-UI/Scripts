# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
library(raster)
library(rgdal)
library(reshape2)
library(ggplot2)
library(plyr)

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
                        group = factor(c(rep("Within fire perimeters", times = 6), rep("Among unburned islands", times = 4), 
                                         rep("Among persistant unburned", times = 3), "Among persistant (2+)", "Among persistant (2+)")))
area.prop$group <- factor(area.prop$group, levels = rev(levels(area.prop$group)))
area.prop$values <- 100 * area.prop$values

ggplot(aes(y = values, x = group, fill = overlap), data = area.prop) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  labs(title = "", x = "", y = "Proportion of area") +
  scale_fill_manual("Degree of persistence ", values = col) +
  guides(fill=guide_legend(nrow=2,byrow=F))+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(position = "bottom") +
  theme(axis.ticks.x=element_blank(), axis.text.y=element_text(size=26), axis.title=element_text(size=28), axis.text.x = element_text(size = 24), 
        legend.text = element_text(size=26),legend.title=element_text(size=28), legend.position="top")

c(fire.table$SUM_AREA[1] / 10000, sum(fire.table$SUM_AREA[-1]) / 10000, area.stats$total / 10000)

# Shape KDE plots
med <- ddply(ui.data, "lvl", summarise, # calculate medians
             AREA.med = median(AREA), PARA.med = median(PARA), FRAC.med = median(FRAC))
med$lvl <- factor(med$lvl)

ggplot(ui.data, aes(x=AREA, colour = lvl)) +
  geom_density(adjust = 4.5, size = 1) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=lvl),  size = 1, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  geom_vline(xintercept=c(0, 1), colour="white", size=1) +
  scale_x_continuous(breaks = seq(0, 10000, 2500), limits=c(0, 10000)) +
  labs(colour = "Degree of Persistence ", x = expression(paste("Area (", m^2, ")")), y = "Density function (# of pixels)") +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=26), axis.text.x = element_text(size = 24), axis.title=element_text(size=28), 
        legend.text = element_text(size=26),legend.title=element_text(size=28), legend.position="top")

ggplot(ui.data, aes(x=FRAC, colour = lvl)) +
  geom_density(adjust = 3, size = 1) +
  geom_vline(data = med, aes(xintercept = FRAC.med, colour=lvl),  size = 1, linetype = c("dashed", "dashed", "dashed", "longdash"), show.legend = F) +
  coord_cartesian(xlim = c(1, 1.15))  +
  labs(colour = "Degree of Persistence ", x = "Fractional dimension index", y = "Density function (# of pixels)") +
  annotate("text", y = 33, x = 1.02, label = paste(sprintf('\u2190'), "More simple"), size = 22/2.5) +
  annotate("text", y = 33, x = 1.13, label = paste("More complex", sprintf('\u2192')), size = 22/2.5) +
  guides(colour = guide_legend(nrow=2,byrow=F)) +
  theme(axis.text.y=element_text(size=26), axis.text.x = element_text(size = 24), axis.title=element_text(size=28), 
        legend.text = element_text(size=26),legend.title=element_text(size=28), legend.position="top")

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



########## strat by cover
area.stats.f <- ddply(Forest, "overlap", summarise,
                    total = sum(AREA), 
                    prop.persist = sum(AREA), 
                    min = min(AREA), 
                    max = max(AREA),
                    mean = mean(AREA), 
                    area.median = median(AREA), 
                    sd = sd(AREA), 
                    se = sd(AREA)/sqrt(length(AREA)))
area.stats.f$prop.persist <- c(0, area.stats.f$prop.persist[-1] / sum(area.stats.f$total[-1]))
area.stats.f$Cvr <- rep(1, times = 4)

area.stats.r <- ddply(Range, "overlap", summarise,
                      total = sum(AREA), 
                      prop.persist = sum(AREA), 
                      min = min(AREA), 
                      max = max(AREA),
                      mean = mean(AREA), 
                      area.median = median(AREA), 
                      sd = sd(AREA), 
                      se = sd(AREA)/sqrt(length(AREA)))
area.stats.r$prop.persist <- c(0, area.stats.r$prop.persist[-1] / sum(area.stats.r$total[-1]))
area.stats.r$Cvr <- rep(2, times = 4)

# Proportions
prop.stats.f <- ddply(Forest, "overlap", summarise,
                    area.fire = sum(AREA) / fire.area,
                    area.unburned = sum(AREA) / sum(Forest$AREA),
                    area.persist = sum(AREA))
prop.stats.f$area.persist <- c(0, prop.stats.f$area.persist[-1] / sum(area.stats.f$total[-1]))

prop.stats.r <- ddply(Range, "overlap", summarise,
                      area.fire = sum(AREA) / fire.area,
                      area.unburned = sum(AREA) / sum(Range$AREA),
                      area.persist = sum(AREA))
prop.stats.r$area.persist <- c(0, prop.stats.f$area.persist[-1] / sum(area.stats.f$total[-1]))




area.prop <- data.frame(overlap = c("Burned", 1,2,3,4,1,2,3,4,2,3,4),
                        values = c(1-sum(prop.stats$area.fire), prop.stats[,5], prop.stats[,6], prop.stats[-1,7]),
                        group = factor(c(rep("Within fire perimeters", times = 5), rep("Among unburned islands", times = 4), 
                                         rep("Among persistant unburned islands", times = 3))))
area.prop$overlap <- factor(as.character(c("Burned", 1,2,3,4,1,2,3,4,2,3,4)), levels = c("Burned", 1,2,3,4))
area.prop$group <- factor(area.prop$group, levels = rev(levels(area.prop$group)))
area.prop$values <- 100 *area.prop$values

ggplot(aes(y = values, x = group, fill = overlap), data = area.prop) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  labs(title = "Distribution of persistent unburned islands (area) by degree of persistence", x = "", y = "Proportion of area") +
  guides(fill=guide_legend(title="Degree of\npersistence"))
####
