# Setup
setwd("U:/Refugia/Persistance/")
library(raster)
library(rgdal)
library(reshape2)
library(ggplot2)
library(plyr)

# Load data
fire <- readOGR("Data/perims_dissolve.shp")
fire.data <- fire@data
#ui <- readOGR("Data/ui_overlap.shp")
#ui.data <- ui@data[,-1]
#write.csv(ui.data, "Intermediates/OverlapData.csv")
#ui.data <- read.csv("Intermediates/OverlapData.csv")
ui.data <- read.csv("Intermediates/OverlapLndCvr.csv")
#ui.data <- ui.data[, c(2:5]
ui.data$Cvr <- rep(1:2, length.out = 816802)
ui.data$overlap <- factor(ui.data$overlap)
ui.data$lvl[ui.data$overlap == 1] <- "Unburned for 1 fire  "
ui.data$lvl[ui.data$overlap == 2] <- "Unburned for 2 fires  "
ui.data$lvl[ui.data$overlap == 3] <- "Unburned for 3 fires  "
ui.data$lvl[ui.data$overlap == 4] <- "Unburned for 4 fires  "

Forest <- ui.data[ui.data$Cvr == 1,]
Range <- ui.data[ui.data$Cvr == 2,]

# Fire boundary statistics
fire.area <- sum(fire.data$Area)

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
col <- c("#D55E00", "#f39c12", "#27ae60", "#2980b9", "#8e44ad")
lvl <- c("Burned", "Unburned for 1 fire", "Unburned for 2 fires", "Unburned for 3 fires", "Unburned for 4 fires")

area.prop <- data.frame(overlap = c(lvl, lvl[2:5],lvl[3:5], lvl[4:5]),
                        values = c(1-sum(prop.stats$area.fire), prop.stats[,5], prop.stats[,6], prop.stats[-1,7], prop.stats[3:4,8]),
                        group = factor(c(rep("Within fire perimeters", times = 5), rep("Among unburned islands", times = 4), 
                                         rep("Among persistant unburned islands", times = 3), "Among persistant (2+)", "Among persistant (2+)")))
area.prop$group <- factor(area.prop$group, levels = rev(levels(area.prop$group)))

ggplot(aes(y = values, x = group, fill = overlap), data = area.prop) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  labs(title = "", x = "", y = "Proportion of area") +
  scale_fill_manual("Degree of\npersistence ", values = col) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), legend.position="bottom")

# Stacked shape plots
med <- ddply(ui.data, "overlap", summarise, # calculate medians
             AREA.med = median(AREA), PARA.med = median(PARA), FRAC.med = median(FRAC))
med$overlap <- factor(med$overlap)

ggplot(ui.data, aes(x=FRAC, fill = factor(lvl))) +
  geom_histogram(breaks = seq(from = 1, to = 1.25, by = 0.025)) +
  geom_vline(data = med, aes(xintercept = FRAC.med, colour=overlap), 
             linetype = c("dashed", "dashed", "dashed", "longdash"), size = 1) +
  guides(colour = FALSE, fill = guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom") +
  labs(fill = "Degree of Persistence ", x = "FRAC", y = "Frequency")

ggplot(ui.data, aes(x=AREA, fill = factor(lvl))) +
  geom_histogram(breaks = seq(from = 0, to = 18000, by = 1200)) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=overlap), 
             linetype = c("dashed", "dashed", "dashed", "longdash"), size = 1) +
  guides(colour = FALSE, fill = guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text = element_text(size=18),legend.title=element_text(size=22), 
        strip.text.x = element_text(size = 22), legend.position="bottom") +
  labs(fill = "Degree of Persistence ", x = expression(paste("Area (", m^2, ")")), y = "Frequency")

# Partition data for testing
ui.1 <- ui.data[ui.data$overlap == 1,]
ui.2 <- ui.data[ui.data$overlap == 2,]
ui.3 <- ui.data[ui.data$overlap == 3,]
ui.4 <- ui.data[ui.data$overlap == 4,]

# K-S test
Area.ks <- round(c(ks.test(ui.1$AREA, ui.2$AREA)$p.value,
                   ks.test(ui.1$AREA, ui.3$AREA)$p.value,
                   ks.test(ui.1$AREA, ui.4$AREA)$p.value,
                   ks.test(ui.2$AREA, ui.3$AREA)$p.value,
                   ks.test(ui.2$AREA, ui.4$AREA)$p.value,
                   ks.test(ui.3$AREA, ui.4$AREA)$p.value), digits = 4)
FRAC.ks <- round(c(ks.test(ui.1$FRAC, ui.2$FRAC)$p.value,
                   ks.test(ui.1$FRAC, ui.3$FRAC)$p.value,
                   ks.test(ui.1$FRAC, ui.4$FRAC)$p.value,
                   ks.test(ui.2$FRAC, ui.3$FRAC)$p.value,
                   ks.test(ui.2$FRAC, ui.4$FRAC)$p.value,
                   ks.test(ui.3$FRAC, ui.4$FRAC)$p.value), digits = 4)
shape.ks <- data.frame(Area = Area.ks, FRAC = FRAC.ks)

# K-W test
shape.kw <- round(c(kruskal.test(AREA ~ overlap, data = ui.data)$p.value,
                    kruskal.test(FRAC ~ overlap, data = ui.data)$p.value), digits = 4)

# Combine statistical testing table
shape.tests <- rbind(shape.ks, shape.kw)
row.names(shape.tests) <- c("1 vs. 2", "1 vs. 3", "1 vs. 4", "2 vs. 3", "2 vs. 4", "3 vs. 4", "Kruskal-Wallis")


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


#####
fire.area / 10000
area.stats$total / 10000

col <- c("#D55E00", "#f39c12", "#27ae60", "#2980b9", "#8e44ad")
col <- c("#b33939", "#cd6133", "#cc8e35", "#ffda79", "#f7f1e3")
