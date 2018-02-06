# Setup
setwd("S:/COS/PyroGeog/amartinez/Persistance")
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
ui.data$Cvr <- rep(1:3, length.out = 816802)
ui.data$overlap <- factor(ui.data$overlap)

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
                    area.persist = sum(AREA))
prop.stats$count.persist <- c(0, prop.stats$count.persist[-1] / sum(prop.stats$count.persist[-1]))
prop.stats$area.persist <- c(0, prop.stats$area.persist[-1] / sum(area.stats$total[-1]))

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
area.prop <- data.frame(overlap = c("Burned", 1,2,3,4,1,2,3,4,2,3,4),
                        values = c(1-sum(prop.stats$area.fire), prop.stats[,5], prop.stats[,6], prop.stats[-1,7]),
                        group = factor(c(rep("Within fire perimeters", times = 5), rep("Among unburned islands", times = 4), 
                                         rep("Among persistant unburned islands", times = 3))))
area.prop$overlap <- factor(as.character(c("Burned", 1,2,3,4,1,2,3,4,2,3,4)), levels = c("Burned", 1,2,3,4))
area.prop$group <- factor(area.prop$group, levels = rev(levels(area.prop$group)))
area.prop$values <- 100 *area.prop$values

ggplot(aes(y = values, x = group, fill = overlap), data = area.prop) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Distribution of persistent unburned islands (area) by degree of persistence", x = "", y = "Proportion of area (%)") +
  guides(fill=guide_legend(title="Degree of\npersistence"))

# Stacked shape plots
med <- ddply(ui.data, "overlap", summarise, # calculate medians
             AREA.med = median(AREA), PARA.med = median(PARA), FRAC.med = median(FRAC))
med$overlap <- factor(med$overlap)

ggplot(ui.data, aes(x=AREA, colour = factor(overlap))) + 
  geom_density(position = "identity", fill = NA, size = 1) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=overlap), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 15000, 2500), limits=c(0, 15000)) +
  labs(title = expression(paste("Unburned island area (", m^2, ") distribution by degree of persistance")), 
       x = expression(paste("Area (", m^2, ")")), y = "Density", colour = "Degree of\npersistence")
ggplot(ui.data, aes(x=PARA, colour = factor(overlap))) + 
  geom_density(position = "identity", fill = NA, size = 1) +
  geom_vline(data = med, aes(xintercept = PARA.med, colour=overlap), linetype = "dashed") +
  labs(title = "Unburned island perimeter area ratio (PARA) distribution by degree of persistance", x = "PARA", y = "Density", colour = "Degree of\npersistence")
ggplot(ui.data, aes(x=FRAC, colour = factor(overlap))) + 
  geom_density(position = "identity", fill = NA, size = 1) +
  geom_vline(data = med, aes(xintercept = FRAC.med, colour=overlap), linetype = "dashed") +
  scale_x_continuous(limits=c(1, 1.25)) +
  labs(title = "Unburned island Fractional Dimension Index (FRAC) distribution by degree of persistance", x = "FRAC", y = "Density", colour = "Degree of\npersistence")

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
PARA.ks <- round(c(ks.test(ui.1$PARA, ui.2$PARA)$p.value,
                   ks.test(ui.1$PARA, ui.3$PARA)$p.value,
                   ks.test(ui.1$PARA, ui.4$PARA)$p.value,
                   ks.test(ui.2$PARA, ui.3$PARA)$p.value,
                   ks.test(ui.2$PARA, ui.4$PARA)$p.value,
                   ks.test(ui.3$PARA, ui.4$PARA)$p.value), digits = 4)
FRAC.ks <- round(c(ks.test(ui.1$FRAC, ui.2$FRAC)$p.value,
                   ks.test(ui.1$FRAC, ui.3$FRAC)$p.value,
                   ks.test(ui.1$FRAC, ui.4$FRAC)$p.value,
                   ks.test(ui.2$FRAC, ui.3$FRAC)$p.value,
                   ks.test(ui.2$FRAC, ui.4$FRAC)$p.value,
                   ks.test(ui.3$FRAC, ui.4$FRAC)$p.value), digits = 4)
shape.ks <- data.frame(Area = Area.ks, PARA = PARA.ks, FRAC = FRAC.ks)

# K-W test
shape.kw <- round(c(kruskal.test(AREA ~ overlap, data = ui.data)$p.value,
                    kruskal.test(PARA ~ overlap, data = ui.data)$p.value,
                    kruskal.test(FRAC ~ overlap, data = ui.data)$p.value), digits = 4)

# Combine statistical testing table
shape.tests <- rbind(shape.ks, shape.kw)
row.names(shape.tests) <- c("1 vs. 2", "1 vs. 3", "1 vs. 4", "2 vs. 3", "2 vs. 4", "3 vs. 4", "Kruskal-Wallis")
