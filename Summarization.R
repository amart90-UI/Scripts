# Setup
setwd("U:/Refugia/Persistance/")
library(raster)
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
area.stats$prop.persist <- area.stats$prop.persist / (sum(ui.data$Area) - sum(ui.1$Area))
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
#write.csv(stats, "Persitsent_stats.csv")
#read.csv("Persitsent_stats.csv")

# Proportional Plots - this one might not be great
prop.plot <- stats[-1, c(1,4,7)]
colnames(prop.plot) <- c("Overlap", "By count", "By area")
prop.plot.melt <- melt(prop.plot, id.vars = "Overlap")

ggplot(aes(y = value, x = variable, fill = factor(Overlap)), data = prop.plot.melt) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0.95, 1)) +
  labs(title = "Distribution of persistent unburned islands by degree of persistence", x = "", y = "Proportion") +
  scale_fill_manual(values=c("#78c679", "#31a354", "#006837")) +
  guides(fill=guide_legend(title="Degree of\npersistence"))

# Count proportion
count.plot <- stats[,c(1,3,4)]
colnames(count.plot) <- c("Overlap", "Among all unburned", "Among persistent unburned only")
count.plot.melt <- melt(count.plot, id.vars = "Overlap")
count.plot.melt$value <- as.numeric(count.plot.melt$value)
count.plot.melt$Overlap <- factor(count.plot.melt$Overlap, levels = c("1", "2", "3", "4"))

ggplot(aes(y = value, x = variable, fill = Overlap), data = count.plot.melt) +
  geom_bar(position = position_stack(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0.85, 1)) +
  labs(title = "Distribution of unburned islands (by count) by degree of persistence", x = "", y = "Proportion by count") +
  scale_fill_manual(values=c("#c2e699", "#78c679", "#31a354", "#006837")) +
  guides(fill=guide_legend(title="Degree of\npersistence"))

# Area proportion
area.plot <- stats[, c(1,5:7)]
area.plot <- rbind(c("Burned", 1- sum(area.plot$area.prop.fire), NA, NA), area.plot)
colnames(area.plot) <- c("Overlap", "Within fire perimeter", "Among all unburned", "Among persistent unburned only")
area.plot.melt <- melt(area.plot, id.vars =  "Overlap")
area.plot.melt$value <- as.numeric(area.plot.melt$value)
area.plot.melt$Overlap <- factor(area.plot.melt$Overlap, levels = c("Burned", "1", "2", "3", "4"))

ggplot(aes(y = value, x = variable, fill = Overlap), data = area.plot.melt) +
  geom_bar(position = position_stack(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0.85, 1)) +
  labs(title = "Distribution of unburned islands (by area) by degree of persistence", x = "", y = "Proportion by area") +
  scale_fill_manual(values=c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"))

# Faceted shape Plots - maybe not these ones
ggplot(ui.data, aes(x=Perim)) + 
  geom_density(fill = "darkgreen", alpha = 0.2) +
  facet_wrap(~overlap) +
  labs(title = "Unburned island perimeter (m) distribution by degree of persistance", x = "Perimeter length (m)", y = "Density") +
  xlim(c(0, 1000))
ggplot(ui.data, aes(x=AREA)) + 
  geom_density(fill = "darkgreen", alpha = 0.2) +
  facet_wrap(~overlap) +
  labs(title = "Area (sq. m) distribution by degree of persistance", x = "Area (m)", y = "Density") +
  xlim(c(0, 20000))
ggplot(ui.data, aes(x=PARA)) + 
  geom_density(fill = "darkgreen", alpha = 0.2) +
  facet_wrap(~overlap) +
  labs(title = "Perimeter-area ratio (PARA) distribution by degree of persistance", x = "PARA", y = "Density")
ggplot(ui.data, aes(x=FRAC)) + 
  geom_density(fill = "darkgreen", alpha = 0.2) +
  facet_wrap(~overlap) +
  labs(title = "Fractional dimension index (FRAC) distribution by degree of persistance", x = "FRAC", y = "Density") +
  xlim(c(1,1.2))
##

med <- ddply(ui.data, "overlap", summarise, # calculate medians
             AREA.med = median(AREA), PARA.med = median(PARA), FRAC.med = median(FRAC))
med$overlap <- factor(med$overlap)
##

# Stacked shape plots
ggplot(ui.data, aes(x=AREA, colour = factor(overlap))) + 
  geom_density(position = "identity", fill = NA, size = 1) +
  geom_vline(data = med, aes(xintercept = AREA.med, colour=overlap), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 15000, 2500), limits=c(0, 15000)) +
  labs(title = "Unburned island area (sq. m) distribution by degree of persistance", x = "Area (sq. m)", y = "Density", colour = "Degree of\npersistence")
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

#FRAC K-S test
a <- round(c(ks.test(ui.1$FRAC, ui.2$FRAC)$p.value,
       ks.test(ui.1$FRAC, ui.3$FRAC)$p.value,
       ks.test(ui.1$FRAC, ui.4$FRAC)$p.value,
       ks.test(ui.2$FRAC, ui.3$FRAC)$p.value,
       ks.test(ui.2$FRAC, ui.4$FRAC)$p.value,
       ks.test(ui.3$FRAC, ui.4$FRAC)$p.value), digits = 4)
b <- c("-", a[1:3], "-", "-", a[4:5], "-", "-", "-", a[6])
FRAC.summary <- data.frame(matrix(b, ncol = 4, nrow = 3, byrow = T))
colnames(FRAC.summary) <- c(1:4)

# PARA K-S test
a <- round(c(ks.test(ui.1$PARA, ui.2$PARA)$p.value,
             ks.test(ui.1$PARA, ui.3$PARA)$p.value,
             ks.test(ui.1$PARA, ui.4$PARA)$p.value,
             ks.test(ui.2$PARA, ui.3$PARA)$p.value,
             ks.test(ui.2$PARA, ui.4$PARA)$p.value,
             ks.test(ui.3$PARA, ui.4$PARA)$p.value), digits = 4)
b <- c("-", a[1:3], "-", "-", a[4:5], "-", "-", "-", a[6])
PARA.summary <- data.frame(matrix(b, ncol = 4, nrow = 3, byrow = T))
colnames(PARA.summary) <- c(1:4)

# Area K-S test
a <- round(c(ks.test(ui.1$AREA, ui.2$AREA)$p.value,
             ks.test(ui.1$AREA, ui.3$AREA)$p.value,
             ks.test(ui.1$AREA, ui.4$AREA)$p.value,
             ks.test(ui.2$AREA, ui.3$AREA)$p.value,
             ks.test(ui.2$AREA, ui.4$AREA)$p.value,
             ks.test(ui.3$AREA, ui.4$AREA)$p.value), digits = 4)
b <- c("-", a[1:3], "-", "-", a[4:5], "-", "-", "-", a[6])
AREA.summary <- data.frame(matrix(b, ncol = 4, nrow = 3, byrow = T))
colnames(AREA.summary) <- c(1:4)


