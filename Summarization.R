# Setup
setwd("U:/Refugia/Persistance/")
library(raster)
library(rgdal)
library(reshape2)
library(ggplot2)
library(plotrix)


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

# Fire boundary statistics
fire.area <- sum(fire.data$Area)

# Shape indices
ui.data$FRAC <- 2 * log(.25 * ui.data$Perim) / log(ui.data$AREA) # Fractal dimension index
ui.data$PARA <- ui.data$Perim / ui.data$Area # Perimeter-Area Ratio

# Partition
ui.1 <- ui.data[ui.data$overlap == 1,]
ui.2 <- ui.data[ui.data$overlap == 2,]
ui.3 <- ui.data[ui.data$overlap == 3,]
ui.4 <- ui.data[ui.data$overlap == 4,]

# Summary stats: http://www.umass.edu/landeco/research/fragstats/documents/Metrics/Shape%20Metrics/SHAPE%20METRICS.htm

# Proportion across all levels of persistance
prop.area.fire <- sum(ui.1$Area, ui.2$Area, ui.3$Area, ui.4$Area) / sum(fire.data$Area)
prop.area.unburned <- sum(ui.1$Area, ui.2$Area, ui.3$Area, ui.4$Area) / sum(ui.data$Area)

# Count
count <- c(nrow(ui.1), nrow(ui.2), nrow(ui.3), nrow(ui.4))
count.prop.persist <- c(NA, count[2:4]) / sum(count[2:4])
count.prop.unburned <- count / nrow(ui.data)
stats.count <- data.frame(rbind(count, count.prop.unburned, count.prop.persist))
colnames(stats.count) <- c("UI 1", "UI 2", "UI 3", "UI 4")

# Area
area.prop.fire <- c(sum(ui.1$Area), sum(ui.2$Area), sum(ui.3$Area), sum(ui.4$Area)) / sum(fire.data$Area)
area.prop.unburned <- c(sum(ui.1$Area), sum(ui.2$Area), sum(ui.3$Area), sum(ui.4$Area)) / sum(ui.data$Area)
area.prop.persist <- c(NA, sum(ui.2$Area), sum(ui.3$Area), sum(ui.4$Area)) / (sum(ui.data$Area) - sum(ui.1$Area))
area.min <- c(min(ui.1$Area), min(ui.2$Area), min(ui.3$Area), min(ui.4$Area))
area.max <- c(max(ui.1$Area), max(ui.2$Area), max(ui.3$Area), max(ui.4$Area))
area.mean <- c(mean(ui.1$Area), mean(ui.2$Area), mean(ui.3$Area), mean(ui.4$Area))
area.median <- c(median(ui.1$Area), median(ui.2$Area), median(ui.3$Area), median(ui.4$Area))
area.sd <- c(sd(ui.1$Area), sd(ui.2$Area), sd(ui.3$Area), sd(ui.4$Area))
area.se <- area.sd / sqrt(count)
stats.area <- data.frame(rbind(area.prop.fire, area.prop.unburned, area.prop.persist, area.min, area.max, area.mean, area.median, area.sd))
colnames(stats.area) <- c("UI 1", "UI 2", "UI 3", "UI 4")

# Shape index summary
FRAC.mean <- c(mean(ui.1$FRAC), mean(ui.2$FRAC), mean(ui.3$FRAC), mean(ui.4$FRAC))
FRAC.sd <- c(sd(ui.1$FRAC), sd(ui.2$FRAC), sd(ui.3$FRAC), sd(ui.4$FRAC))
FRAC.se <- FRAC.sd / sqrt(count)
FRAC.AreaWtMean <- c(sum(ui.1$FRAC * (ui.1$Area / sum(ui.1$Area))),
                     sum(ui.2$FRAC * (ui.2$Area / sum(ui.2$Area))),
                     sum(ui.3$FRAC * (ui.3$Area / sum(ui.3$Area))),
                     sum(ui.4$FRAC * (ui.4$Area / sum(ui.4$Area))))
PARA.mean <- c(mean(ui.1$PARA), mean(ui.2$PARA), mean(ui.3$PARA), mean(ui.4$PARA))
PARA.sd <- c(sd(ui.1$PARA), sd(ui.2$PARA), sd(ui.3$PARA), sd(ui.4$PARA))
PARA.se <- PARA.sd / sqrt(count)
PARA.AreaWtMean <- c(sum(ui.1$PARA * (ui.1$Area / sum(ui.1$Area))),
                     sum(ui.2$PARA * (ui.2$Area / sum(ui.2$Area))),
                     sum(ui.3$PARA * (ui.3$Area / sum(ui.3$Area))),
                     sum(ui.4$PARA * (ui.4$Area / sum(ui.4$Area))))
stats.shape <- data.frame(rbind(FRAC.mean, FRAC.sd, FRAC.AreaWtMean, PARA.mean, PARA.sd, PARA.AreaWtMean))
colnames(stats.shape) <- c("UI 1", "UI 2", "UI 3", "UI 4")

# Construct dataframe
stats <- data.frame(rbind(stats.count, stats.area, stats.shape))
colnames(stats) <- c("UI 1", "UI 2", "UI 3", "UI 4")
#write.csv(stats, "Persitsent_stats.csv")

# Convert data to ggplot
stats.plot <- data.frame(Overlap = c(1,2,3,4), cbind(count, count.prop.unburned, count.prop.persist, area.prop.fire, area.prop.unburned, 
            area.prop.persist, area.min, area.max, area.mean, area.median, area.sd, area.se, FRAC.mean, 
            FRAC.sd, FRAC.se, FRAC.AreaWtMean, PARA.mean, PARA.sd, PARA.se, PARA.AreaWtMean))

# Proportional Plots
prop.plot <- stats.plot[-1, c(1,4,7)]
colnames(prop.plot) <- c("Overlap", "By count", "By area")
prop.plot.melt <- melt(prop.plot, id.vars = "Overlap")

ggplot(aes(y = value, x = variable, fill = factor(Overlap)), data = prop.plot.melt) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0.95, 1)) +
  labs(title = "Distribution of persistent unburned islands by degree of persistence", x = "", y = "Proportion") +
  scale_fill_manual(values=c("#78c679", "#31a354", "#006837")) +
  guides(fill=guide_legend(title="Degree of\npersistence"))

# Count proportion
count.plot <- stats.plot[,c(1,3,4)]
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
area.plot <- stats.plot[, c(1,5:7)]
area.plot <- rbind(c("Burned", 1- sum(area.plot$area.prop.fire), NA, NA), area.plot)
colnames(area.plot) <- c("Overlap", "Within fire perimeter", "Among all unburned", "Among persistent unburned")
area.plot.melt <- melt(area.plot, id.vars =  "Overlap")
area.plot.melt$value <- as.numeric(area.plot.melt$value)
area.plot.melt$Overlap <- factor(area.plot.melt$Overlap, levels = c("Burned", "1", "2", "3", "4"))

ggplot(aes(y = value, x = variable, fill = Overlap), data = area.plot.melt) +
  geom_bar(position = position_stack(reverse = T), stat="identity") +
  coord_cartesian(ylim = c(0.85, 1)) +
  labs(title = "Distribution of unburned islands (by area) by degree of persistence", x = "", y = "Proportion by area") +
  scale_fill_manual(values=c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"))

# Shape Plots
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

#FRAC K-S test
ks.test(ui.1$FRAC, ui.2$FRAC)
ks.test(ui.1$FRAC, ui.3$FRAC)
ks.test(ui.1$FRAC, ui.4$FRAC)
ks.test(ui.2$FRAC, ui.3$FRAC)
ks.test(ui.2$FRAC, ui.4$FRAC)
ks.test(ui.3$FRAC, ui.4$FRAC) # no difference

# PARA K-S test
ks.test(ui.1$PARA, ui.2$PARA)
ks.test(ui.1$PARA, ui.3$PARA)
ks.test(ui.1$PARA, ui.4$PARA)
ks.test(ui.2$PARA, ui.3$PARA)
ks.test(ui.2$PARA, ui.4$PARA)
ks.test(ui.3$PARA, ui.4$PARA) # no difference

# Area K-S test
ks.test(ui.1$AREA, ui.2$AREA)
ks.test(ui.1$AREA, ui.3$AREA)
ks.test(ui.1$AREA, ui.4$AREA)
ks.test(ui.2$AREA, ui.3$AREA)
ks.test(ui.2$AREA, ui.4$AREA)
ks.test(ui.3$AREA, ui.4$AREA) # no difference

# Perim K-S test
ks.test(ui.1$Perim, ui.2$Perim)
ks.test(ui.1$Perim, ui.3$Perim)
ks.test(ui.1$Perim, ui.4$Perim)
ks.test(ui.2$Perim, ui.3$Perim)
ks.test(ui.2$Perim, ui.4$Perim)
ks.test(ui.3$Perim, ui.4$Perim) # no difference
dunn.test(ui.data$Perim, ui.data$Unburned)

#ggplot(stats.plot, aes(x=Overlap, y=PARA.mean)) + 
#  geom_col(fill = c("#132B43", "#28547A", "#3E81B7", "#56B1F7")) +
#  geom_errorbar(aes(ymin = PARA.mean - PARA.se, ymax = PARA.mean + PARA.se), width = .1, 
#                position = position_dodge(.9)) +
#  labs(title = "Perimeter-area ratio (PARA) by degree of persistance", x = "Degree of persistance (number of fires)", y = "PARA")


colour = c("#132B43", "#28547A", "#3E81B7", "#56B1F7")

#
breaks <- 
prop.plot.melt2 <- prop.plot.melt
prop.plot.melt2$mask <- c(1, 0, 0, 1, 0, 0)
ggplot(aes(y = value, x = variable, fill = factor(Overlap)), data = prop.plot.melt2) +
  geom_bar(position = position_fill(reverse = T), stat="identity") +
  #coord_cartesian(ylim = c(0.80, 1)) +
  labs(title = "Distribution of persistent unburned islands by degree of persistence", x = "", y = "Proportion") +
  scale_fill_manual(values=c("#78c679", "#31a354", "#006837")) +
  guides(fill=guide_legend(title="Degree of\npersistence")) +
  facet_grid(mask~., scales = "free") +
  expand_limits()

#
ggplot(ui.data, aes(x=Perim, colour = factor(overlap))) + 
  geom_density(position = "identity", fill = NA, size = 1) +
  scale_x_continuous(breaks = seq(0, 1000, 250), limits=c(0, 1000)) +
  labs(title = "Unburned island perimeter (m) distribution by degree of persistance", x = "Perimeter length (m)", y = "Density", colour = "Degree of\npersistence")
