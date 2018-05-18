setwd("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/VegSampling")
library(plyr)
library(ggplot2)
library(reshape2)

LookUp <- read.csv("S:/COS/PyroGeog/amartinez/Persistance/Intermediates/LandCover/GapAttrib.csv")
vegsamp <- read.csv("VegSamp.csv")
vegsamp$UI[vegsamp$UI == 2] <- "Burned"
vegsamp$UI[vegsamp$UI == 5] <- "Persistent Unburned"
Reclass <- data.frame(old = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                     new.c = c(1,2,2,2,3,2,3,3,2,3,3,2),
                     new = c("Forest","Range","Range","Range","Other","Range","Other","Other","Range","Other","Other","Range"))
#"Forest & Woodland", "Shrubland & Grassland", "Semi-Desert", 
#"Polar & High Montane Vegetation", "Nonvascular & Sparse Vascular Rock Vegetation", 
#"Agricultural Vegetation", "Introduced & Semi Natural Vegetation", 
#"Recently Disturbed or Modified", "Open Water", "Developed & Other Human Use"



vegsamp$ForRan <- LookUp$CL[match(unlist(vegsamp$LCvr.Gap), LookUp$Value)]
vegsamp$ForRan <- Reclass$new[match(unlist(vegsamp$ForRan), Reclass$old)]

vegsamp <- vegsamp[! vegsamp$FBFM %in% c(91, 93, 98, 99) , ]
vegsamp <- vegsamp[! vegsamp$ForRan == "Other", ]
vegsamp <- na.omit(vegsamp)
vegsamp <- vegsamp[vegsamp$FBFM < 14, ]
vegsamp <- vegsamp[-sample((1:nrow(vegsamp))[vegsamp$UI == "Burned"], sum(vegsamp$UI == "Burned") - sum(vegsamp$UI == "Persistent Unburned"), replace = F),]
veg <- ddply(vegsamp[, c("FBFM", "UI", "ForRan")], c("FBFM", "UI", "ForRan"), summarise, count = length(FBFM))
FBFM.names <- c("(1) Short grass", "(2) Timber and grass", "(3) Tall grass", "(4) Chaparral", "(5) Brush", "(6) Dominant brush", "(7) Southern rough", 
                "(8) Closed, timber litter", "(9) Hardwood litter", "(10) Timber litter\nw/ understory", "(11) Light slash", "(12) Medium Slash", "(13) Heavy slash")
FBFM.reclass <- data.frame(old = 1:13, new = as.factor(FBFM.names))
veg$FBFM2 <- FBFM.reclass$new[match(unlist(veg$FBFM), FBFM.reclass$old)]
veg$FBFM2 <- factor(veg$FBFM2, levels =  FBFM.names)
col <- c("#d8b365", "cyan3")
rect1 <- data.frame(xmin = -Inf, xmax = 3.5, ymin = -Inf, ymax = Inf)
rect2 <- data.frame(xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf)
rect3 <- data.frame(xmin = 6.5, xmax = 10.5, ymin = -Inf, ymax = Inf)
rect4 <- data.frame(xmin = 10.5, xmax = Inf, ymin = -Inf, ymax = Inf)

p_veg <- 
  ggplot(veg, aes(x = FBFM2, y = count, fill = UI)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  labs(x = " Fuel Type", y = "Frequency (# of pixles)", colour = "Burn Status") +
  scale_fill_manual(labels = c("Burned   ", "Persistent\nUnburned"), values = col) +
  geom_rect(data = rect1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, color = "black", inherit.aes = F) +
  geom_rect(data = rect2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, color = "black", inherit.aes = F) +
  geom_rect(data = rect3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, color = "black", inherit.aes = F) +
  geom_rect(data = rect4, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, color = "black", inherit.aes = F) +
  annotate("text", x = c(2, 5, 8.5, 12), y = 6300, label = c("Grass dominated", "Shrub dominated", "Timber litter", "Logging slash"), size = 10/2.5) +
  coord_cartesian(ylim = c(0,6500)) +
  theme_classic() +
  theme(axis.text.y=element_text(size=10, colour = "black"), axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, margin = margin(b = -8), colour = "black"), axis.title=element_text(size=12), 
          legend.text = element_text(size=9),legend.title=element_blank(),legend.position=c(.9185,.55), legend.box.background = element_rect(colour = "black", size = 1),
        plot.margin = margin(t= 2, l = 0, r = 0.5, b = 0, unit = "mm"), legend.margin = margin(t=-2, b=3, l = 2, r = 2), axis.title.y = element_text(hjust = 1))

ggsave(plot = p_veg, filename = "Veg_bars.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

tbl <- table(vegsamp$FBFM, vegsamp$UI)
chisq.test(tbl)
n <- colSums(tbl)
sum(tbl)
####
#panel.background = element_rect(fill = 'white', colour = "black")
