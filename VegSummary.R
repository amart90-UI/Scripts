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
veg <- ddply(vegsamp[, c("FBFM", "UI", "ForRan")], c("FBFM", "UI", "ForRan"), summarise, count = length(FBFM))
veg$FBFM <- as.factor(veg$FBFM)

#p_veg <- 
  ggplot(veg, aes(x = FBFM, y = count, fill = UI)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  #facet_wrap(~ForRan)+
  labs(x = "Fuel Type", y = "Frequency", colour = "Burn Status") +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_text(size=12),legend.position=c(.85,.8), 
        plot.margin = margin(t= 2, l = .5, r = 1, b = 1, unit = "mm"))

ggsave(plot = p_veg, filename = "Veg_bars.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)



tbl <- table(vegsamp$FBFM, vegsamp$UI)
chisq.test(tbl)
n <- colSums(tbl)
colSums(tbl)
####

table(vegsamp$ForRan, vegsamp$UI)
