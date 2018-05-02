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
#veg <- na.omit(veg)
col <- c("#d8b365", "cyan3")
#p_veg <- 
  ggplot(veg, aes(x = FBFM2, y = count, fill = UI)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  #facet_wrap(~ForRan)+
  labs(x = "Fuel Type", y = "Frequency (# of pixles)", colour = "Burn Status") +
  scale_fill_manual(labels = c("Burned   ", "Persistent\nUnburned"), values = col) +
  theme(axis.text.y=element_text(size=10), axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1, margin = margin(b = -8)), axis.title=element_text(size=12), 
        legend.text = element_text(size=10),legend.title=element_blank(),legend.position=c(.81,.80), legend.box.background = element_rect(colour = "black"),
        plot.margin = margin(t= 2, l = 0, r = 0.5, b = 0, unit = "mm"), axis.title.y = element_text(hjust = 1))

ggsave(plot = p_veg, filename = "Veg_bars.png", path = "C:/Users/PyroGeo/Documents/UI-Drive/UI-Drive/Refugia/Persistence/Plots/",
       width = 170, height = 70, units = "mm", dpi = 300)

tbl <- table(vegsamp$FBFM, vegsamp$UI)
chisq.test(tbl)
n <- sum(colSums(tbl))
sum(tbl)
####
sum(vegsamp$UI == "Burned") - sum(vegsamp$UI == "Persistent Unburned")
sample(vegsamp[,], 100)
sample((1:nrow(vegsamp))[vegsamp$UI == "Burned"], sum(vegsamp$UI == "Burned") - sum(vegsamp$UI == "Persistent Unburned"), replace = F)
v2 <- vegsamp[-sample((1:nrow(vegsamp))[vegsamp$UI == "Burned"], sum(vegsamp$UI == "Burned") - sum(vegsamp$UI == "Persistent Unburned"), replace = F),]
table(vegsamp$UI)
