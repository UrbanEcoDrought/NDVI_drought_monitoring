# Making an NDVI panel to go with Lindsay's pretty graphical abstract
# What we want is something simplified that shows that not all landcovers respond to drought evenly
# So lets pull forest (dark green?) + Urban Medium (orange?) and see how it looks
# Normal NDVI = solid line; drought year = dashed

library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)


Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/Manuscript - Urban Drought NDVI Monitoring by Land Cover Class/figures")


# Loading in data frames
yrs <- read.csv(file.path(google.drive, "data/NDVI_drought_monitoring/k=12_individual_years_post_GAM_with_forest-wet.csv")) #individual years
yrs <- yrs[yrs$type!="forest",]
yrs$type[yrs$type=="forest-wet"] <- "forest"
yrs$type <- factor(yrs$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))

norms <-read.csv(file.path(google.drive, "data/NDVI_drought_monitoring/k=12_norms_all_LC_types_with_wet-forest.csv")) #normals
norms <- norms[norms$type!="forest",]
norms$type[norms$type=="forest-wet"] <- "forest"
norms$type <- factor(norms$type, levels = c("crop", "forest", "grassland", "urban-open", "urban-low", "urban-medium", "urban-high"))


summary(yrs)

summary(norms)

# Attempt number 1
ggplot(data=norms[norms$type %in% c("urban-medium", "forest"),]) +
  # geom_ribbon(aes(x=yday, ymin=lwr, ymax=upr, fill=type)) +
  geom_line(aes(x=yday, y=mean, color=type, linewidth="normal"), alpha=0.5) +
  geom_line(data=yrs[yrs$type %in% c("urban-medium", "forest") & yrs$year==2005,], aes(x=yday, y=mean, color=type, linewidth="drought"),) +
  scale_linetype_manual(values=c("normal"="solid", "drought"="dashed"))


norms$YrType <- "normal"
dfPlot <- rbind(norms[norms$type %in% c("urban-medium", "forest"),c("mean", "lwr", "upr", "yday", "type", "YrType")],
                cbind(yrs[yrs$type %in% c("urban-medium", "forest") & yrs$year==2005,c("mean", "lwr", "upr", "yday", "type")], data.frame("YrType"=rep("drought", 365*2))))

dfPlot$type <- car::recode(dfPlot$type, "'urban-medium'='urban'")
dfPlot$PlotGroup <- paste(dfPlot$type, dfPlot$YrType, sep="-")
dfPlot$PlotGroup <- factor(dfPlot$PlotGroup, levels=c("forest-normal", "forest-drought", "urban-normal", "urban-drought"))
head(dfPlot)

seasonLabels <- data.frame(season=c("Spring", "Summer", "Autumn"), yday=lubridate::yday(c("2005-05-25", "2005-08-06", "2005-11-06"))-c(20,0,0), NDVI=0.95)

png(file.path(pathShare, "Abstract-NDVI-simple.png"), width=6, height=2.75, units="in", res=320)
ggplot(data=dfPlot) +
  coord_cartesian(xlim=c(lubridate::yday(c("2005-03-20","2005-12-21")))) +
  geom_line(aes(x=yday, y=mean, color=PlotGroup), linewidth = 2) +
  scale_color_manual(name="NDVI Pattern", values=c("forest-normal"="#4dac26", "forest-drought"="#b8e186", "urban-normal"="#d01c8b", "urban-drought"="#f1b6da")) +
  # geom_vline(xintercept=c(lubridate::yday(c("2005-05-25", "2005-08-06", "2005-11-06")))) + # season midpoints 
  geom_vline(xintercept=c(lubridate::yday(c("2005-03-20", "2005-06-21", "2005-09-22", "2005-12-21"))), linetype="dashed") + # season startpoints 
  # geom_text(data=seasonLabels, aes(x=yday, y=NDVI, label = season)) +
  scale_y_continuous(name="NDVI", limits=c(0,1)) +
  scale_x_continuous(name="Time of Year") +
  theme_classic() +
  theme(legend.position="bottom", 
        axis.text=element_blank(),
        axis.ticks=element_blank())
dev.off()
 