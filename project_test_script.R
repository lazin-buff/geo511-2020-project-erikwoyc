library(tidyverse)
library(ggplot2)
library(ggmap)
library(maptools)
library(ggthemes)
library(rgeos)
library(broom)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(sp)
library(sf)
library(rgdal)
library(RColorBrewer)

# 2017 - 2019 Buffalo Assessment Roll
Parcel17 <- read.csv(file = "~/Desktop/Geo511 Spatial Data Science/geo511-2020-project-erikwoyc/2017-2018_Assessment_Roll.csv")
SingleFam_propclass <- c("210", "215", "240", "241", "250", "270")
Buffalo_17 <- filter(Parcel17, PROPERTY.CLASS %in% SingleFam_propclass)

# 2019 - 2020 Buffalo Assessment Roll
Parcel20 <- read.csv(file = "~/Desktop/Geo511 Spatial Data Science/geo511-2020-project-erikwoyc/2019-2020_Assessment_Roll.csv")
SingleFam_propclass <- c("210", "215", "240", "241", "250", "270")
Buffalo_20 <- filter(Parcel20, PROPERTY.CLASS %in% SingleFam_propclass)

# Neighborhood Shapefile
Neighborhood_URL <- "https://data.buffalony.gov/api/geospatial/q9bk-zu3p?method=export&format=GeoJSON"
Buffalo_Neighborhoods <- st_read(dsn = Neighborhood_URL)
Buffalo_sp <- as_Spatial(Buffalo_Neighborhoods)

# 2017 - 2018 Single Family Housing Price Histogram
Plot_2017 <- ggplot(data = Buffalo_17, mapping = aes(x = TOTAL.VALUE)) + 
  geom_histogram() + xlab("Total Property Value($)") + ylab("Count") +
  scale_fill_manual(values="lightblue") + theme_few() +
  labs(x="Total Value($)", y="Count", title="Distribution of Buffalo Home Prices",
       subtitle="Single Family Property Prices (2019 - 2020)", 
       caption="Source: Buffalo Open Data") + scale_x_continuous() + scale_y_continuous()
plot(Plot_2017)

# 2019 - 2020 Single Family Housing Price Histogram
Plot_2019 <- ggplot(data = Buffalo_20, mapping = aes(x = TOTAL.VALUE)) + 
  geom_histogram() + xlab("Total Property Value($)") + ylab("Count") +
  scale_fill_manual(values="lightblue") + theme_few() +
  labs(x="Total Value($)", y="Count", title="Distribution of Buffalo Home Prices",
       subtitle="Single Family Property Prices (2019 - 2020)", 
       caption="Source: Buffalo Open Data") + scale_x_continuous() + scale_y_continuous()
plot(Plot_2019)

#Data is transformed using Natural Log for Map Visualization
Buffalo_17$logprice <- log(Buffalo_17$TOTAL.VALUE)
Buffalo_20$logprice <- log(Buffalo_20$TOTAL.VALUE)

#Buffalo Bounding Box
Buffalo_bbox <- Buffalo_sp@bbox

# Download the basemap
basemap <- get_stamenmap(
  bbox = Buffalo_bbox,
  zoom = 13,
  maptype = "toner-lite")

# View Map
BFMap <- ggmap(basemap) + 
  labs(title="Buffalo Basemap")
BFMap


# 2017 - 2018 Assessment Roll Plot
SingleFam17 <- ggmap(basemap) + 
  geom_point(data = Buffalo_17, aes(x = LONGITUDE, y = LATITUDE, color = logprice, 
             size = .05, alpha = 0.6)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title="Distribution of Buffalo Home Prices",
       subtitle="Property Prices (2017 - 2018)",
       caption="Open Data Buffalo")
SingleFam17

# 2019 - 2020 Assessment Roll Plot
SingleFam20 <- ggmap(basemap) + 
  geom_point(data = Buffalo_20, aes(x = LONGITUDE, y = LATITUDE, color = logprice, 
             size = .25, alpha = 0.6)) +
  scale_fill_brewer(palette = "Greens") +
  labs(title="Distribution of Buffalo Home Prices",
       subtitle="Property Prices (2019 - 2020)",
       caption="Open Data Buffalo")
SingleFam20


