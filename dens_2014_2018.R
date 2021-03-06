library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(ggmap)
library(leaflet)
library(shiny)

theme_set(theme_bw())

lat <- c(40.675, 40.775)
long <- c(-74.02, -73.94)
bbox <- make_bbox(long,lat)
map <- get_map(bbox,maptype="toner-lite",source="stamen")
density.2d.2014 <- read.csv("dane/density_2d_2014.csv", header=TRUE)
density.2d.2018 <- read.csv("dane/density_2d_2018.csv", header=TRUE)

# Tworzymy mape na 2014
# ---------------
dens <- ggtern::kde2d.weighted(density.2d.2014$start_station_longitude, 
                       density.2d.2014$start_station_latitude, 
                       w=density.2d.2014$count, 
                       n=1000)
dfdensity <- data.frame(expand.grid(x=dens$x,
                                    y=dens$y),
                        z=as.vector(dens$z))

map1 <- ggmap(map) +
  geom_contour(data=dfdensity, 
               aes(x=x, y=y, z=z, col=stat(level)),
               bins=12) +
  geom_point(data=density.2d.2014, aes(x=start_station_longitude, y=start_station_latitude), size = 0.9) +
  labs(x = "Długość geograficzna", y = "Szerokość geograficzna", col = "Natężenie")
map1

# ---------------

# Tworzymy mape na 2018
# ---------------
dens <- ggtern::kde2d.weighted(density.2d.2018$start_station_longitude, 
                       density.2d.2018$start_station_latitude, 
                       w=density.2d.2018$count, 
                       n=1000)
dfdensity <- data.frame(expand.grid(x=dens$x,
                                    y=dens$y),
                        z=as.vector(dens$z))

map2 <- ggmap(map) +
  geom_contour(data=dfdensity, 
               aes(x=x, y=y, z=z, colour=stat(level)),
               bins=12) +
  geom_point(data=density.2d.2018, aes(x=start_station_longitude, y=start_station_latitude), size = 0.9) +
  labs(x = "Długość geograficzna", y = "Szerokość geograficzna", col = "Natężenie")
# ---------------
# Wspólny wykres
# ---------------
ggtern::grid.arrange(map1, map2, nrow=1, top = "Częstotliwość użytkowania stacji początkowych w 2014 i 2018 roku")
