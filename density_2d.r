path <- file.path(normalizePath(".."), "Desktop", "PAD-R", "PD3")
setwd(path)

source("read_data_density.R")

# Tworzymy mapê na 2014
# ---------------
dens <- kde2d.weighted(density.2d.2014$start_station_longitude, 
                       density.2d.2014$start_station_latitude, 
                       w=density.2d.2014$count, 
                       n=1000)
dfdensity <- data.frame(expand.grid(x=dens$x,
                                    y=dens$y),
                        z=as.vector(dens$z))

map1 <- ggmap(map) +
  geom_contour(data=dfdensity, 
               aes(x=x, y=y, z=z, colour=stat(level)),
               bins=12) +
  geom_point(data=density.2d.2014, aes(x=start_station_longitude, y=start_station_latitude)) +
  ggtitle("Density of start stations 2014")
map1

# ---------------

# Tworzymy mapê na 2018
# ---------------
dens <- kde2d.weighted(density.2d.2018$start_station_longitude, 
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
  geom_point(data=density.2d.2018, aes(x=start_station_longitude, y=start_station_latitude)) +
  ggtitle("Density of start stations 2018")
# ---------------
# Wspólny wykres
# ---------------
grid.arrange(map1, map2, nrow=1)


