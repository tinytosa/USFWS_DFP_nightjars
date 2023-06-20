#explore GIS layers for nightjar project

############# 
#load packages

require(sf)
require(raster)

############# 
#load gis layers

AF_states <- st_read(dsn="../GIS", layer="AF_states") 
BCR <- read_sf(dsn="../GIS", layer="BCR_AF_clip")

plot(AF_states) plot(BCR)
