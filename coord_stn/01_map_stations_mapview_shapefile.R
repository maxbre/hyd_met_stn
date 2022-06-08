library(tidyverse)
library(sf)
library(mapview)

#####################################################################################################################
# HYDRO

# read data hyd
hyd_stn <- read_rds('./input/hyd_stn.rds')

# epsg Monte Mario / Italy zone 1
crs <- 3003

# transform df to sf
hyd_stn_sf <- st_as_sf(x = hyd_stn,
         coords = c("x", "y"),
         crs = crs)

# set name of shapefile
shp_hyd <- paste0('./output/', "hyd_stn.shp")

# create shapefile, eventually overwrite it!
st_write(hyd_stn_sf, shp_hyd, delete_layer = TRUE)

# mapview
m_hyd <- mapview(hyd_stn_sf)

# export as html
mapshot(m_hyd, url = paste0('./output/', "mapview_hyd_stn.html"))

#####################################################################################################################
# METEO

# read data hyd
met_stn <- read_rds('./input/met_stn.rds')

# transform df to sf
met_stn_sf <- st_as_sf(x = met_stn,
                       coords = c("x", "y"),
                       crs = crs)

# set name of shapefile
shp_met <- paste0('./output/', "met_stn.shp")

# create shapefile, eventtually overwrite
st_write(met_stn_sf, shp_met, delete_layer = TRUE)

# plot the map
m_met <- mapview(met_stn_sf)

#export as html
mapshot(m_met, url = paste0('./output/', "mapview_met_stn.html"))

