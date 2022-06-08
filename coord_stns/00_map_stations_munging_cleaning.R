library(tidyverse)

#####################################################################################################################
# HYDRO

hyd_stn <- read_delim('./raw_data/CoordStazioni_hydro.csv', 
                      delim=";",
                      locale = locale(decimal_mark = ".", encoding = "latin1"))%>%
  select(-c(Fine, Bacino, SottoBacino, ZonaAllerta))%>%
  drop_na(starts_with("Epsg3003"))%>% # dropping rows with no coords in "Epsg3003"
  rename_with(tolower) %>%
  select(-c("epsg4258_lat", "epsg4258_lon" ))%>%
  rename(x = epsg3003_x, y = epsg3003_y)


# set name of rds file
rds_hyd <- paste0('./input/', "hyd_stn.rds")

# export as rds
write_rds(hyd_stn, rds_hyd)

#####################################################################################################################
# METEO

met_stn <- read_delim('./raw_data/CoordStazioni_meteo.csv',
                      locale = locale(decimal_mark = ".",
                                      encoding = "latin1"))%>%
  select(-c(Fine, Bacino, SottoBacino, ZonaAllerta))%>%
  drop_na(starts_with("Epsg3003"))%>% # dropping rows with no coords in "Epsg3003"
  rename_with(tolower) %>%
  select(-c("epsg4258_lat", "epsg4258_lon" ))%>%
  rename(x = epsg3003_x, y = epsg3003_y)

# set name of rds file
rds_met <- paste0('./input/', "met_stn.rds")

# export as rds
write_rds(met_stn, rds_met)
