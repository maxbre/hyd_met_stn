library(tidyverse)
library(lubridate)

#####################################################################################################################
# HYDRO

# list names of hydro files but just with csv extension
hyd_f <- list.files('./raw_data/hydro', "*\\.csv$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# map files to hydro_df
# read_csv2 because files are using ; for field separation
# cleaning up of redundant variables
hyd_df <- map_df(hyd_f, 
                 read_delim,
                 delim=";",
                 trim_ws = TRUE, # delete the empty spaces
                 locale = locale(decimal_mark = ".", encoding = "latin1"),
                 na = ">>") %>%
  mutate(datetime = dmy_hms(paste(giorno, ora)),
         valore = medio) %>%
  select(-c(giorno, ora, minimo, medio, massimo))%>%
  rename_with(tolower)

# read data about hyd stn
xyz_hyd <- read_rds('../coord_stns/input/hyd_stn.rds') %>%
  select(codice, nome, comune, quota, x, y)
  
# left join data
hyd_df <- left_join(hyd_df, xyz_hyd, by = c("codstz" = "codice"))

# compose file name of hydro station
hyd_stn_name <- paste0("hyd_stn_", unique(hyd_df$codstz))

# export as rds for use in further elaborations
write_rds(hyd_df, paste0("./input/", hyd_stn_name, "_1h.rds"))

# export munged original data 1h
write_csv(hyd_df, paste0("./output/data_export/", hyd_stn_name, "_1h.csv"))

#####################################################################################################################
# METEO

# more or less the same operations as for hydro
prec_f <- list.files('./raw_data/meteo/prec', "*\\.csv$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# list files
temp_f <- list.files('./raw_data/meteo/temp', "*\\.csv$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# pretty much same as above but with some workaround
# because precipitation and temp are in different format (for the joy of the programmer)
prec_df <- map_df(prec_f, 
                  read_delim,
                  delim=";",
                  trim_ws = TRUE, # delete the empty spaces
                  locale = locale(decimal_mark = ".", encoding = "latin1"),
                  na = ">>") %>%
  mutate(datetime = dmy_hms(paste(giorno, ora)), .before = valore) %>%
  select(-c(giorno, ora))%>%
  rename_with(tolower)


temp_df <- temp_df <- map_df(temp_f, 
                 read_delim,
                 delim=";",
                 trim_ws = TRUE, # delete the empty spaces
                 locale = locale(decimal_mark = ".", encoding = "latin1"),
                 na = ">>") %>%
  mutate(datetime = dmy_hms(paste(giorno, ora)),
         valore = medio) %>%
  select(-c(giorno, ora, minimo, medio, massimo))%>%
  rename_with(tolower)

# row bind two previous dfs in a single met df
met_df <- bind_rows(prec_df, temp_df)

# read data about met stn
xyz_met <- read_rds('../coord_stns/input/met_stn.rds') %>%
  select(codice, nome, comune, quota, x, y)

# left join data
met_df <- left_join(met_df, xyz_met, by = c("codstz" = "codice"))

# compose file name of met station
met_stn_name <- paste0("met_stn_", unique(met_df$codstz))

# export as rds for use in further elaborations
write_rds(met_df, paste0("./input/", met_stn_name, "_1h.rds"))

# export munged original data 1h
write_csv(met_df, paste0("./output/data_export/", met_stn_name, "_1h.csv"))

