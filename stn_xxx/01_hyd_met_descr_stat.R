library(tidyverse)
library(lubridate)

#####################################################################################################################
# HYDRO

# list name of hydro file rds
hyd_f <- list.files('./input', "hyd.*\\.rds$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# import rds 1h
hyd_df <- read_rds(hyd_f)

# compose file name of hydro station
hyd_stn_name <- paste0("hyd_stn_", unique(hyd_df$codstz))

# export descriptive stat by year

hyd_df %>%
  group_by(anno = year(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
            ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", hyd_stn_name, "_y.csv"))

# export descriptive stat by year, month

hyd_df %>%
  group_by(anno = year(datetime), mese = month(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
            ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", hyd_stn_name, "_ym.csv"))

# export descriptive stat by year, semester

hyd_df %>%
  group_by(anno = year(datetime), semestre = semester(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
            ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", hyd_stn_name, "_ys.csv"))

# export descriptive stat by year, quarter

hyd_df %>%
  group_by(anno = year(datetime), trimestre = quarter(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
            ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", hyd_stn_name, "_yq.csv"))


#####################################################################################################################
# MET
# list name of hydro file rds
met_f <- list.files('./input', "met.*\\.rds$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# import rds 1h
met_df <- read_rds(met_f)

# compose file name of hydro station
met_stn_name <- paste0("met_stn_", unique(met_df$codstz))

# export descriptive stat by year

met_df %>%
  group_by(anno = year(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
  ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", met_stn_name, "_y.csv"))

# export descriptive stat by year, month

met_df %>%
  group_by(anno = year(datetime), mese = month(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
  ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", met_stn_name, "_ym.csv"))

# export descriptive stat by year, semester

met_df %>%
  group_by(anno = year(datetime), semestre = semester(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
  ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", met_stn_name, "_ys.csv"))

# export descriptive stat by year, quarter

met_df %>%
  group_by(anno = year(datetime), trimestre = quarter(datetime), sensore = tiposens)%>%
  summarise(recs = n(),
            min = min(valore, na.rm=TRUE),
            mean = mean(valore, na.rm=TRUE),
            p25 = quantile(valore, 0.25, na.rm=TRUE),
            median = median(valore, na.rm = TRUE),
            p75 = quantile(valore, 0.25, na.rm=TRUE),
            p98 = quantile(valore, 0.98, na.rm=TRUE),
            max = max(valore, na.rm=TRUE),
            dev_st = sd(valore, na.rm=TRUE)
  ) %>%
  write_csv(paste0("./output/stat/", "stat_descr_", met_stn_name, "_yq.csv"))

