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

# vec of sens
sens<-unique(hyd_df$tiposens)

# LFREAT
lfreat<-sens[1]

# TFREAT
tfreat<-sens[2]

# set theme as preferred
theme_set(theme_light())

hyd_df %>%
  filter(tiposens==lfreat) %>%
  mutate(quota_ass = quota-valore) %>%
  ggplot(mapping = aes(x = month(datetime, label = TRUE), y = quota_ass, group = month(datetime)))+
  geom_boxplot()+
  labs(x = NULL , y = lfreat )+
  facet_wrap(vars(year(datetime)))

ggsave(paste0("./output/charts/", "boxplot_", hyd_stn_name, "_", lfreat, "_ym.png"),
       width = 16, height = 8, units = "cm")

hyd_df %>%
  filter(tiposens==tfreat) %>%
  ggplot(mapping = aes(x=month(datetime, label = TRUE), y=valore, group = month(datetime)))+
  geom_boxplot()+
  labs(x = NULL , y = tfreat )+
  facet_wrap(vars(year(datetime)))

ggsave(paste0("./output/charts/", "boxplot_", hyd_stn_name, "_", tfreat, "_ym.png"),
       width = 16, height = 8, units = "cm")


### TO BE CONTINUED

#####################################################################################################################
# MET
# list name of hydro file rds
met_f <- list.files('./input', "met.*\\.rds$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# import rds 1h
met_df <- read_rds(met_f)

# compose file name of hydro station
met_stn_name <- paste0("met_stn_", unique(met_df$codstz))


### TO BE CONTINUED