#script made by  dr. Massimo Bressan
#(ARPAV, Environment Regional Agency of Veneto, Italy) on feb 2022


####################################
#DATA PREPARATION
####################################

#library load

library(tidyverse)
library(lubridate)
library(patchwork)
library(knitr)
library(kableExtra)

# list files (check data with txt extension on the predefined path)
myfiles<-list.files('./raw_data', "*\\.txt$", full.names = TRUE, ignore.case = TRUE, include.dirs = TRUE)

# map df (data frame maps of data obtained from the previous command)
mydf<-map_df(myfiles,
       read_delim,
       trim_ws=TRUE, # delete the empty spaces
       locale = locale(decimal_mark = "."),
       na=">>")%>%

#this is necessary for data formatting and its stratification)
  mutate(date=dmy_h(paste(DATA, ORA)),
                  anno=year(date),
                  mese=month(date, label = TRUE),
                  giorno=day(date),
                  ora=hour(date),
                  trimestre=quarter(date),
                  semestre=semester(date))

# here write down the title of all your graphs. For different titles, 
# write a different one into
# the ggtitle("write your title here"); same for label x and y axis

ggtitle <- "stazione 0131 Sandrigo via Chilesotti"
xlab <- "tempo"
ylab <- "m slm"

###########################################
#BASIC STATISTICS TABLES
###########################################

# basic statistics output for all years grouped by year
mydf%>%
  group_by(anno)%>%
  summarise(n=n(),
            mean=mean(VALORE, na.rm=TRUE),
            max=max(VALORE, na.rm=TRUE),
            min=min(VALORE, na.rm=TRUE),
            dev_st=sd(VALORE, na.rm=TRUE),
            P98=quantile(VALORE, 0.98, na.rm=TRUE))%>%
 knitr::kable(format ="pipe")

# basic statistics output for all years 
mydf%>%
  summarise(n=n(),
            mean=mean(VALORE, na.rm=TRUE),
            max=max(VALORE, na.rm=TRUE),
            min=min(VALORE, na.rm=TRUE),
            dev_st=sd(VALORE, na.rm=TRUE),
            P98=quantile(VALORE, 0.98, na.rm=TRUE))%>%
 knitr::kable(format ="pipe")

# basic statistics output by year - 2019
mydf%>%
  filter(anno==2019)%>%
  group_by(SENSORE, anno, trimestre)%>%
  summarise(n=n(),
            mean=mean(VALORE, na.rm=TRUE),
            max=max(VALORE, na.rm=TRUE),
            min=min(VALORE, na.rm=TRUE),
            dev_st=sd(VALORE, na.rm=TRUE),
            P98=quantile(VALORE, 0.98, na.rm=TRUE))%>%
  knitr::kable(format ="pipe")

# basic statistics output by year - 2020
mydf%>%
  filter(anno==2020)%>%
  group_by(SENSORE, anno)%>%
  summarise(n=n(),
            mean=mean(VALORE, na.rm=TRUE),
            max=max(VALORE, na.rm=TRUE),
            min=min(VALORE, na.rm=TRUE),
            dev_st=sd(VALORE, na.rm=TRUE),
            P98=quantile(VALORE, 0.98, na.rm=TRUE))%>%
 knitr::kable(format ="pipe")

# basic statistics output by year - 2021
mydf%>%
  filter(anno==2021)%>%
  group_by(SENSORE, anno, trimestre)%>%
  summarise(n=n(),
            mean=mean(VALORE, na.rm=TRUE),
            max=max(VALORE, na.rm=TRUE),
            min=min(VALORE, na.rm=TRUE),
            dev_st=sd(VALORE, na.rm=TRUE),
            P98=quantile(VALORE, 0.98, na.rm=TRUE))%>%
 knitr::kable(format ="pipe")

###################################
#BOXPLOTS FOR BASIC STATISTICS
###################################

#boxplot for water level
bpl<-mydf%>% #boxplot (groundwater) level
  filter(str_detect(SENSORE, 'Livello'))%>%
  ggplot(mapping = aes(x=mese, y=VALORE))+
  geom_boxplot()+
  ylab("metri slm")+
  xlab("mese")+
  ggtitle(paste(ggtitle, " - boxplot livello/mese"))+
  facet_grid(rows=vars(anno))

bpl

ggsave("./output/boxplot_livello_anno.png")

#boxplot for water temperature: draw the boxplot by month
bptw<-mydf%>% #boxplot temperature (ground)water
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  ggplot(mapping = aes(x=factor(trimestre), y=VALORE))+
  geom_boxplot()+
  ylab("temp Celsius")+
  xlab("trimestre")+
  ggtitle(paste(ggtitle, " - boxplot temperatura falda/mese"))+
  facet_grid(rows=vars(anno))

bptw

ggsave("./output/boxplot_Temperatura_acqua.png")

#boxplot for air temperature: draw the boxplot by month
bpta<-mydf%>% #boxplot temperature air
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  ggplot(mapping = aes(x=factor(trimestre), y=VALORE))+
  geom_boxplot()+
  ylab("temp Celsius")+
  xlab("trimestre")+
  ggtitle(paste(ggtitle, " - boxplot temperatura aria/mese"))+
  facet_grid(rows=vars(anno))

bpta

ggsave("./output/boxplot_Temperatura_aria.png")

########################################
# HEATMAPS
########################################

library(viridis)
library(ggExtra)

#level heat maps
hml<-mydf%>% #heatmap (growndwater) level
  filter(str_detect(SENSORE, 'Livello'))%>%
  ggplot(aes(giorno,ora, fill=VALORE))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Livello freatimetrico [m]",option ="D")+
  facet_grid(anno~mese)+
  scale_y_continuous(trans = "reverse", breaks = unique(mydf$ora))+
  scale_x_continuous(breaks =c(1,10,20,31))+
  theme_minimal(base_size = 8)+
  ylab("ora")+
  xlab("giorno")+
  ggtitle(paste(ggtitle, " - livello falda (heat map)"))+
  theme(legend.position = "bottom",
        plot.title=element_text(size = 14, hjust=0),
        axis.text.y=element_text(size=6),
        strip.background = element_rect(colour="white"),
        axis.ticks=element_blank(),
        axis.text=element_text(size=7),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))+
  removeGrid()

hml

ggsave("./output/heatmap_level.jpg")

#temperature heat maps
hmwt<-mydf%>% #heatmap grownd(water) temperature
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  ggplot(aes(giorno,ora, fill=VALORE))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="temp Celsius", option ="C")+
  facet_grid(anno~mese)+
  scale_y_continuous(trans = "reverse", breaks = unique(mydf$ora))+
  scale_x_continuous(breaks =c(1,10,20,31))+
  theme_minimal(base_size = 8)+
  ylab("ora")+
  xlab("giorno")+
  ggtitle(paste(ggtitle, " - temperatura falda (heat map)"))+
  theme(legend.position = "bottom",
        plot.title=element_text(size = 14, hjust=0),
        axis.text.y=element_text(size=6),
        strip.background = element_rect(colour="white"),
        axis.ticks=element_blank(),
        axis.text=element_text(size=7),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))+
  removeGrid()

hmwt

ggsave("./output/heatmap_temp.jpg")


##########################
#DISTRIBUTION MAPS BY MONTH
#########################
library(ggridges)

#elevation distribution by month
livdis<-l1h_p24h%>% #(groundwater) level distribution
  ggplot(aes(x=VALORE, y=mese.x))+
  geom_density_ridges(rel_min_height = 0.01) +
  ylab("mesi")+
  xlab("m slm")+
  ggtitle(paste(ggtitle, " - escursione falda/mese"))+
  #theme_ridges() + 
  facet_grid(~anno.x)

livdis

ggsave("./output/liv_distrib.png")

#temperature distribution by month
temdis<-mydf%>% #(groundwater) temperature distribution
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  ggplot(aes(x=VALORE, y=mese))+
  geom_density_ridges(rel_min_height = 0.01) +
  ylab("mesi")+
  xlab("temp Celsius")+
  xlim(13,18)+
  ggtitle(paste(ggtitle, " - escursione temp/mese"))+
  facet_grid(~anno)

temdis

ggsave("./output/temp_distrib.png")


###########################################
#RAINFALL, GROUNDWATER LEVEL AND TEMPERATURE
###########################################

############
#RAINFALL
############

#plot rainfall (precipitazioni) - all years
p24h<-mydf%>% #rainfall as day cumulative values at 23.00 
  filter(str_detect(SENSORE, 'Precipitazione'))%>%
  group_by(anno, mese, giorno) %>%
  summarise(
    sum_prec = sum(VALORE, na.rm = TRUE),
    n = n())%>%
  mutate(date=ymd_hms(paste(paste(anno, mese, giorno, sep="-"),
                            "23:00:00")))

p<-l1h_p24h%>%  #rainfall
  ggplot(aes(x=date))+
  geom_col(aes(y=sum_prec), col="brown")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("mm/giorno")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - precipitazioni"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

p

ggsave("./output/rainfall_all.png")

#plot rainfall (precipitazioni) - 2019
p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Precipitazione'))%>%
  filter(str_detect(SENSORE, 'Precipitazione'),
         anno==2019)%>%
  group_by(anno, mese, giorno) %>%
  summarise(
    sum_prec = sum(VALORE, na.rm = TRUE),
    n = n())%>%
  mutate(date=ymd_hms(paste(paste(anno, mese, giorno, sep="-"),
                            "23:00:00")))


p2019<-l1h_p24h%>%
  ggplot(aes(x=date))+
  geom_col(aes(y=sum_prec), col="brown")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("mm/giorno")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - precipitazioni"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

p2019

ggsave("./output/rainfall_2019.png")

#plot rainfall (precipitazioni) - 2020
p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Precipitazione'))%>%
  filter(str_detect(SENSORE, 'Precipitazione'),
         anno==2020)%>%
  group_by(anno, mese, giorno) %>%
  summarise(
    sum_prec = sum(VALORE, na.rm = TRUE),
    n = n())%>%
  mutate(date=ymd_hms(paste(paste(anno, mese, giorno, sep="-"),
                            "23:00:00")))


p2020<-l1h_p24h%>%
  ggplot(aes(x=date))+
  geom_col(aes(y=sum_prec), col="brown")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("mm/giorno")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - precipitazioni"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

p2020

ggsave("./output/rainfall_2020.png")


#plot rainfall (precipitazioni) - 2021
p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Precipitazione'))%>%
  filter(str_detect(SENSORE, 'Precipitazione'),
         anno==2021)%>%
  group_by(anno, mese, giorno) %>%
  summarise(
    sum_prec = sum(VALORE, na.rm = TRUE),
    n = n())%>%
  mutate(date=ymd_hms(paste(paste(anno, mese, giorno, sep="-"),
                            "23:00:00")))

p2021<-l1h_p24h%>%
  ggplot(aes(x=date))+
  geom_col(aes(y=sum_prec), col="brown")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("mm/giorno")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - precipitazioni"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

p2021

ggsave("./output/rainfall_2021.png")


############
#GROUNDWATER LEVEL
############
#plot level (livello) - all years
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Livello'))%>%
  left_join(p24h, by="date")

l<-l1h_p24h%>%
  filter(str_detect(SENSORE, 'Livello'))%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark blue")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("m slm")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - livello falda"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

l

ggsave("./output/gwlevel_all.png")

#plot level (livello) - 2019
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Livello'))%>%
  left_join(p24h, by="date")

l19<-l1h_p24h%>%
  filter(anno.x==2019)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark blue")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("m slm")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - livello"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

l19
ggsave("./output/gwlevel_2019.png")

#plot level (livello) - 2020
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Livello'))%>%
  left_join(p24h, by="date")

l20<-l1h_p24h%>%
  filter(anno.x==2020)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark blue")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("m slm")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - livello"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

ggsave("./output/gwlevel_2020.png")


#plot level (livello) - 2021
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Livello'))%>%
  left_join(p24h, by="date")

l21<-l1h_p24h%>%
  filter(anno.x==2021)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark blue")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("m slm")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - livello"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

l21

ggsave("./output/gwlevel_2021.png")


############
#GROUNDWATER TEMPERATURE
############
#plot temperature (temperatura) - all years

l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  left_join(p24h, by="date")

gwt<-l1h_p24h%>% #GroundWater Temperature
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="red")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura falda"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

gwt

ggsave("./output/gwtemperaure_all.png")

#plot temperature (temperatura) - 2019
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  left_join(p24h, by="date")

gwt19<-l1h_p24h%>% #GroundWater Temperature
  filter(anno.x==2019)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="red")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura falda"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

gwt19

ggsave("./output/gwtemperaure_2019.png")

#plot temperature (temperatura) - 2020
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  left_join(p24h, by="date")

gwt20<-l1h_p24h%>% #GroundWater Temperature
  filter(anno.x==2020)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="red")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura falda"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

gwt20

ggsave("./output/gwtemperaure_2020.png")

#plot temperature (temperatura) - 2021
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura acqua'))%>%
  left_join(p24h, by="date")

gwt21<-l1h_p24h%>% #GroundWater Temperature
  filter(anno.x==2021)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="red")+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura falda"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

gwt21

ggsave("./output/gwtemperaure_2021.png")

############
#AIR TEMPERATURE
############
#plot air temperature (temperatura) - all years
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  left_join(p24h, by="date")

at<-l1h_p24h%>% #Air Temperature
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark grey", size=0.05)+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura aria"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

at

ggsave("./output/air_temperaure_all.png")

#plot air temperature (temperatura) - 2019
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  left_join(p24h, by="date")

at19<-l1h_p24h%>% #Air Temperature
  filter(anno.x==2019)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark grey", size=0.05)+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura aria"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

at19

ggsave("./output/airtemperaure_2019.png")

#plot air temperature (temperatura) - 2020
l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  left_join(p24h, by="date")

at20<-l1h_p24h%>% #Air Temperature
  filter(anno.x==2020)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark grey", size=0.05)+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura aria"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

at20

ggsave("./output/airtemperaure_2020.png")

#plot air temperature (temperatura) - 2021
library(forecast)

l1h_p24h<-mydf%>%
  filter(str_detect(SENSORE, 'Temperatura aria'))%>%
  left_join(p24h, by="date")

at21<-l1h_p24h%>% #Air Temperature
  filter(anno.x==2021)%>%
  ggplot(mapping = aes(x=date, y=VALORE))+
  geom_line(color="dark grey", size=0.05)+
  scale_x_datetime(date_breaks= "4 week", date_labels = "%d-%b-%y" )+
  theme_light()+
  ylab("temp Celsius")+
  xlab(xlab)+
  ggtitle(paste(ggtitle, " - temperatura aria"))+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

at21

ggsave("./output/airtemperaure_2021.png")