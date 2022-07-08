
# delete raw data in hydro
stn_xxx_rw_hyd<-list.files('./raw_data/hydro', full.names = TRUE)
unlink(stn_xxx_rw_hyd)

# delete raw data in meteo
stn_xxx_rw_met_prec<-list.files('./raw_data/meteo/prec', full.names = TRUE)
unlink(stn_xxx_rw_met_prec)

# delete raw data in temp
stn_xxx_rw_met_temp<-list.files('./raw_data/meteo/temp', full.names = TRUE)
unlink(stn_xxx_rw_met_temp)
