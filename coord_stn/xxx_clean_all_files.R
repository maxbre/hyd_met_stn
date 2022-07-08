
# delete raw data in coord_stn
coord_stn_rw<-list.files('./raw_data', full.names = TRUE)
unlink(coord_stn_rw)

# delete input data in coord_stn
coord_stn_in<-list.files('./input', full.names = TRUE)
unlink(coord_stn_in)

# delete output data in coord_stn
coord_stn_ou<-list.files('./output', full.names = TRUE)
unlink(coord_stn_ou, recursive = TRUE)
