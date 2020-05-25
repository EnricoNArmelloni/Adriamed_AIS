##########
##
## Code to format initial dataset
##
## Enrico Nicola Armelloni - enrico.armelloni@irbim.cnr.it
##
## May 2020
##
##########


library(sf)
library(sp)
library(tidyverse)
library(data.table)
"%ni%"<-Negate("%in%")
setwd("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Input_data")
yr<-"2018"

#### Import grids
grid_whole <- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/SHAPEFILES/MEDUNITS_OTB17_fra")%>%dplyr::mutate(fra=ifelse(is.na(fra)==TRUE, "D", fra))%>%dplyr::mutate(FID =seq(1:nrow(.)))%>%dplyr::select(FID, FID_1, fra)
harbours <- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/GitHub/MED_UNITS/SHAPEFILES/med_harb_gsa_b1km", layer = "med_harb_gsa_b1km") %>%st_set_crs(st_crs(grid_whole))%>% dplyr::mutate(id=seq(1:nrow(.)))%>%dplyr::select(id, geometry) 
AREA<-read_sf("~/CNR/AIS/Datasets/SHAPEFILES/area_art_pomo") ### Study Area
####


if(yr=="2018"){
  
  barchemese <- read_delim("Gear_by_month.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  sessions<-read_csv("sessions_18.csv") ## 2018
  segments<-read_csv("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Ricontrollo/segments_raw_1718.csv") ## 2018
  barchemese<-gather(barchemese, value=gear, key=month, -MMSI)%>%arrange(MMSI, month)%>%dplyr::filter(gear=="OTB")%>%dplyr::mutate(month=str_remove(month, "M"))%>%dplyr::mutate(month=as.integer(month))
  # Format session data by merging date time information
  sess<-sessions%>%dplyr::mutate(starttime =as.POSIXct(paste(startdata, starttime), format='%Y-%m-%d %H:%M:%S', tz="UTC"), month=lubridate::month(startdata))%>%dplyr::select(MMSI, session, starttime,departure, arrival, gsa_departure, gsa_arrival, country_departure, country_arrival, month)
  seg<-segments%>%dplyr::mutate(finish_time=as.POSIXct(f_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'))%>%  dplyr::mutate(start_time=as.POSIXct(s_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'))%>%dplyr::filter(lubridate::year(finish_time)==2018) %>% dplyr::select( -id,-query, -f_time, -s_time, -n_pings)
  
}else if(yr=="2017"){
  #barchemese <- read_delim("Gear_by_month2017.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  #barchemese<-gather(barchemese, value=gear, key=month, -MMSI)%>%arrange(MMSI, month)%>%dplyr::filter(gear=="OTB")%>%dplyr::mutate(month=str_remove(month, "M"))%>%dplyr::mutate(month=as.integer(month))
  barchemese <-read_csv("barchemese_17mu.csv")%>%dplyr::rename("month"="month_obs")
  load("~/CNR/AIS/Datasets/SESSIONI/session_2017.RData")
  dataset <- read_csv("~/CNR/AIS/Lavori/Lavori 2020/MED_UNITS/Controlli gennaio/Altre barche/new_sessions.csv")
  sessions<-df2017%>%bind_rows(., dataset)
  #segments<-read_csv("~/CNR/AIS/Lavori/Lavori 2020/MED_UNITS/Data/Summary/segments_MEDUNITS_trawlers.csv") ## 2017
  segments<-read_csv("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Ricontrollo/segments_raw_1718.csv")
  # Format session data by merging date time information
  sess<-sessions%>%dplyr::mutate(starttime =as.POSIXct(paste(startdata, starttime), format='%Y-%m-%d %H:%M:%S', tz="UTC"), month=lubridate::month(startdata))%>%dplyr::select(MMSI, session, starttime,departure, arrival, gsa_departure, gsa_arrival, country_departure, country_arrival, month)
  
 
  seg<-segments%>%dplyr::mutate(finish_time=as.POSIXct(f_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'))%>%  dplyr::mutate(start_time=as.POSIXct(s_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'))%>%dplyr::filter(lubridate::year(finish_time)==2017) %>% dplyr::select( -id,-query, -f_time, -s_time, -n_pings)
  
}

#####
# Days at Sea
segments_otb<-left_join(barchemese, sess,  by=c("MMSI", "month")) %>% inner_join( .,seg, by=c("MMSI", "session")) %>%dplyr::mutate(day = as.numeric(ceiling(abs(difftime(starttime, finish_time, units="days")))))


# Transform in sf object 
segments_otb<-segments_otb %>%dplyr::mutate(geo=st_as_sfc(structure(geom, class = "WKB"), EWKB = TRUE))%>% st_as_sf(.)%>% st_set_crs(st_crs(grid_whole))%>%dplyr::mutate(segm_id= seq(1:nrow(.)))


# Intersection with harbours
area<-as.data.frame(st_intersects(AREA, segments_otb))%>%dplyr::rename("FID"="row.id", "segm_id"="col.id")
segments_otb<-segments_otb %>% dplyr::filter(segm_id %in% area$segm_id)%>%dplyr::mutate(segm_id= seq(1:nrow(.)))

porti<-as.data.frame(st_intersects(harbours, segments_otb))%>%dplyr::rename("FID"="row.id", "segm_id"="col.id")
segments_otb<-segments_otb %>% dplyr::filter(segm_id %ni% porti$segm_id)%>%dplyr::mutate(segm_id= seq(1:nrow(.)))
nrow(segments_otb%>%dplyr::filter(trattov==4))

segments_otb<-segments_otb %>%dplyr::mutate(dist_m=as.numeric(st_length(geo))) %>% dplyr::mutate(speed_kn =((dist_m/(duration*60))*3.6)/1.852) %>%dplyr::mutate(month_obs=month(finish_time)) %>%dplyr::select(MMSI, session, segment, geo,speed_kn, day, trattov,arrival,gsa_arrival, country_arrival, month_obs,duration)%>%dplyr::mutate(segm_id= seq(1:nrow(.))) ## Formatting data


####
### Save shp
#save(grid_fishingdata, file="grid_fishingdata.RData") 
dir.create(file.path("./",paste0( "segm_input",yr) ))
st_write(segments_otb,  paste0("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Ricontrollo/segm_input", yr, ".shp")) #shapefile


