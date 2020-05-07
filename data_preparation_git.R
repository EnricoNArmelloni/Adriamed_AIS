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
yr<-"2017"

#### Import grids
grid_whole <- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/SHAPEFILES/fra")%>%dplyr::mutate(fra=ifelse(is.na(fra)==TRUE, "D", fra))%>%dplyr::mutate(FID =seq(1:nrow(.)))%>%dplyr::select(FID, FID_1, fra)
harbours <- read_sf(dsn = "med_harb_gsa_b1km", layer = "med_harb_gsa_b1km") %>%st_set_crs(st_crs(grid_whole))%>% dplyr::mutate(id=seq(1:nrow(.)))%>%dplyr::select(id, geometry) 
AREA<-read_sf("~/CNR/AIS/Datasets/SHAPEFILES/area_art_pomo") ### Study Area
####


if(yr=="2018"){
  
  barchemese <- read_delim("Gear_by_month.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  sessions<-read_csv("sessions.csv") ## 2018
  segments<-read_csv("segments.csv") ## 2018
  barchemese<-gather(barchemese, value=gear, key=month, -MMSI)%>%arrange(MMSI, month)%>%dplyr::filter(gear=="OTB")%>%dplyr::mutate(month=str_remove(month, "M"))%>%dplyr::mutate(month=as.integer(month))
  
}else if(yr=="2017"){
  load("~/CNR/AIS/Datasets/SESSIONI/session_2017.RData")
  dataset <- read_csv("new_sessions.csv")
  sessions<-df2017%>%bind_rows(., dataset)
  segments<-read_csv("segments_trawlers17.csv") ## 2017
  
}
  
#####
# Format session data by merging date time information
sess<-sessions%>%dplyr::mutate(starttime =as.POSIXct(paste(startdata, starttime), format='%Y-%m-%d %H:%M:%S', tz="UTC"))%>% dplyr::mutate(month=lubridate::month(startdata), endm=lubridate::month(enddata))%>%dplyr::select(MMSI, session, starttime, month,departure, arrival, gsa_departure, gsa_arrival, country_departure, country_arrival)

# Format segments data by adjusting time format
segments<-segments%>%dplyr::mutate(ret=ifelse(trattov==1 & duration <30, "N", "Y"))%>%dplyr::filter(ret=="Y")%>%dplyr::select(-ret)

seg<-segments%>%dplyr::mutate(finish_time=as.POSIXct(f_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'))%>%  dplyr::mutate(start_time=as.POSIXct(s_time*60, origin="1970-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S')) %>% dplyr::select(-X1, -id,-query, -f_time, -s_time, -n_pings)

# Merge and calculate number of day at sea by segment

if(yr=="2018"){
segments_otb<-left_join(barchemese, sess,  by=c("MMSI", "month")) %>% inner_join( .,seg, by=c("MMSI", "session")) %>%dplyr::mutate(day = as.numeric(round((finish_time-starttime)/86400)))

}else if(yr=="2017"){
  
  segments_otb<-sess%>% inner_join( .,seg, by=c("MMSI", "session")) %>%dplyr::mutate(day = as.numeric(round((finish_time-starttime)/86400)))
  
}

# Transform in sf object 
segments_otb<-segments_otb %>%dplyr::mutate(geo=st_as_sfc(structure(geom, class = "WKB"), EWKB = TRUE))%>% st_as_sf(.)%>% st_set_crs(st_crs(grid_whole))%>%dplyr::mutate(segm_id= seq(1:nrow(.)))

# Intersection with harbours
area<-as.data.frame(st_intersects(AREA, segments_otb))%>%dplyr::rename("FID"="row.id", "segm_id"="col.id")
segments_otb<-segments_otb %>% dplyr::filter(segm_id %in% area$segm_id)%>%dplyr::mutate(segm_id= seq(1:nrow(.)))

segments_otb<-segments_otb %>%dplyr::mutate(dist_m=as.numeric(st_length(geo))) %>% dplyr::mutate(speed_kn =((dist_m/(duration*60))*3.6)/1.852) %>%dplyr::mutate(month_obs=month(finish_time)) %>%dplyr::select(MMSI, session, segment, geo,speed_kn, day, trattov,arrival,gsa_arrival, country_arrival, month_obs)%>%dplyr::mutate(segm_id= seq(1:nrow(.))) ## Formatting data

####
### Save shp
#save(grid_fishingdata, file="grid_fishingdata.RData") 
dir.create(file.path("./",paste0( "segm_input",yr) ))
st_write(segments_otb,  paste0("./","segm_input",yr,"/", "segm_input", yr, ".shp")) #shapefile

