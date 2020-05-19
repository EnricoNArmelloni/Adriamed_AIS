##########
##
## Code to calculate days at sea from points
##
## Enrico Nicola Armelloni - enrico.armelloni@irbim.cnr.it
##
## May 2020
##
##########


library(sf)
library(readxl)
library(sp)
library(tidyverse)
library(data.table)
"%ni%"<-Negate("%in%")
setwd("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Input_data")

#### Import data
grid_whole <- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/SHAPEFILES/MEDUNITS_OTB17_fra")%>%dplyr::mutate(fra=ifelse(is.na(fra)==TRUE, "D", fra))%>%dplyr::mutate(FID =seq(1:nrow(.)))%>%dplyr::select(FID, FID_1, fra)
points_area <- read_csv("points_area.csv")
country_codes<- read_excel("~/CNR/AIS/Lavori/Lavori 2019/GSA_EXCHANGE/GSA-exchange/COUNTRY_CODES.xlsx")
### Periods
t1<-seq(1:8)
t2<-c(9,10)
t3<-c(11,12)
temporal_seq<-list(t1=t1,t2=t2,t3=t3)
## Data format
dataset<- points_area%>% dplyr::mutate(CODE= as.numeric(substr(MMSI, 1,3))) %>% dplyr::left_join(country_codes, by="CODE")%>%dplyr::mutate(datetime =as.POSIXct(paste(DATE.UTC., TIME.UTC.), format='%Y-%m-%d %H:%M:%S', tz="UTC"))%>% dplyr::mutate(month=lubridate::month(datetime))%>%dplyr::rename("DAY"= "DATE.UTC.") %>%dplyr::filter(COUNTRY== "Italy",aut=="NO", SPEED >= 7 ) %>%dplyr::select(MMSI, DAY, month, year, LATITUDE, LONGITUDE) %>% st_as_sf(., coords=c("LONGITUDE", "LATITUDE"))%>% st_set_crs(st_crs(grid_whole))%>% dplyr::mutate(point_id=seq(1:nrow(.)))


### Intersection
int<-as.data.frame(st_intersects(grid_whole, dataset))%>%dplyr::rename("FID"="row.id", "point_id"="col.id")
dataset<-left_join(dataset, int, by="point_id")


#################################### OUTPUT 4
# 2017
dat17<-dataset%>%dplyr::filter(year ==2017)

Days_at_sea_period17<-lapply(temporal_seq, function(i){
  nm<-names(i)
  i<-as.data.frame(dat17%>%dplyr::filter(month %in% i)%>%dplyr::distinct(FID, MMSI, DAY,.keep_all=T)%>%dplyr::group_by(FID)%>%count(name="Days_at_Sea"))%>%dplyr::select(FID, Days_at_Sea)
  return(i)
})

names(Days_at_sea_period17[[1]])<-paste(c("FID", "Days_at_Sea17_t1"))
names(Days_at_sea_period17[[2]])<-paste(c("FID", "Days_at_Sea17_t2"))
names(Days_at_sea_period17[[3]])<-paste(c("FID", "Days_at_Sea17_t3"))

# 2018
dat18<-dataset%>%dplyr::filter(year ==2018)

Days_at_sea_period18<-lapply(temporal_seq, function(i){
  nm<-names(i)
  i<-as.data.frame(dat18%>%dplyr::filter(month %in% i)%>%dplyr::distinct(FID, MMSI, DAY,.keep_all=T)%>%dplyr::group_by(FID)%>%count(name="Days_at_Sea"))%>%dplyr::select(FID, Days_at_Sea)
  return(i)
})

names(Days_at_sea_period18[[1]])<-paste(c("FID", "Days_at_Sea18_t1"))
names(Days_at_sea_period18[[2]])<-paste(c("FID", "Days_at_Sea18_t2"))
names(Days_at_sea_period18[[3]])<-paste(c("FID", "Days_at_Sea18_t3"))



grid_das<-left_join(grid_whole, Days_at_sea_period17[[1]], by="FID") %>%left_join(., Days_at_sea_period17[[2]], by="FID" )%>%left_join(., Days_at_sea_period17[[3]], by="FID" )%>%left_join(., Days_at_sea_period18[[1]], by="FID" )%>%left_join(., Days_at_sea_period18[[2]], by="FID" )%>%left_join(., Days_at_sea_period18[[3]], by="FID" )%>% replace(., is.na(.), 0)


####
### Save shp
setwd("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results")
dir.create(file.path("./","daysatsea7" ))
st_write(grid_das,  paste0("./","daysatsea7/", "daysatsea7.shp")) #shapefile

