library(tidyverse)
library(sf)
library(readxl)
library(sp)

########################### Confronto 1: scatterplot dei Fdays
df17<-read_sf("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results/grid_filled/grid_filled2017.shp")
df18<-read_sf("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results/grid_filled/grid_filled2018.shp")

#### Lavorare per i tre periodi

df_17<-data.frame(df17)%>%dplyr::select(-FID_1, -geometry)%>% replace(., is.na(.), 0) %>%gather(key=type, value=FD, -fra, -FID) %>%dplyr::rename("FD_17"="FD")
df_18<-data.frame(df18)%>%dplyr::select(-FID_1, -geometry)%>% replace(., is.na(.), 0)%>%gather(key=type, value=FD, -fra, -FID)%>%dplyr::rename("FD_18"="FD")#
df<-df_17%>%dplyr::left_join(., df_18, by=c("FID","fra", "type"))%>%arrange(type, desc(FD_17))

df<-df_17%>%dplyr::select(FID, FDays)%>%dplyr::filter(FDays >0)%>%arrange(desc(FDays))%>%dplyr::rename("FDays_2014"= "FDays")%>%dplyr::left_join(., df17, by="FID")%>%dplyr::rename("FDays_2017"= "FDays_tot")
#df<-df%>%dplyr::mutate(FD14= log10(1+FDays_2014), FD17=log10(1+FDays_2017))%>%dplyr::filter(FD14>=1, FD17 >=1)


df_bisett<-df%>%dplyr::rename("FDy"= "FD_17", "FDx"="FD_18")%>%dplyr::mutate(FDx = FDy)

#df_bisett<-df_bisett%>%dplyr::mutate(FDxl= log10(FDx), FDyl=log10(FDy))%>%dplyr::filter(FDxl >=1, FDyl >=1)
#plot(df$FDays_2014, df$FDays_tot)
head(df)
####### Plot grezzo
ggplot(df, aes(x=FD_17, y=FD_18)) +
  geom_point(aes(col=as.factor(fra))) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett, aes(x=FDx, y=FDy), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("Fishing days 2017")+ylab("Fishing days 2018")+ facet_wrap(~type, scales="free")+  labs(color = "FRA area") 

setwd("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results")
ggsave("Comparison_raw_GSA17.png", width = 300, units="mm")

### area A
####### Plot grezzo
ggplot(df%>%dplyr::filter(fra=="A"), aes(x=FD_17, y=FD_18)) +
  geom_point(aes(col=as.factor(fra))) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett%>%dplyr::filter(fra=="A"), aes(x=FDx, y=FDy), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("Fishing days 2017")+ylab("Fishing days 2018")+ facet_wrap(~type, scales="free")+  labs(color = "FRA area") 

ggsave("Comparison_raw_GSA17_zonaA.png", width = 300, units="mm")

## area B
ggplot(df%>%dplyr::filter(fra=="B"), aes(x=FD_17, y=FD_18)) +
  geom_point(aes(col=as.factor(fra))) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett%>%dplyr::filter(fra=="B"), aes(x=FDx, y=FDy), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("Fishing days 2017")+ylab("Fishing days 2018")+ facet_wrap(~type, scales="free")+  labs(color = "FRA area") 

ggsave("Comparison_raw_GSA17_zonaB.png", width = 300, units="mm")

## area C
ggplot(df%>%dplyr::filter(fra=="C"), aes(x=FD_17, y=FD_18)) +
  geom_point(aes(col=as.factor(fra))) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett%>%dplyr::filter(fra=="C"), aes(x=FDx, y=FDy), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("Fishing days 2017")+ylab("Fishing days 2018")+ facet_wrap(~type, scales="free")+  labs(color = "FRA area") 

ggsave("Comparison_raw_GSA17_zonaC.png", width = 300, units="mm")





####### Plot Log
df<-df%>%dplyr::mutate(FD14= log10(1+FDays_2014), FD17=log10(1+FDays_2017))%>%dplyr::filter(FD14>=1, FD17 >=1)
df_bisett<-df_bisett%>%dplyr::mutate(FDxl= log10(FDx), FDyl=log10(FDy))%>%dplyr::filter(FDxl >=1, FDyl >=1)

ggplot(df, aes(x=FD14, y=FD17)) +
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett, aes(x=FDxl, y=FDyl), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("log10 Fishing days 2014")+ylab("log10 Fishing days 2017")
ggsave("Comparison_log_GSA17.png")

write_sf(df, "Differenza2.shp")

############################################ Confronto 2: conta MMSI nell'area
box<-read_sf(dsn= "~/CNR/AIS/Datasets/SHAPEFILES/GSA17")
setwd("~/CNR/AIS/Lavori/Lavori 2020/Oloturie/ais/2014")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read_csv)
country_codes<- read_excel("~/CNR/AIS/Lavori/Lavori 2019/GSA_EXCHANGE/GSA-exchange/COUNTRY_CODES.xlsx")

Processed_data<-lapply(myfiles, function(i){
    colonne<-names(i)
    ## set id and spatial characteristics
    i<-st_as_sf(i, coords=c("LONGITUDE", "LATITUDE"))%>% st_set_crs(st_crs(box))%>% dplyr::mutate(point_id=seq(1:nrow(.)))
    ## intersect
    point_in_clip<-as.data.frame(st_intersects(box, i))%>%dplyr::rename("FID"="row.id", "point_id"="col.id")
    ## remove
    i<-i%>%dplyr::filter(point_id %in% point_in_clip$point_id)
    coor<-as.data.frame(st_coordinates(i))%>%dplyr::rename("LONGITUDE"= "X", "LATITUDE"="Y")
    i<-as.data.frame(bind_cols(i, coor))%>%dplyr::select(colonne)
    return(i)
  })

DF14<-lapply(Processed_data, function(i){
  i<-i%>%dplyr::select(MMSI, LATITUDE, LONGITUDE, SPEED)
  return(i)
})
#Final_df_correspondence<-rbind_list(DF14)
#Punti_select<-st_as_sf(Final_df_correspondence, coords=c("LONGITUDE", "LATITUDE"))%>% st_set_crs(st_crs(bbox))
#st_write(Punti_select,  "check.shp") #shapefile


DF14<-rbind_list(DF14)%>%dplyr::distinct(MMSI)%>%dplyr::mutate(CODE= as.numeric(substr(MMSI, 1,3)))%>% dplyr::left_join(country_codes, by="CODE")%>%dplyr::group_by(COUNTRY)%>%tally(name="n_boat14")

#### Subset grid
Grid1718<-grid_base%>%dplyr::filter(FID %in% point_in_clip$FID)

## Subset DF17
load("~/CNR/AIS/Lavori/Lavori 2020/GitHub/MED_UNITS/Files_intermedi/grid_1x1_trawlers_completo.RData")# DF17
DF17OTB<-rbind_list(df)%>%dplyr::filter(FID %in% Grid1718$FID)%>%dplyr::distinct(MMSI)

load("~/CNR/AIS/Lavori/Lavori 2020/Oloturie/ais/TBB_1x1_rawinters.RData")
DF17TBB<-rbind_list(df)%>%dplyr::filter(FID %in% Grid1718$FID)%>%dplyr::distinct(MMSI)

DF17<-bind_rows(DF17OTB, DF17TBB)%>%dplyr::distinct(MMSI)%>%dplyr::mutate(CODE= as.numeric(substr(MMSI, 1,3)))%>% dplyr::left_join(country_codes, by="CODE")%>%dplyr::group_by(COUNTRY)%>%tally(name="n_boat17")

#####
DF_tot<-full_join(DF14, DF17, by="COUNTRY")%>%dplyr::filter(COUNTRY %in% c("Italy", "Croatia", "Slovenia"))%>%dplyr::mutate(Variation_perc = round((n_boat17/n_boat14)-1, 3))

write.csv(DF_tot, "Numero_barche.csv")
