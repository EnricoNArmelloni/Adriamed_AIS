##########
##
## Code to compare Fishing Days by cell between 2017 and 2017. Separate scatterplots are created by each area of the FRA.
##
## Enrico Nicola Armelloni - enrico.armelloni@irbim.cnr.it
##
## May 2020
##
##########




library(tidyverse)
library(sf)
library(readxl)
library(sp)

########################### Load data
df17<-read_sf("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results/grid_filled/grid_filled2017.shp")
df18<-read_sf("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results/grid_filled/grid_filled2018.shp")

# Format
df_17<-data.frame(df17)%>%dplyr::select(-FID_1, -geometry)%>% replace(., is.na(.), 0) %>%gather(key=type, value=FD, -fra, -FID) %>%dplyr::rename("FD_17"="FD")
df_18<-data.frame(df18)%>%dplyr::select(-FID_1, -geometry)%>% replace(., is.na(.), 0)%>%gather(key=type, value=FD, -fra, -FID)%>%dplyr::rename("FD_18"="FD")#

# Merge
df<-df_17%>%dplyr::left_join(., df_18, by=c("FID","fra", "type"))%>%arrange(type, desc(FD_17))

# Create bisector
df_bisett<-df%>%dplyr::rename("FDy"= "FD_17", "FDx"="FD_18")%>%dplyr::mutate(FDx = FDy)

####### Scatterplot with all areas
ggplot(df, aes(x=FD_17, y=FD_18)) +
  geom_point(aes(col=as.factor(fra))) + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  geom_smooth(data=df_bisett, aes(x=FDx, y=FDy), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("Fishing days 2017")+ylab("Fishing days 2018")+ facet_wrap(~type, scales="free")+  labs(color = "FRA area") 

setwd("~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Results")
ggsave("Comparison_raw_GSA17.png", width = 300, units="mm")

### area A
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



# Not run: example for log scatterplot

######## Plot Log
#df<-df%>%dplyr::mutate(FD14= log10(1+FDays_2014), FD17=log10(1+FDays_2017))%>%dplyr::filter(FD14>=1, FD17 >=1)
#df_bisett<-df_bisett%>%dplyr::mutate(FDxl= log10(FDx), FDyl=log10(FDy))%>%dplyr::filter(FDxl >=1, FDyl >=1)
#
#ggplot(df, aes(x=FD14, y=FD17)) +
#  geom_point() + 
#  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
#  geom_smooth(data=df_bisett, aes(x=FDxl, y=FDyl), color="red")+ ggtitle("Temporal evolution of fishing effort", subtitle = "Trawling pressure in 2017 vs Trawling pressure in 2018")+xlab("log10 Fishing #days 2014")+ylab("log10 Fishing days 2017")
#ggsave("Comparison_log_GSA17.png")
#
#write_sf(df, "Differenza2.shp")
