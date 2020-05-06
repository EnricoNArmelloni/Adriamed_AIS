#####################
# 
# Fishing effort from AIS data: intersection between fishing segments and grid
#####################
library(sf)
library(sp)
library(tidyverse)
library(data.table)
library(parallel)
library(MASS)
####----------------- Set working directory
#mydir<-"~/1_MEDUNITS"
mydir<-"~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Input_data"
setwd(mydir)
no_cores<-detectCores()-2  ### number of cores to be used
#####---------------- Define parameters
### Periods
t1<-seq(1:8)
t2<-c(9,10)
t3<-c(11,12)
temporal_seq<-list(t1=t1,t2=t2,t3=t3)

#####---------------- Data Import
grid_whole <- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/SHAPEFILES/MEDUNITS_OTB17_fra")%>%dplyr::mutate(fra=ifelse(is.na(fra)==TRUE, "D", fra))%>%dplyr::mutate(FID =seq(1:nrow(.)))%>%dplyr::select(FID, FID_1, fra)

# segments
segments_otb<- read_sf(dsn = "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Input_data/segm_input2018") %>%dplyr::mutate(segm_id= seq(1:nrow(.)))

#####---------------- Data adjustments
# Crop the grid: divide the grid in portions
chunk <- function(x,no_cores) split(x, factor(sort(rank(x)%%no_cores)))
split_grid<-chunk(x <- 1:max(grid_whole$FID),no_cores)
grid_chunk<-lapply(split_grid, function(i){ 
  grid_chunk<-grid_whole %>%dplyr::filter(FID %in% i)
  return(grid_chunk)
})

#####---------------- Data cleaning
#rm(grid_whole, split_grid)

#####---------------- Define parallelization settings
cl <- makeCluster(no_cores) # Initiate cluster
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(sf))
clusterEvalQ(cl, library(sp))
clusterEvalQ(cl, library(data.table))
clusterExport(cl, "segments_otb")
clusterExport(cl, "no_cores")

#####---------------- Core function 
system.time({ 
  df<-parLapply(cl, grid_chunk,function(i){ 
    
    ### Step 1: make an empty polygon from bbox of grid chunk
    a<-st_bbox(i)
    x_min<-as.numeric(a[1])
    x_max<-as.numeric(a[3])
    y_min<-as.numeric(a[2])
    y_max<-as.numeric(a[4])
    coords<-matrix(c(x_min, y_min, x_max, y_min, x_max, y_max, x_min, y_max, x_min, y_min), ncol=2, byrow=TRUE)
    sps = SpatialPolygons(list(Polygons(list(Polygon(coords)),1)))
    pol<-st_as_sf(sps)%>%st_set_crs(st_crs(i))
    
    ### Step 2: Retain segments and harbours falling whitin the polygon
    sbs<- st_intersects(pol, segments_otb, sparse =TRUE)
    selection<-as.data.frame(sbs)%>%dplyr::rename("FID"="row.id", "segm_id"="col.id")
    
    ## verify if there are segments whitin the polygon
    if(nrow(selection) >= no_cores){
      df<-segments_otb%>% dplyr::filter(segm_id %in% selection$segm_id)
      
      ### Step 3: split selected segments in chunks and setting for parallelization
      bins  <- rep(1:no_cores, nrow(df) / no_cores)
      dc<-split(df, bins)
      
      ### Step 4: spatial intersection
      elab1<-lapply( dc, function(j){
        int<- st_intersection(i, j )
        int$obs_len<-as.numeric(st_length(int))/1000 ## lenght in km
        int<-as.data.frame(int)%>%dplyr::select(-geometry)
        return(int)
      })
      
      ### Step 5: unlist
      dat<-rbindlist(elab1)
      return(dat)
      
    } else if (nrow(selection) >= 1 & nrow(selection)< no_cores) {
      df<-segments_otb%>% dplyr::filter(segm_id %in% selection$segm_id)%>%dplyr::mutate(segm_id =seq(1:nrow(.)))
      int<- st_intersection(i, df)
      int$obs_len<-as.numeric(st_length(int))/1000 ## lenght in km
      dat<-as.data.frame(int)%>%dplyr::select(-geometry)
      return(dat)
    } else {
      dat<-tibble("FID"=numeric(),   "MMSI"=numeric()  ,"session" =numeric(),"segment" =numeric(),"speed_kn" =numeric(),"month_obs"=numeric(),"day"=numeric(), "segm_id"=numeric(),"obs_len"=numeric())
      return(dat)
    }
  })
})
stopCluster(cl)
#save(df, file=paste0(grid_nm,gear, "raw_intersection.RData"))

#####---------------- Statistics
# summarize information
int2<-rbindlist(df) # unlist

############# Map fishing
data_fish<-int2%>%dplyr::filter(trattov== 4)

Fishing_Days_period<-lapply(temporal_seq, function(i){
  nm<-names(i)
  i<-as.data.frame(data_fish%>%dplyr::filter(mnth_bs %in% i)%>%dplyr::distinct(FID, MMSI, session, day,.keep_all=T)%>%dplyr::group_by(FID)%>%count(name="FDays"))
  return(i)
})
names(Fishing_Days_period[[1]])<-paste(c("FID", "FDays_t1"))
names(Fishing_Days_period[[2]])<-paste(c("FID", "FDays_t2"))
names(Fishing_Days_period[[3]])<-paste(c("FID", "FDays_t3"))

grid_fishing<-left_join(grid_whole, Fishing_Days_period[[1]], by="FID") %>%left_join(., Fishing_Days_period[[2]], by="FID" )%>%left_join(., Fishing_Days_period[[3]], by="FID" )


############# Map unknown
data_unk<-int2%>%dplyr::filter(trattov== 1)

UNK_Days_period<-lapply(temporal_seq, function(i){
  nm<-names(i)
  i<-as.data.frame(data_unk%>%dplyr::filter(mnth_bs %in% i)%>%dplyr::distinct(FID, MMSI, session, day,.keep_all=T)%>%dplyr::group_by(FID)%>%count(name="UNK_Days"))
  return(i)
})
names(UNK_Days_period[[1]])<-paste(c("FID", "UNKDays_t1"))
names(UNK_Days_period[[2]])<-paste(c("FID", "UNKDays_t2"))
names(UNK_Days_period[[3]])<-paste(c("FID", "UNKDays_t3"))


grid_fishing<-left_join(grid_fishing, UNK_Days_period[[1]], by="FID") %>%left_join(., UNK_Days_period[[2]], by="FID" )%>%left_join(., UNK_Days_period[[3]], by="FID" )


# save 
st_write(grid_fishing, "~/CNR/AIS/Lavori/Lavori 2020/Adriamed/Input_data/grid_filled/grid_filled.shp" ) #shapefile

