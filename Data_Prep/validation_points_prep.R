### In this script I extract certain vegetation classes from Roy et al., csv location data
### Started on- 09/03/2021
### Last edit made on- 
### Last edit made- 

###############################################################################################

library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(tmap)
library(RColorBrewer)
library(viridis)
library(terra)

terraOptions(memfrac=0.5, tempdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
             progress=10)
memory.limit(size=20000)
############################################################################################### Data Extraction#India shp
spp_grid<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//PSR_Outputs//spp_grid_data.csv")
head(spp_grid)
dim(spp_grid)

# As decided in presence_pa.xlxs Data/Royetal2015_Data/IndiaVegType2012, for presences, keeping only following
# Vegetation.type
unique(spp_grid$Vegetation.type)
x<-spp_grid %>% distinct(Vegetation.type)
x %>% arrange (desc(x))

openecosystem_data<-spp_grid[(spp_grid$Vegetation.type=="Ziziphus"|spp_grid$Vegetation.type=="Woodland"|spp_grid$Vegetation.type=="Wet grassland" | 
                           spp_grid$Vegetation.type=="Tree savannah" |spp_grid$Vegetation.type=="Thorn scrub"|
                           spp_grid$Vegetation.type=="Thorn forest"| spp_grid$Vegetation.type=="Swampy grassland"|
                           spp_grid$Vegetation.type=="Shrub savannah" | spp_grid$Vegetation.type=="Shifting cultivation"|
                           spp_grid$Vegetation.type=="Sehima-Dichanthium grassland" |spp_grid$Vegetation.type=="Scrub"|
                           spp_grid$Vegetation.type=="Riverine grasslands"| spp_grid$Vegetation.type=="Prosopis juliflora"|spp_grid$Vegetation.type=="Prosopis cineraria"|
                           spp_grid$Vegetation.type=="Open Scrub"| spp_grid$Vegetation.type=="Open scrub"|
                           spp_grid$Vegetation.type=="Moist Alpine Scrub"|spp_grid$Vegetation.type=="Moist alpine scrub"|spp_grid$Vegetation.type=="Moist Alpine Pasture"|
                           spp_grid$Vegetation.type=="Moist alpine pasture"| spp_grid$Vegetation.type=="Lasiurus-Panicum grassland"|
                           spp_grid$Vegetation.type=="Lantana scrub"|spp_grid$Vegetation.type=="Euphorbia scrub"|
                           spp_grid$Vegetation.type=="Dry evergreen scrub"|spp_grid$Vegetation.type=="Dry deciduous scrub"|
                           spp_grid$Vegetation.type=="Dry alpine scrub"| spp_grid$Vegetation.type=="Dry alpine pasture"|
                           spp_grid$Vegetation.type=="Drt grassland"| spp_grid$Vegetation.type=="Degraded Forest"|spp_grid$Vegetation.type=="Degraded forest"|
                           spp_grid$Vegetation.type=="Current jhum"| spp_grid$Vegetation.type=="Costal swampy grassland"|spp_grid$Vegetation.type=="Cenchrus-Dactyloctenium grasslan"|
                           spp_grid$Vegetation.type=="Abandoned jhum"|spp_grid$Vegetation.type=="Grassland"),]
#67483 points in presence_data

distinct_latlong<-openecosystem_data %>% 
  distinct (LON,LAT)  #3565 unique lon and lat

count_vegtype<- latlong_veg_aggregate %>%
  group_by(Vegetation.type) %>%
  tally() #34 different "vegetation types"
write.csv(count_vegtype, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed//openecosystems_tally.csv")

openecosystem_data2<- openecosystem_data %>% mutate(Formation=case_when(
  Vegetation.type=="Woodland"| Vegetation.type== "Tree savannah"| Vegetation.type=="Shrub savannah"~"Woodland",
  Vegetation.type=="Degraded Forest"| Vegetation.type== "Degraded forest"| Vegetation.type=="Shifting cultivation"|
  Vegetation.type=="Abandoned jhum"| Vegetation.type== "Current jhum"~"Degraded Formation",
  Vegetation.type=="Scrub"| Vegetation.type== "Open Scrub"|Vegetation.type=="Open scrub"| Vegetation.type=="Dry evergreen scrub"|
  Vegetation.type=="Dry deciduous scrub"| Vegetation.type== "Ziziphus"| Vegetation.type=="Euphorbia scrub"|
  Vegetation.type=="Moist Alpine Scrub"| Vegetation.type== "Moist alpine scrub"| Vegetation.type=="Dry alpine scrub"|
  Vegetation.type=="Prosopis juliflora"| Vegetation.type== "Lantana scrub"| Vegetation.type=="Thorn scrub"|
  Vegetation.type=="Prosopis cineraria"~ "Scrub/shrub",
  Vegetation.type=="Grassland"| Vegetation.type== "Wet grassland"| Vegetation.type=="Riverine grasslands"|
  Vegetation.type=="Moist Alpine Pasture"| Vegetation.type== "Moist alpine pasture"| Vegetation.type=="Dry alpine pasture"| 
  Vegetation.type=="Swampy grassland"| Vegetation.type== "Lasiurus-Panicum grassland"| Vegetation.type=="Cenchrus-Dactyloctenium grasslan"| 
    Vegetation.type=="Sehima-Dichanthium grassland"| Vegetation.type== "Costal swampy grassland"|Vegetation.type=="Drt grassland"~"Grasslands",
  Vegetation.type=="Thorn forest"~"Thorn Forest"
))
   
latlong_veg_aggregate<- openecosystem_data2 %>%
  distinct(LON, LAT, Vegetation.type, Formation) #3565 unique lat long and associated veg type
write.csv(latlong_veg_aggregate, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed//openecosystems_latlong.csv")

count_vegtype<- latlong_veg_aggregate %>%
  group_by(Vegetation.type) %>%
  tally() #34 different "vegetation types"
write.csv(count_vegtype, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed//openecosystems_tally.csv")


state_shp<-st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//gadm36_IND_shp//gadm36_IND_1.shp")
state_shp<-state_shp[!(state_shp$NAME_1=="Andaman and Nicobar" | state_shp$NAME_1=="Lakshadweep"),] #removing A & N and Lakshadweep

state_shp<-state_shp %>% mutate(NAME_1=replace(NAME_1, NAME_1=="Chandigarh", "Punjab"))
state_shp<-state_shp %>% mutate(NAME_1=replace(NAME_1, NAME_1=="Dadra and Nagar Haveli", "Gujarat"))
state_shp<-state_shp %>% mutate(NAME_1=replace(NAME_1, NAME_1=="Daman and Diu", "Gujarat"))
state_shp<-state_shp %>% mutate(NAME_1=replace(NAME_1, NAME_1=="Puducherry", "Tamil Nadu"))

state_shp2 <- state_shp %>% group_by(NAME_1) %>% summarise(geometry=st_union(geometry))


latlong<-latlong_veg_aggregate[,c(1,2)]
latlong_shp<-SpatialPointsDataFrame(coords = latlong, data = latlong_veg_aggregate,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
latlong_shp2<-st_as_sf(latlong_shp)
latlong_state<-st_join(latlong_shp2, state_shp2, join=st_intersects) %>% ungroup()
head(latlong_state)
st_write(latlong_state, dsn="C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed",
         layer="roy_openecosystems", driver= "ESRI Shapefile", append = FALSE)

perstate_presence_df<-latlong_state %>% group_by(NAME_1) %>% tally() %>% ungroup
perstate_presence_df
perstate_presence_df<-as.data.frame(perstate_presence_df)
## No points in Nagaland. 21 points have no state. Visually checked these in QGIS and found that all these points lie in A&N islands and part of Kashmir not included
##in above shp. So removed these points.
latlong_state<-latlong_state[!is.na(latlong_state$NAME_1),]
st_write(latlong_state, dsn = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed",
         layer = "roy_openecosystems_final", driver = "ESRI Shapefile", append = FALSE)

##Map
asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")
india_shp<-readOGR(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()



openecosystems_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(latlong_state) + tm_dots(size=0.1,col="Formation", palette=c("black", "#339900", "#FF9933", "#FF0033"),title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
openecosystems_map
tmap_save(openecosystems_map,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed//openecosystems_map.png", dpi = 300)

veg_tally_state<-latlong_state %>% st_drop_geometry()
veg_tally_state<-veg_tally_state %>% group_by(NAME_1, Vegetation.type) %>% tally() %>% ungroup()

library(ggplot2)

ggplot(veg_tally_state, aes(x= NAME_1, y=n))+  geom_col(aes(fill=Vegetation.type), width=0.7)+
  xlab("States")+ylab("Number of points")+
  theme_bw(base_size = 10)+theme(axis.text.x = element_text(size=10, angle = 90))+theme(axis.title.x = element_blank())+
  theme(axis.text.y=element_text(size = 10))+
  theme(legend.title = element_text(size=10))+
  theme(legend.text = element_text(size = 10))

ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Vegetation_Processed//tally_states_openecosystems.png", width = 20, height = 10, dpi =300)
