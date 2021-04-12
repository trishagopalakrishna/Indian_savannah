### In this script I process the MODIS data that I have extracted from GEE
### Started on- 04/03/2021
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
rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+09)
memory.limit(size=20000)
############################################################################################### Data Extraction
#Following this website https://www.r-bloggers.com/2013/11/working-with-hdf-files-in-r-example-pathfinder-sst-data/
#install.packages("BiocManager")
#BiocManager::install("rhdf5")


india_shp<-readOGR(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")

input_path<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//"
month_input_list <- list.files(path= input_path , pattern = '.tif$', full.names = T) #change '_month #.tif$'

modis_burn_rasters<-list()

for (i in 1:length(month_input_list)){
  temp<-stack(file.path(month_input_list[i]))
  modis_burn_rasters[[i]]<-temp
}
remove(temp)
modis_burn_rasters

modis_burn_rasters_SPAT<-list()
for (i in 1:length(modis_burn_rasters)){
 x<-rast(modis_burn_rasters[[i]])
 modis_burn_rasters_SPAT[[i]]<-x
}
remove(x)
#install.packages("terra")
#library(terra)

Sys.time();merged_rasters<-do.call(terra::merge, modis_burn_rasters_SPAT); Sys.time() 
##Seems ot have worked? There are 242 layers, 1 for each month Nov 2000-Dec 2020

merged_rasters2<-stack(merged_rasters)

Sys.time()
merged_rasters_crop<-terra::crop(merged_rasters, india_shp);Sys.time()
merged_rasters_mask<-terra::mask (merged_rasters_crop,vect(india_shp)); Sys.time()

merged_rasters_mask2<-stack(merged_rasters_mask)

#output_dir<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//MODIS_BurnArea_Monthly//"
#for (i in 1:nlayers(merged_rasters_mask2)){
#  writeRaster(merged_rasters_mask2[[i]], filename=file.path(paste0(output_dir,"\\", names(merged_rasters_mask2[[i]]))), bylayer=TRUE,format="GTiff")
#}

merged_rasters_mask2

input_path<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\MODIS_BurnedArea\\MODIS_BinaryBurn\\MODIS_BurnArea_Monthly\\"
burnedarealist <- list.files(path= input_path , pattern = '.tif$', full.names = T) #change '_month #.tif$'
burnedarealist


library(gtools)
merged_rasters_mask2 <- stack(mixedsort(burnedarealist))

Sys.time();merged_rasters_mask2_coarse<-terra::aggregate(rast(merged_rasters_mask2), fact=20, fun=max); Sys.time() #2 min
merged_rasters_mask2_coarse

library(av)

asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()
anim_burnarea_modis<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(merged_rasters_mask2_coarse) + tm_raster(title="")+ tm_facets(nrow=1, ncol=1, free.coords = FALSE)+
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title= "Burn/No Burn", legend.show = TRUE)

tmap_animation(anim_burnarea_modis, filename = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//modis_burnarea.mp4",
               delay = 50, loop = TRUE, restart.delay = 500,
               width = 900, height = 900, dpi = 150) 


merged_rasters_mask3<-rast(merged_rasters_mask2)
merged_rasters_mask3

##Finding Burn freq by adding all the rasters and creating one raster where any pixel value>0 is the number of times
##the pixel has burned

Sys.time();burnfreq<-raster::calc(merged_rasters_mask2, sum, na.rm=TRUE); Sys.time()

roy<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Royetal2015_Data//IndiaVegType2012//recoded_india_veg.img")
plot(roy)
crs(roy) #taking projection from here
Sys.time();merged_rasters_mask2_proj<-terra::project(merged_rasters_mask3, "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs", method = "near")
Sys.time() #took about 13 hours

merged_rasters_mask2_proj2<-stack(merged_rasters_mask2_proj)
output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\MODIS_BurnedArea\\MODIS_BinaryBurn\\MODIS_BurnArea_Monthly\\UTM_proj"
Sys.time()
for (i in 1:nlayers(merged_rasters_mask2_proj2)){
 writeRaster(merged_rasters_mask2_proj2[[i]], filename=file.path(paste0(output_dir,"\\", names(merged_rasters_mask2_proj2[[i]]))), bylayer=TRUE,format="GTiff")
}; Sys.time() #took an hour


burnfreq_map_gee<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//MODIS_BurnFreq.tif")
burnfreq_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(burnfreq_map_gee) + tm_raster(palette = "YlOrRd",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title= "Burn Frequency", legend.show = TRUE)
burnfreq_map
tmap_save(burnfreq_map, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//burnfreq_map_modis.png", dpi = 300)

resolution<- ((res(merged_rasters_mask2_proj2)[1])/1000*(res(merged_rasters_mask2_proj2)[2])/1000) #area in Ha

burnedarea<- function (r_s){
  r_s<-r_s*resolution
  return(r_s)
}

merged_rasters_mask2_proj3<-rast(merged_rasters_mask2_proj2)
Sys.time(); burnedarea_modis<- terra::arith(merged_rasters_mask2_proj3, fun = burnedarea); Sys.time() #6 min

burnedarea_modis

state_shp<-st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//gadm36_IND_shp//gadm36_IND_1.shp")
state_shp_proj<-st_transform(state_shp, crs("+proj=utm +zone=43 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
state_shp_proj

india_states<-st_as_sf(state_shp_proj)
india_states<- india_states %>% mutate(Final_State_Name= case_when(
  NAME_1=="Andaman and Nicobar"~"Andaman and Nicobar",
  NAME_1=="Andhra Pradesh" ~ "Andhra Pradesh",
  NAME_1=="Arunachal Pradesh" ~ "Arunachal Pradesh",
  NAME_1=="Assam" ~ "Assam",
  NAME_1=="Bihar"~ "Bihar",
  NAME_1=="Chandigarh"~"Punjab",
  NAME_1=="Chhattisgarh"~"Chhattisgarh",
  NAME_1=="Dadra and Nagar Haveli"~ "Gujarat",
  NAME_1=="Daman and Diu"~ "Gujarat",
  NAME_1=="Goa"~ "Goa",
  NAME_1=="Gujarat"~"Gujarat",
  NAME_1=="Haryana"~"Haryana",
  NAME_1=="Himachal Pradesh"~ "Himachal Pradesh",
  NAME_1=="Jammu and Kashmir"~ "Jammu and Kashmir",
  NAME_1=="Jharkhand"~"Jharkhand",
  NAME_1=="Karnataka"~"Karnataka",
  NAME_1=="Kerala"~"Kerala",
  NAME_1=="Lakshadweep"~"Lakshadweep",
  NAME_1=="Madhya Pradesh"~"Madhya Pradesh",
  NAME_1=="Maharashtra"~ "Maharashtra",
  NAME_1=="Manipur"~"Manipur",
  NAME_1=="Meghalaya"~"Meghalaya",
  NAME_1=="Mizoram"~"Mizoram",
  NAME_1=="Nagaland"~"Nagaland",
  NAME_1=="NCT of Delhi"~"NCT of Delhi",
  NAME_1=="Odisha"~"Odisha",
  NAME_1=="Puducherry"~"Tamil Nadu",
  NAME_1=="Punjab"~"Punjab",
  NAME_1=="Rajasthan"~"Rajasthan",
  NAME_1=="Sikkim"~"Sikkim",
  NAME_1=="Tamil Nadu"~"Tamil Nadu",
  NAME_1=="Telangana"~"Telangana",
  NAME_1=="Tripura"~"Tripura",
  NAME_1=="Uttar Pradesh"~"Uttar Pradesh",
  NAME_1=="Uttarakhand"~"Uttarakhand",
  NAME_1=="West Bengal"~ "West Bengal"
))
india_states

state_name_df<-india_states$Final_State_Name
state_name_df<-data.frame(state_name_df)
colnames(state_name_df)<-"State_Name"

area_list<-list()
Sys.time()
for ( i in 1:nlyr(burnedarea_modis)){
  x_raster<-burnedarea_modis[[i]]
  print ("Raster read, extraction begins")
  x<- terra::extract(x_raster, vect(india_states), fun=sum, na.rm= TRUE, df= TRUE)
  area_list[[i]] <- x
  i
  print ("Next raster")
}; Sys.time() #took 5 hours

remove(x, x_raster,i)

area_list2<-area_list

for ( i in 1:length(area_list2)){
  area_list2[[i]]<-area_list2[[i]] %>% bind_cols(state_name_df)
  area_list2[[i]]<-area_list2[[i]] %>% mutate(Date=names(area_list2[[i]])[2])
  area_list2[[i]]<-area_list2[[i]] %>% tidyr::separate(Date, c("Year", "Month", NA, NA), sep="_")
  area_list2[[i]]<-area_list2[[i]] %>% mutate(Year=gsub("X","",Year))
  
}

area_list3<-list()
for (i in 1:length(area_list2)){
  colnames(area_list2[[i]])[2]<-"BurnedArea_Ha"
  area_list3[[i]]<-area_list2[[i]] %>% group_by(State_Name, Year, Month) %>% summarise(TotalBurnedArea_Ha=sum(BurnedArea_Ha))
}

burnedarea_df<-bind_rows(area_list3)
write.csv(burnedarea_df, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//MODIS_BurnArea_Monthly//UTM_proj//burnedarea_alldata_modis.csv")

burnedarea_df_years<-burnedarea_df %>% group_by(Year) %>% summarise(TotalBurnedArea_Ha=sum(TotalBurnedArea_Ha))
write.csv(burnedarea_df_years, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//MODIS_BurnArea_Monthly//UTM_proj//burnedarea_by_year.csv")

############ MODIS vs GFEDvs
gfed<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//BurnedArea_rasters//UTM_proj//burnedarea_by_year.csv")
modis<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//MODIS_BurnedArea//MODIS_BinaryBurn//MODIS_BurnArea_Monthly//UTM_proj//burnedarea_by_year.csv")

head(gfed)
head(modis)

gfed<-gfed %>% mutate(Product= "GFEDvs")
modis<-modis %>% mutate(Product="MODIS")

burnarea<-bind_rows(gfed, modis)
write.csv(burnarea,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//modis_gfed.csv" )

ggplot(burnarea, aes(as.factor(Year), TotalBurnedArea_Ha)) + scale_fill_manual(values=c("#999999", "#D55E00"))+ xlab("Year")+ylab("Total Burned Area (Ha)")+
  geom_bar(aes(fill = Product), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//modis_gfed.png", dpi=300)
# Contrary to what I had thought. I had thought that GFEDvs will show more burned area per year than MODIS, which is not the case.
# Maybe this is because well GFEDvs is a product of MODIS
