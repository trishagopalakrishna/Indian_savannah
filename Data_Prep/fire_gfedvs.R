### In this script I process the GFED vs data
### Started on- 02/03/2021
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

rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+10)
memory.limit(size=20000)
############################################################################################### Data Extraction
#Following this website https://www.r-bloggers.com/2013/11/working-with-hdf-files-in-r-example-pathfinder-sst-data/
#install.packages("BiocManager")
#BiocManager::install("rhdf5")


india_shp<-readOGR(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")
library("rhdf5")

yearly_burnedarea<-function(fname, year){
  #fname<-("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_1998.hdf5")
  tmp<- h5ls(fname)
  tmp

  lon <- h5read(fname, "lon")
  lat <- h5read(fname, "lat")

  burnedarea <- h5read(fname, "burned_area")
  

  burned_area_year<-list()
  for ( i in 1:12){
    extraction<-burnedarea[[i]][1]
    r<-raster(t(extraction$burned_fraction), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #transpose lat long 
    r_cropped<- crop(r, india_shp)
    r_masked <- mask (r_cropped, india_shp)
    burned_area_year[i]<-r_masked
    print ("Next raster")
  }
  months<-1:12
  output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\BurnedArea_rasters"
  for (i in 1:length(months)){
    writeRaster(burned_area_year[[i]], filename=file.path(paste0(output_dir,"\\", year,"_", months[i])), bylayer=TRUE,format="GTiff")
  }
  print ("Next year needed")
  }

year1997<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_1997.hdf5",1997)
year1998<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_1998.hdf5",1998)
year1999<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_1999.hdf5",1999)
year2000<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2000.hdf5",2000)
year2001<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2001.hdf5",2001)
year2002<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2002.hdf5",2002)
year2003<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2003.hdf5",2003)
year2004<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2004.hdf5",2004)
year2005<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2005.hdf5",2005)
year2006<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2006.hdf5",2006)
year2007<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2007.hdf5",2007)
year2008<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2008.hdf5",2008)
year2009<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2009.hdf5",2009)
year2010<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2010.hdf5",2010)
year2011<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2011.hdf5",2011)
year2012<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2012.hdf5",2012)
year2013<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2013.hdf5",2013)
year2014<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2014.hdf5",2014)
year2015<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2015.hdf5",2015)
year2016<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2016.hdf5",2016)
#year2017<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2017_beta.hdf5",2017)
#year2018<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2018_beta.hdf5",2018)
#year2019<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2019_beta.hdf5",2019)
#year2020<-yearly_burnedarea("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//GFED4.1s_2020_beta.hdf5",2020)
#2017, 2018, 2019, 2020 does not have burned area, only has emissions

remove(year1997, year1998, year1999, year2000, year2001, year2002, year2003, year2004, year2005, year2006, year2007, year2008,
       year2009, year2010, year2011, year2012, year2013, year2014, year2015, year2016,i, lat, lon, temp, tmp, BA_list)
############################################################################################### Data Processing
input_path<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//BurnedArea_rasters"
burnedarea_month_input_list <- list.files(path= input_path , pattern = '.tif$', full.names = T) #change '_month #.tif$'
burnedarea_month_input_list
#install.packages("gtools")
library(gtools)
burnedarea_reorder_month <- stack(mixedsort(burnedarea_month_input_list))
burnedarea_reorder_month #0.25 degrees is th eunits


##1. Burned counts i.e. number of times a pixel burnt i.e frequency of burn
binary_function<- function (r_s){
  if (any(is.na(r_s))) return(r_s*NA)
  r_s<-ifelse (r_s>0,1,0 )
  return(r_s)
}
Sys.time()
burnedarea_reorder_month_binary<- calc(burnedarea_reorder_month, fun = binary_function); Sys.time() #8 minutes
burnedarea_reorder_month_binary

months<-rep(c(1:12), times=20)
years<-rep(c(1997:2016), each=12)
#output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\Binaryburn_rasters"
#for (i in 1:nlayers(burnedarea_reorder_month_binary)){
#  writeRaster(burnedarea_reorder_month_binary[[i]], filename=file.path(paste0(output_dir,"\\", years[[i]],"_", months[i])), bylayer=TRUE,format="GTiff")
#}

Sys.time()
burnfreq<- calc(burnedarea_reorder_month_binary, fun=sum); Sys.time()
burnfreq
plot(burnfreq)
#riteRaster(burnfreq, "C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\Binaryburn_rasters\\burnfreq.tif")
burnfreq<-raster("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\Binaryburn_rasters\\burnfreq.tif")

burnfreq[burnfreq==0]<-NA

asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()


burnfreq_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(burnfreq) + tm_raster(palette = "YlOrRd",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title= "Burn Frequency", legend.show = TRUE)
burnfreq_map
tmap_save(burnfreq_map,"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\Binaryburn_rasters\\burnfreq_gfed.png", dpi = 300)
#Convert to UTM projection using nearest neighbor method to 
remove(burnfreq, burnfreq_map, burnedarea_reorder_month_binary)

##1. Burn Area
roy<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Royetal2015_Data//IndiaVegType2012//recoded_india_veg.img")
plot(roy)
crs(roy) #taking projection from here
burnedarea_reorder_month_proj<-projectRaster(burnedarea_reorder_month, crs = "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs", method = "ngb")
burnedarea_reorder_month_proj # resolution is 26km x 27 km which is close to 0.25 degrees. So di dno
#output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\BurnedArea_rasters\\UTM_proj\\"
#for (i in 1:nlayers(burnedarea_reorder_month_proj)){
#  writeRaster(burnedarea_reorder_month_proj[[i]], filename=file.path(paste0(output_dir,"\\", years[[i]],"_", months[i])), bylayer=TRUE,format="GTiff")
#}

input_path<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Fire\\GFEDvs\\BurnedArea_rasters\\UTM_proj\\"
burnedarealist <- list.files(path= input_path , pattern = '.tif$', full.names = T) #change '_month #.tif$'
burnedarealist

library(gtools)
burnedarea_reorder_month_proj <- stack(mixedsort(burnedarealist))

resolution<- ((res(burnedarea_reorder_month_proj)[1])/1000*(res(burnedarea_reorder_month_proj)[2])/1000) #area in Ha

burnedarea<- function (r_s){
  if (any(is.na(r_s))) return(r_s*NA)
  r_s<-r_s*resolution
  return(r_s)
}
burnedarea<- calc(burnedarea_reorder_month_proj, fun = burnedarea)
burnedarea

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
for ( i in 1:nlayers(burnedarea)){
  x_raster<-burnedarea[[i]]
  print ("Raster read, extraction begins")
  x<- raster::extract(x_raster, india_states, fun=sum, na.rm= TRUE, df= TRUE)
  area_list[[i]] <- x
  i
  print ("Next raster")
}; Sys.time() #takes about an hour

remove(x, x_raster,i)


for ( i in 1:length(area_list)){
  area_list[[i]]<-area_list[[i]] %>% bind_cols(state_name_df)
  area_list[[i]]<-area_list[[i]] %>% mutate(Month=months[i])
  area_list[[i]]<-area_list[[i]] %>% mutate(Year=years[i])
}

area_list2<-list()
for (i in 1:length(area_list)){
  colnames(area_list[[i]])[2]<-"BurnedArea_Ha"
  area_list2[[i]]<-area_list[[i]] %>% group_by(State_Name, Year, Month) %>% summarise(TotalBurnedArea_Ha=sum(BurnedArea_Ha))
}

burnedarea_df<-bind_rows(area_list2)
write.csv(burnedarea_df, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//BurnedArea_rasters//UTM_proj//burnedarea_alldata.csv")

burnedarea_df_years<-burnedarea_df %>% group_by(Year) %>% summarise(TotalBurnedArea_Ha=sum(TotalBurnedArea_Ha))
write.csv(burnedarea_df_years, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Fire//GFEDvs//BurnedArea_rasters//UTM_proj//burnedarea_by_year.csv")

