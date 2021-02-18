### In this script I process response variables
### Started on- 16/02/2021
### Last edit made on- 
### Last edit made-

###############################################################################################
#install.packages(c("ncdf4", "lattice", "leaflet"))
#library(ncdf4)
library(sf)
library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)
library(leaflet)
library(tmap)
library(RColorBrewer)
library(viridis)

rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+10)
memory.limit(size=20000)
############################################################################################### Input data
#rainfall 
mcwd<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//mcwd.tif")
dryseasonlength<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//DrySeasonLength//dryseasonlength_1982_2010.tif")
dryseasonrainfall<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//DrySeasonRainfall//dryseasonrainfall_1982_2010.tif")
seasonalitywalsh<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//PrecipSeasonality//seasonalitywalsh_1982_2010.tif")

#soil
sand0_5<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand0_5_proj.tif")
sand5_15<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand5_15_proj.tif")
sand15_30<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand15_30_proj.tif")

############################################################################################### Pre-processing
dryseasonlength_pro<-crop(dryseasonlength, mcwd)
dryseasonlength_pro<-mask(dryseasonlength_pro, mcwd)
writeRaster(dryseasonlength_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Rainfall_Processed//dryseasonlength_processed.tif")
dryseasonlength_pro<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Rainfall_Processed//dryseasonlength_processed.tif")

dryseasonrainfall_pro<-crop(dryseasonrainfall, mcwd)
dryseasonrainfall_pro<-mask(dryseasonrainfall_pro, mcwd)
writeRaster(dryseasonrainfall_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Rainfall_Processed//dryseasonrainfall_processed.tif")

seasonality_pro<-crop(seasonalitywalsh, mcwd)
seasonality_pro<-mask(seasonality_pro, mcwd)
writeRaster(seasonality_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Rainfall_Processed//seasonality_processed.tif")

sand0_5_pro<-crop(sand0_5, mcwd)
sand0_5_pro<-mask(sand0_5_pro, mcwd)
writeRaster(sand0_5_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Sandfraction_Processed//sand0_5_processed.tif")

sand5_15_pro<-crop(sand5_15, mcwd)
sand5_15_pro<-mask(sand5_15_pro, mcwd)
writeRaster(sand5_15_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Sandfraction_Processed//sand5_15_processed.tif")

sand15_30_pro<-crop(sand15_30, mcwd)
sand15_30_pro<-mask(sand15_30_pro, mcwd)
writeRaster(sand15_30_pro, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Sandfraction_Processed//sand15_30_processed.tif")
sand15_30_pro<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Sandfraction_Processed//sand15_30_processed.tif")

remove(dryseasonlength, dryseasonrainfall, seasonalitywalsh, sand0_5, sand5_15, sand15_30)

##Plots
library(tmap)

asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")
india_shp<-readOGR(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()

mcwd_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(mcwd) + tm_raster(palette = "YlOrRd",title="MCWD (mm)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
mcwd_map

dryseasonrainfall_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(dryseasonrainfall_pro) + tm_raster(palette = "Blues",title="Dry Season Rainfall (mm)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
dryseasonrainfall_map

dryseasonlength_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(dryseasonlength_pro) + tm_raster(style= "quantile",n=8, palette = "Purples",title="Dry Season Length (months)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
dryseasonlength_map

seasonality_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(seasonality_pro) + tm_raster(palette = "Greys",title="Seasonality Index (Walsh & Lawler 1981)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
seasonality_map
drv<-tmap_arrange(mcwd_map,dryseasonrainfall_map, dryseasonlength_map,seasonality_map, ncol=2)
tmap_save(drv,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Rainfall_Processed//drv_plot.png", dpi = 300)

sand15_30_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(sand15_30_pro) + tm_raster(n=8,palette = "Reds",title="Sand Fraction 15-30cm (g/100g)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sand15_30_map
tmap_save(sand15_30_map,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Sandfraction_Processed//sand15_30_plot.png", dpi = 300)

############################################################################################### Processing
###1. Seasonality sand index

sand15_30_pro #min=0, max=79.345 g/100g soil
#plot(sand15_30_pro)
dryseasonlength_pro #min=0, max=12 months
plot(dryseasonlength_pro)

# This index has infinite values because in some pixels there is no dry season length i.e value=0, so adding a 1 to it
dryseasonlength_1<-dryseasonlength_pro+1

sand15_30_dryseasonlength_1<-sand15_30_pro/dryseasonlength_1
sand15_30_dryseasonlength_1
plot(sand15_30_dryseasonlength_1)
writeRaster(sand15_30_dryseasonlength_1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//sandfraction_season_index.tif")

sandseason_index<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(sand15_30_dryseasonlength_1) + tm_raster(style= "quantile",n=10,palette = "inferno",title="Sand Fraction 15-30cm (g/100g)/ Dry season length (months")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sandseason_index
tmap_save(sandseason_index,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//sandseasonlength_index.png", dpi = 300)


#normalize by maximum value for both and then divide
#sand15_30_normalize<-sand15_30_pro/cellStats(sand15_30_pro, stat = max) #normalize with respect to sandiest place. So make sense that Rajasthan has values
#close to 1
#dryseasonlength_1_normalize<-dryseasonlength_1/cellStats(dryseasonlength_1, stat = max)

#plot(sand15_30_normalize)
#plot(dryseasonlength_1_normalize)

#sand15_30_dryseasonlength_2<-sand15_30_normalize/dryseasonlength_1_normalize
#sand15_30_dryseasonlength_2
#writeRaster(sand15_30_dryseasonlength_2, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch//index3.tif")

#sandnormalize_dryseason1<-sand15_30_normalize/dryseasonlength_1
#writeRaster(sandnormalize_dryseason1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch//sandnormalize_dryseason.tif")
