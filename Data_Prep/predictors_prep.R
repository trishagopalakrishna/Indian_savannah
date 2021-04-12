### In this script I process response variables
### Started on- 16/02/2021
### Last edit made on- 
### Last edit made-

###############################################################################################
#install.packages(c("ncdf4", "lattice", "leaflet"))
#library(ncdf4)
library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(ncdf4)
library(tmap)
library(RColorBrewer)
library(viridis)
library(ncdf4)
library(terra)
library(gtools)

terraOptions(memfrac=0.5, tempdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
             progress=10)
rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+09)
memory.limit(size=20000)
############################################################################################### Input data
#climate
mcwd<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//mcwd_noreorder.tif")
dryseasonlength<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//era_mean_annual_dryseasonlength.tif")
dryseasonrainfall<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//era5_mean_annual_dryseasonrainfall.tif")
aridity<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//era_mean_annual_aridity.tif")
map<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//era5_mean_annual_ppt.tif")

#soil
soilwatercapcaity<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Soil_Processed//ol_soilwatercapacity.tif")
isricsandfrac<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Soil_Processed//isric2_sandfrac0_30.tif")

#fire
burn_binary<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Fire_Processed//modis_totalburn2000_2020_nocrop.tif")

#herbivory
buffalo_dm<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//buffalo_density_DM.tif")
cattle_dm<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//cattle_density_DM.tif")
goat_dm<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//goat_density_DM.tif")
horse_dm<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//horse_density_DM.tif")
sheep_dm<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//sheep_density_DM.tif")


############################################################################################### Pre-processing
mcwd
dryseasonlength
dryseasonrainfall
aridity
map

climate<-rast(stack(mcwd, dryseasonlength, dryseasonrainfall, aridity, map))

soilwatercapcaity #extenbt is slightly different from climate variables
isricsandfrac#extenbt is slightly different from climate variables
soil<-rast(stack(soilwatercapcaity, isricsandfrac))

burn_binary #extenbt is slightly different from climate variables

buffalo_dm;cattle_dm;goat_dm;horse_dm; sheep_dm#extenbt is slightly different from climate variables; resolution is coarser so needs to be resample

###align extent by cropping and masking
soil # has the same resolution and nrows and ncols, only extent needs to be aligned
Sys.time();soil_resample<-terra::resample(soil, climate, method="bilinear"); Sys.time() # less than a minute

Sys.time(); cattle_resample<-terra::resample(rast(cattle_dm), rast(mcwd), method="bilinear"); Sys.time() #less than a minute
Sys.time(); buffalo_resample<-terra::resample(rast(buffalo_dm), rast(mcwd), method="bilinear"); Sys.time()
Sys.time(); goat_resample<-terra::resample(rast(goat_dm), rast(mcwd), method="bilinear"); Sys.time()
Sys.time(); horse_resample<-terra::resample(rast(horse_dm), rast(mcwd), method="bilinear"); Sys.time()
Sys.time(); sheep_resample<-terra::resample(rast(sheep_dm), rast(mcwd), method="bilinear"); Sys.time()
#for some reason min is -3.4+38 which is basically 0, so I am going to make every value less than 0 to 0
cattle_resample[cattle_resample<0]<-0
cattle_resample<-cattle_resample*10^6 #convert to tonnes from Mt

buffalo_resample[buffalo_resample<0]<-0
buffalo_resample<-buffalo_resample*10^6 #convert to tonnes from Mt

goat_resample[goat_resample<0]<-0
goat_resample<-goat_resample*10^6 #convert to tonnes from Mt

sheep_resample[sheep_resample<0]<-0
sheep_resample<-sheep_resample*10^6 #convert to tonnes from Mt

horse_resample[horse_resample<0]<-0
horse_resample<-horse_resample*10^6 #convert to tonnes from Mt

cattle_resample<-terra::crop(cattle_resample, rast(mcwd))
cattle_resample<-terra::mask(cattle_resample, rast(mcwd))

buffalo_resample<-terra::crop(buffalo_resample, rast(mcwd))
buffalo_resample<-terra::mask(buffalo_resample, rast(mcwd))

sheep_resample<-terra::crop(sheep_resample, rast(mcwd))
sheep_resample<-terra::mask(sheep_resample, rast(mcwd))

goat_resample<-terra::crop(goat_resample, rast(mcwd))
goat_resample<-terra::mask(goat_resample, rast(mcwd))

horse_resample<-terra::crop(horse_resample, rast(mcwd))
horse_resample<-terra::mask(horse_resample, rast(mcwd))

Sys.time();burn_binary_resample<-terra::resample(rast(burn_binary), climate, method= "bilinear"); Sys.time() #less than a minute

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
  tm_shape(dryseasonrainfall) + tm_raster(palette = "Blues",title="Dry Season Rainfall (mm)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
dryseasonrainfall_map

dryseasonlength_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(dryseasonlength) + tm_raster(style= "quantile",n=8, palette = "Purples",title="Dry Season Length (months)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
dryseasonlength_map

aridity_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(aridity) + tm_raster(palette = "Greens",title="Aridity Index", breaks=c(-1.25,-1,-0.5,0,0.5,1,1.25),midpoint = 0)+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
aridity_map

map_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(map) + tm_raster(style="quantile", n=8, palette = "Greys",title="Mean Annual Precipitation (m)")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
map_map

drv<-tmap_arrange(mcwd_map,dryseasonrainfall_map, dryseasonlength_map,aridity_map,map_map, ncol=2)
tmap_save(drv,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Climate_Processed//drv_plot.png", dpi = 300)

sand0_30_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(soil_resample[[2]]) + tm_raster(n=8,palette = "Reds",title="Weighted sum sand fraction 0-30cm (%)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sand0_30_map

soilwatercap_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(soil_resample[[1]]) + tm_raster(n=8,palette = "Purples",title="Soil water content at 30cm and 33kPa field (%)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
soilwatercap_map
soil<-tmap_arrange(soilwatercap_map, sand0_30_map, ncol=3)
tmap_save(soil,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Soil_Processed//soil_plot.png", dpi = 700)

burn_binary_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(burn_binary_resample) + tm_raster(title="Burn Area 2000-2020")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
burn_binary_map
tmap_save(burn_binary_map,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Fire_Processed//binaryburn_plot.png", dpi = 700)

buffalo_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(raster(buffalo_resample)) + tm_raster(style="quantile", n=8,palette = "Purples", title="Buffalo total dry matter intake (t/sqkm/year)")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
buffalo_map

cattle_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(raster(cattle_resample)) + tm_raster(style="quantile", n=6, title="Cattle total dry matter intake (t/sqkm/year)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
cattle_map

sheep_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(raster(sheep_resample)) + tm_raster(style="quantile", n=8,palette = "Greys", title="Sheep total dry matter intake (t/sqkm/year)")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sheep_map

goat_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(raster(sheep_resample)) + tm_raster(style="quantile", n=8,palette = "Reds", title="Goat total dry matter intake (t/sqkm/year)")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
goat_map

horse_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(raster(horse_resample)) + tm_raster(style="quantile", n=8,palette = "Blues", title="Horse total dry matter intake (t/sqkm/year)")+ 
  tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
horse_map

livestock_density_DM<-tmap_arrange(cattle_map, buffalo_map, goat_map,sheep_map, horse_map, ncol=3)
tmap_save(livestock_density_DM,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//livestock_density_DM.png", dpi = 300)
