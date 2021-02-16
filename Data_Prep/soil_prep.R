### In this script I pre process the sand fraction data extracted from GEE
### Started on- 15/02/2021
### Last edit made on- 
### Last edit made-

###############################################################################################
#install.packages(c("ncdf4", "lattice"))
#library(ncdf4)
library(sf)
library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)


rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+10)
memory.limit(size=20000)
############################################################################################### Input data
sand0_5<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//Savannah_Sand0_5mean.tif")
sand5_15<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//Savannah_Sand5_15mean.tif")
sand15_30<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//Savannah_Sand15_30mean.tif")

sand0_5 
#Projection is Homolosine projection (equal area)- https://www.isric.org/explore/soilgrids/faq-soilgrids

#All rasters need to be divided by conversion factor 10- https://www.isric.org/explore/soilgrids/faq-soilgrids
sand0_5_converted<-sand0_5/10
sand5_15_converted<-sand5_15/10
sand15_30_converted<-sand15_30/10

#Changing projection to WGS 1984

mcwd<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//mcwd.tif")
mcwd #weird resolution 0.04165702 ~4km - SPATIAL RESOLUTION OF ANALYSIS

sand0_5_proj<-projectRaster(sand0_5_converted, mcwd, method = 'bilinear')
sand5_15_proj<-projectRaster(sand5_15_converted,mcwd, method = 'bilinear')
sand15_30_proj<-projectRaster(sand15_30_converted, mcwd, method = 'bilinear')

writeRaster(sand0_5_proj,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand0_5_proj.tif")
writeRaster(sand5_15_proj,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand5_15_proj.tif")
writeRaster(sand15_30_proj,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Soil//Sand//sand15_30_proj.tif")
