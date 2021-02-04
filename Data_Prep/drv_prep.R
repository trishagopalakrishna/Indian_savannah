### In this script I calcualte different dimensions of rainfall variability for India###
### Started on- 20/01/2021
### Last edit made on- 
### Last edit made-

###############################################################################################
#install.packages(c("ncdf4", "lattice"))
library(ncdf4)
library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)



rasterOptions(overwrite = TRUE, tmpdir = "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Scratch",
              tmptime = 2, progress="window", timer = TRUE,chunksize = 2e+07, maxmemory = 1e+10)
memory.limit(size=20000)
###############################################################################################
#From Schwartz et al., 2020 

##################################################1. MCWD quantifies the severity of dry season

india_shp<-readOGR(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")

monthly_ppt<- function (fn_ppt, year){
  #fn_ppt<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1981.nc"
  nc_data<-nc_open(fn_ppt)
  print (nc_data)
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  
  head(lon) # look at the first few entries in the longitude vector
  
  ppt <- ncvar_get(nc_data, 'ppt' )
  dim(ppt) #12 months in 1981
  ppt_fillvalue <- ncatt_get(nc_data, "ppt", "_FillValue")
  ppt_fillvalue #-32768
  
  nc_close(nc_data)
  
  ppt[ppt == ppt_fillvalue$value] <- NA
  
  Sys.time()
  ppt_list<-list()
  for (i in 1:dim(ppt)[3]){
    extraction<- ppt[,,i]
    r<-raster(t(extraction), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #transpose lat long
    r_cropped<- crop(r, india_shp)
    r_masked <- mask (r_cropped, india_shp)
    ppt_list[i]<-r_masked
    print("Next raster")
  }
  Sys.time() #2 min
  
  months<-1:12
  output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT_rasters"
  for (i in 1:length(months)){
    writeRaster(ppt_list[[i]], filename=file.path(paste0(output_dir,"\\", year,"_", months[i])), bylayer=TRUE,format="GTiff")
  }
  print ("Next year needed")
}

Sys.time()
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1982.nc", 1982)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1983.nc", 1983)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1984.nc", 1984)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1985.nc", 1985)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1986.nc", 1986)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1987.nc", 1987)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1988.nc", 1988)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1989.nc", 1989)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1990.nc", 1990)
Sys.time()
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1991.nc", 1991)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1992.nc", 1992)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1993.nc", 1993)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1994.nc", 1994)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1995.nc", 1995)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1996.nc", 1996)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1997.nc", 1997)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1998.nc", 1998)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_1999.nc", 1999)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2000.nc", 2000)
Sys.time()
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2001.nc", 2001)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2002.nc", 2002)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2003.nc", 2003)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2004.nc", 2004)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2005.nc", 2005)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2006.nc", 2006)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2007.nc", 2007)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2008.nc", 2008)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2009.nc", 2009)
monthly_ppt("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Rainfall\\MCWD\\PPT\\TerraClimate_ppt_2010.nc", 2010)
Sys.time()

monthly_pet<- function (fn_pet, year){
  #fn_pet<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2007.nc"
  nc_data<-nc_open(fn_pet)
  print (nc_data)
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  
  head(lon) # look at the first few entries in the longitude vector
  
  pet <- ncvar_get(nc_data, 'pet' )
  dim(pet) #12 
  pet_fillvalue <- ncatt_get(nc_data, "pet", "_FillValue")
  pet_fillvalue #-32768
  
  nc_close(nc_data)
  
  pet[pet == pet_fillvalue$value] <- NA
  
  Sys.time()
  pet_list<-list()
  for (i in 1:dim(pet)[3]){
    extraction<- pet[,,i]
    r<-raster(t(extraction), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) #transpose lat long
    r_cropped<- crop(r, india_shp)
    r_masked <- mask (r_cropped, india_shp)
    pet_list[i]<-r_masked
    print("Next raster")
  }
  Sys.time()
  
  months<-1:12
  output_dir<-"C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET_rasters"
  for (i in 1:length(months)){
    writeRaster(pet_list[[i]], filename=file.path(paste0(output_dir,"\\", year,"_", months[i])), bylayer=TRUE,format="GTiff")
  }
  print ("Next year needed")
}

Sys.time()
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1982.nc", 1982)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1983.nc", 1983)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1984.nc", 1984)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1985.nc", 1985)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1986.nc", 1986)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1987.nc", 1987)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1988.nc", 1988)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1989.nc", 1989)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1990.nc", 1990)
Sys.time()
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1991.nc", 1991)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1992.nc", 1992)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1993.nc", 1993)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1994.nc", 1994)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1995.nc", 1995)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1996.nc", 1996)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1997.nc", 1997)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1998.nc", 1998)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_1999.nc", 1999)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2000.nc", 2000)
Sys.time()
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2001.nc", 2001)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2002.nc", 2002)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2003.nc", 2003)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2004.nc", 2004)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2005.nc", 2005)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2006.nc", 2006)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2007.nc", 2007)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2008.nc", 2008)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2009.nc", 2009)
monthly_pet("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper2\\Data\\Rainfall\\MCWD\\PET\\TerraClimate_pet_2010.nc", 2010)
Sys.time()

############ Modifying script from ## MCWD (Maximum Cumulative Water Deficit) Script ##
# Reference: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006GL028946 #

#Following Yadvinder's 2009 paper, I calculate the average annual precip for all Jans, all febs..... and the average pet for all jans, all febs..
#and then find wd=ppt-pet, resulting in 12 rasters- one for each month

input_path<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PET_rasters"

month_input_list <- list.files(path= input_path , pattern = '_12.tif$', full.names = T) #change '_month #.tif$'
month_input_list

month_rainfall_rasters<-list()

for (i in 1:length(month_input_list)){
  temp<-raster(file.path(month_input_list[i]))
  month_rainfall_rasters[[i]]<-temp
}

month_stack<-stack(month_rainfall_rasters)
mean_month_value<-mean(month_stack, na.rm=TRUE)
mean_month_value
plot(mean_month_value)

output_path<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PET_rasters//Mean_month_pet//"
writeRaster(mean_month_value,filename = file.path(paste0(output_path,"_", "pet_dec")), format="GTiff") #change 'pet/ppt_monthname'

#Could not make above code a function, so reran it by chan
remove(temp, mean_month_value, month_rainfall_rasters, month_stack, input_path, month_input_list, output_path)

mean.monthly.rainfall<-list.files(path= "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PPT_rasters//Mean_month_ppt", pattern = '.tif$', full.names = T)
mean.monthly.rainfall
mean.monthly.pet<-list.files(path= "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PET_rasters//Mean_month_pet", pattern = '.tif$', full.names = T)
mean.monthly.pet

output_dir<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//WD_rasters//"
Sys.time()
for (i in 1:length(mean.monthly.rainfall)){
  ppt<-raster(mean.monthly.rainfall[i])
  pet<-raster(mean.monthly.pet[i])
  wd<- ppt-pet
  wd
  
  writeRaster(wd,filename = file.path(paste0(output_dir, strsplit(names(ppt), split='_', fixed = TRUE)[[1]][3])), format="GTiff") 
}
Sys.time() #mm/month

remove(pet, ppt, i, mean.monthly.pet, mean.monthly.rainfall, output_dir)

monthly.wd<-list.files(path= "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//WD_rasters", pattern = '.tif$', full.names = T)
monthly.wd
wd_stack<-stack(monthly.wd)
wd_stack

# MCWD Function
mcwd.f = function(y){
  result= as.numeric(y)
  for(i in 1:length(result)){
    wdn = result[i]
    wdn1 = result[i-1]
    
    if(i==1){
      if(!is.na(wdn) & wdn>0){ result[i]=0}
      else{result[i]=wdn}
    }
    
    if(i!=1){
      cwd = wdn1+wdn
      if(!is.na(cwd) & cwd < 0){ result[i]=cwd}
      else{result[i]=0}
    }
  }
  return(result)  
}

# Applying the Function
Sys.time()
cwd <- raster::calc(wd_stack, fun = mcwd.f)
Sys.time()
cwd 

Sys.time() # Determining the MCDW across 1981-2010
for (i in seq(1,12,12)) { # Replace 132 by the Total Months of the Time Series
  cwd.a = cwd[[i:(i+11)]]
  mcwd.a = min(cwd.a)
  writeRaster(mcwd.a, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//mcwd.tif") 
}
Sys.time()

mcwd.a
plot(mcwd.a)

##################################################2. MAP
# Processed in GEE (CHIRPS script)
map<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MAP//CHIRPSAnnualPrecip.tif")
map
plot(map)

##################################################3. Coefficient of variation of MAP
# Processed in GEE (CHIRPS script)
map_cv<-raster("C:///Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//PrecipSeasonality_CV//CHIRPSPrecipSeasonality.tif")
map_cv
plot(map_cv)

##################################################4. Dry season length
