### In this script I calcualte different dimensions of rainfall variability for India###
### Started on- 20/01/2021
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
###############################################################################################
#From Schwartz et al., 2020, I extracted which dimensions of rainfall I should calculate 
##################################################
#Absolute seasonality
##################################################1. MCWD quantifies the severity of dry season (based on latest climatalogical normal i.e. 1982-2010)

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

#Could not make above code a function, so reran it by changing ppt to pet
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
mcwd.a<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//mcwd.tif")
##################################################
##################################################2. Dry season length 1982 to 2010
input_path1<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PPT_rasters"
input_path2<-"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MCWD//PET_rasters"

month_input_list1 <- list.files(path= input_path1 , pattern = '.tif$', full.names = T) #All rasters of all years and months
month_input_list1
month_input_list2 <- list.files(path= input_path2 , pattern = '.tif$', full.names = T) #All rasters of all years and months
month_input_list2

month_ppt_rasters<-list()
for (i in 1:length(month_input_list1)){
  temp<-raster(file.path(month_input_list1[i]))
  month_ppt_rasters[[i]]<-temp
}

month_pet_rasters<-list()
for (i in 1:length(month_input_list2)){
  temp<-raster(file.path(month_input_list2[i]))
  month_pet_rasters[[i]]<-temp
}

stack_list<-list() #creating stacks of ppt an dpet for eahc month for each year
for (i in 1:length(month_pet_rasters)){
  ppt<-month_ppt_rasters[[i]]
  pet<-month_pet_rasters[[i]]
  if(names(ppt)==names(pet)){
    stack1<-stack(ppt,pet)
    result<-stack1
    stack_list[[i]]<-stack1
  }
}
remove(stack1)

rc<-function(ppt, pet){
  ifelse(ppt<pet,1,0)
} #function with if statement to reclass 

reclass_list<-list()
for (i in 1:length(stack_list)){ #running function over entire list of stacks of pet and ppt
  r.class<-overlay(stack_list[[i]], fun=rc)
  names(r.class)<-names(stack_list[[i]])[1]
  reclass_list[[i]]<-r.class
}
reclass_stack<-stack(reclass_list)
indice<-rep(c(1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29), each = 12)

year_sum<-stackApply(reclass_stack, indice, fun = sum)
mean_dry_season_months<-mean(year_sum)
writeRaster(mean_dry_season_months, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//DrySeasonLength//dryseasonlength_1982_2010.tif")

remove(rc, reclass_list,i, raster_name_index, drySeasonLength, reclass_stack, year_sum)
##################################################
##################################################3. Dry season rainfall 1982 to 2010
rc_2<-function(ppt, pet){
  ifelse(ppt<pet, ppt,0)
} #function with if statement to reclass 

reclass_list_2<-list()
for (i in 1:length(stack_list)){ #running function over entire list of stacks of pet and ppt
  r.class<-overlay(stack_list[[i]], fun=rc_2)
  names(r.class)<-names(stack_list[[i]])[1]
  reclass_list_2[[i]]<-r.class
}
reclass_stack_2<-stack(reclass_list_2)
year_sum_2<-stackApply(reclass_stack_2, indice, fun = sum)
mean_dry_season_rainfall<-mean(year_sum_2)
writeRaster(mean_dry_season_rainfall, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//DrySeasonRainfall//dryseasonrainfall_1982_2010.tif")

remove(r.class,rc_2,year_sum_2, reclass_stack_2, reclass_list_2, stack_list)
##################################################
##################################################4. Seasonality Index (Feng et al., 2012) 1982 to 2010
ppt_stack<-stack(month_ppt_rasters)

seasonalityIndexWalsh <- function(rainSeries){#function calculates Walsh and Lawler's (1981) seasonality index
  AR <- sum(rainSeries)
  seas <- (1/AR)*sum(abs(rainSeries-AR/12))
  return(seas)
}

seasonality1982<-seasonalityIndexWalsh(ppt_stack[[1:12]])
seasonality1983<-seasonalityIndexWalsh(ppt_stack[[13:24]])
seasonality1984<-seasonalityIndexWalsh(ppt_stack[[24:36]])
seasonality1985<-seasonalityIndexWalsh(ppt_stack[[37:48]])
seasonality1986<-seasonalityIndexWalsh(ppt_stack[[49:60]])
seasonality1987<-seasonalityIndexWalsh(ppt_stack[[61:72]])
seasonality1988<-seasonalityIndexWalsh(ppt_stack[[72:84]])
seasonality1989<-seasonalityIndexWalsh(ppt_stack[[85:96]])
seasonality1990<-seasonalityIndexWalsh(ppt_stack[[97:108]])

seasonality1991<-seasonalityIndexWalsh(ppt_stack[[109:120]])
seasonality1992<-seasonalityIndexWalsh(ppt_stack[[121:132]])
seasonality1993<-seasonalityIndexWalsh(ppt_stack[[133:144]])
seasonality1994<-seasonalityIndexWalsh(ppt_stack[[145:156]])
seasonality1995<-seasonalityIndexWalsh(ppt_stack[[157:168]])
seasonality1996<-seasonalityIndexWalsh(ppt_stack[[169:180]])
seasonality1997<-seasonalityIndexWalsh(ppt_stack[[181:192]])
seasonality1998<-seasonalityIndexWalsh(ppt_stack[[193:204]])
seasonality1999<-seasonalityIndexWalsh(ppt_stack[[205:216]])
seasonality2000<-seasonalityIndexWalsh(ppt_stack[[217:228]])

seasonality2001<-seasonalityIndexWalsh(ppt_stack[[229:240]])
seasonality2002<-seasonalityIndexWalsh(ppt_stack[[241:252]])
seasonality2003<-seasonalityIndexWalsh(ppt_stack[[253:264]])
seasonality2004<-seasonalityIndexWalsh(ppt_stack[[265:276]])
seasonality2005<-seasonalityIndexWalsh(ppt_stack[[277:288]])
seasonality2006<-seasonalityIndexWalsh(ppt_stack[[289:300]])
seasonality2007<-seasonalityIndexWalsh(ppt_stack[[301:312]])
seasonality2008<-seasonalityIndexWalsh(ppt_stack[[313:324]])
seasonality2009<-seasonalityIndexWalsh(ppt_stack[[325:336]])
seasonality2010<-seasonalityIndexWalsh(ppt_stack[[337:348]])

seasonality_across_years<-mean(seasonality1982,seasonality1983,seasonality1984,seasonality1985,seasonality1986,seasonality1987,seasonality1988,seasonality1989,seasonality1990,
                  seasonality1991,seasonality1992,seasonality1993,seasonality1994,seasonality1995,seasonality1996,seasonality1997,seasonality1998,seasonality1999,
                  seasonality2000,seasonality2001,seasonality2002,seasonality2003,seasonality2004,seasonality2005,seasonality2006,seasonality2007,seasonality2008,
                  seasonality2009,seasonality2010)
writeRaster(seasonality_across_years, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//PrecipSeasonality//seasonalitywalsh_1982_2010.tif") 
remove(seasonality1982,seasonality1983,seasonality1984,seasonality1985,seasonality1986,seasonality1987,seasonality1988,seasonality1989,seasonality1990,
       seasonality1991,seasonality1992,seasonality1993,seasonality1994,seasonality1995,seasonality1996,seasonality1997,seasonality1998,seasonality1999,
       seasonality2000,seasonality2001,seasonality2002,seasonality2003,seasonality2004,seasonality2005,seasonality2006,seasonality2007,seasonality2008,
       seasonality2009,seasonality2010)
##################################################5. MAP -1981 to 2019
# Processed in GEE (Savannah script in Climate)
## Calculated as sum of daily precip for each month for all years and then divided by total number of months 
map<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//MAP//Savannah_MAP_1981_2020.tif")
map
plot(map)

##################################################
##################################################6. Coefficient of variation of MAP- 1981 to 2019
# Processed in GEE (CHIRPS script in Climate i.e. not Savannah script)
## Calculated as SD(precip in each months in entire time period)/ 1+(mean of precip of all months)
map_cv<-raster("C:///Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//PrecipSeasonality_CV//Savannah_PrecipSeasonality_1981_2020.tif")
map_cv
plot(map_cv)




##Plots
library(tmap)

asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()

mcwd_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(mcwd.a) + tm_raster(palette = "YlOrRd",title="MCWD")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
mcwd_map

map_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(map) + tm_raster(palette = "Blues",title="Mean Annual Precipitation (mm)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
map_map

mapcv_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(map_cv) + tm_raster(palette = "Purples",title="Coefficient of variation MAP(mm)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
mapcv_map

drv<-tmap_arrange(mcwd_map, map_map, mapcv_map, ncol=3)
tmap_save(drv,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Rainfall//drv_plot.png", dpi = 700)
