### In this script I clean the web scraped livestock data from animal husbandry statistics website, for each district
### Started on- 03/02/2021
### Last edit made on- 
### Last edit made- Calculated grass deduction from dry matter intake

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
############################################################################################### Data Cleaning
urban_livestock_dir <- "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//Urban"
rural_livestock_dir<- "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//Rural"

##cattle
ff_cattle_exotic_urban <- list.files(urban_livestock_dir, pattern="\\_cattle_exotic.csv$", full.names=TRUE)
cattle_exotic_urban_list<-list()

ff_cattle_indi_urban <- list.files(urban_livestock_dir, pattern="\\_cattle_indigenous.csv$", full.names=TRUE)
cattle_indi_urban_list<-list()

ff_cattle_exotic_rural <- list.files(rural_livestock_dir, pattern="\\_cattle_exotic.csv$", full.names=TRUE)
cattle_exotic_rural_list<-list()

ff_cattle_indi_rural <- list.files(rural_livestock_dir, pattern="\\_cattle_indigenous.csv$", full.names=TRUE)
cattle_indi_rural_list<-list()


cattle_processing<-function(ff_cattle, cattle_list, region, type){
  for ( i in 1:length(ff_cattle)){
  x<-read.csv(ff_cattle[i])
  x<- x[,1:14]
  
  colnames(x)<-c("District", "Male_upto1.5", "Male_over1.5_breeding", "Male_over1.5_draught", "Male_over1.5_draught_breeding", "Male_over1.5_others", "Total_Male",
                 "Female_under1", "Female_1_2.5", "Female_over2.5_Milk", "Female_over2.5_Dry", "Female_over2.5_notcalved", "Female_over2.5_others", "Total_Female")
  x$District<-as.character(x$District)
  x$Male_upto1.5<-as.integer(x$Male_upto1.5)
  x$Male_over1.5_breeding<-as.integer(x$Male_over1.5_breeding)
  x$Male_over1.5_draught<-as.integer(x$Male_over1.5_draught)
  x$Male_over1.5_draught_breeding<-as.integer(x$Male_over1.5_draught_breeding)
  x$Male_over1.5_others<-as.integer(x$Male_over1.5_others)
  x$Total_Male<-as.integer(x$Total_Male)
  x$Female_under1<-as.integer(x$Female_under1)
  x$Female_1_2.5<-as.integer(x$Female_1_2.5)
  x$Female_over2.5_Milk<-as.integer(x$Female_over2.5_Milk)
  x$Female_over2.5_Dry<-as.integer(x$Female_over2.5_Dry)
  x$Female_over2.5_notcalved<-as.integer(x$Female_over2.5_notcalved)
  x$Female_over2.5_others<-as.integer(x$Female_over2.5_others)
  x$Total_Female<-as.integer(x$Total_Female)
  
  
  t1<-strsplit(ff_cattle[[i]], "/")[[1]][16]
  t2<-strsplit(t1, "&")[[1]][1]
  t2
  x$State_Name<-t2
  x$State_Name<-gsub('%20', '_', x$State_Name)
  x$Livestock<-"Cattle"
  x$Type<- type
  x$Region<- region
  
  x<-x %>% filter(District != "Total Count")
  cattle_list[[i]]<-x
  }
 cattle_list
}

a<-cattle_processing(ff_cattle_exotic_urban, cattle_exotic_urban_list, "Urban", "Exotic")
b<-cattle_processing(ff_cattle_indi_urban, cattle_indi_urban_list, "Urban", "Indigenous")

c<-cattle_processing(ff_cattle_exotic_rural, cattle_exotic_rural_list, "Rural", "Exotic")
d<-cattle_processing(ff_cattle_indi_rural, cattle_indi_rural_list, "Rural", "Indigenous")
                                    

cattle_data<-bind_rows(a,b,c,d)
summary(cattle_data)
write.csv(cattle_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//cattle_data.csv")
#cattle_data<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//cattle_data.csv")

remove(a,b,c,d,cattle_exotic_rural_list, cattle_exotic_urban_list, cattle_indi_rural_list, cattle_indi_urban_list, ff_cattle_exotic_rural,
       ff_cattle_exotic_urban, ff_cattle_indi_rural, ff_cattle_indi_urban)

##buffalo
ff_buffalo_urban <- list.files(urban_livestock_dir, pattern="\\_buffalo.csv$", full.names=TRUE)
buffalo_urban_list<-list()

ff_buffalo_rural <- list.files(rural_livestock_dir, pattern="\\_buffalo.csv$", full.names=TRUE)
buffalo_rural_list<-list()

buffalo_processing<-function(ff_buffalo, buffalo_list, region){
  for ( i in 1:length(ff_buffalo)){
    x<-read.csv(ff_buffalo[i])
    x<- x[,1:14]
    
    colnames(x)<-c("District", "Male_upto2", "Male_over2_breeding", "Male_over2_draught", "Male_over2_draught_breeding", "Male_over2_others", "Total_Male",
                   "Female_under1", "Female_1_3", "Female_over3_Milk", "Female_over3_Dry", "Female_over3_notcalved", "Female_over3_others", "Total_Female")
    x$District<-as.character(x$District)
    x$Male_upto2<-as.integer(x$Male_upto2)
    x$Male_over2_breeding<-as.integer(x$Male_over2_breeding)
    x$Male_over2_draught<-as.integer(x$Male_over2_draught)
    x$Male_over2_draught_breeding<-as.integer(x$Male_over2_draught_breeding)
    x$Male_over2_others<-as.integer(x$Male_over2_others)
    x$Total_Male<-as.integer(x$Total_Male)
    x$Female_under1<-as.integer(x$Female_under1)
    x$Female_1_3<-as.integer(x$Female_1_3)
    x$Female_over3_Milk<-as.integer(x$Female_over3_Milk)
    x$Female_over3_Dry<-as.integer(x$Female_over3_Dry)
    x$Female_over3_notcalved<-as.integer(x$Female_over3_notcalved)
    x$Female_over3_others<-as.integer(x$Female_over3_others)
    x$Total_Female<-as.integer(x$Total_Female)
    
    
    t1<-strsplit(ff_buffalo[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    x$State_Name<-t2
    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"Buffalo"
    x$Region<- region
    
    x<-x %>% filter(District != "Total Count")
    buffalo_list[[i]]<-x
  }
  buffalo_list
}

a<-buffalo_processing(ff_buffalo_urban, buffalo_urban_list, "Urban")
b<-buffalo_processing(ff_buffalo_rural, buffalo_rural_list, "Rural")

buffalo_data<-bind_rows(a,b)
summary(buffalo_data)
write.csv(buffalo_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//buffalo_data.csv")

remove(i, a,b,buffalo_urban_list, buffalo_rural_list, ff_buffalo_rural, ff_buffalo_urban)

##sheep
ff_sheep_exotic_urban <- list.files(urban_livestock_dir, pattern="\\_sheep_exotic.csv$", full.names=TRUE)
sheep_exotic_urban_list<-list()

ff_sheep_indi_urban <- list.files(urban_livestock_dir, pattern="\\_sheep_indigenous.csv$", full.names=TRUE)
sheep_indi_urban_list<-list()

ff_sheep_exotic_rural <- list.files(rural_livestock_dir, pattern="\\_sheep_exotic.csv$", full.names=TRUE)
sheep_exotic_rural_list<-list()

ff_sheep_indi_rural <- list.files(rural_livestock_dir, pattern="\\_sheep_indigenous.csv$", full.names=TRUE)
sheep_indi_rural_list<-list()

sheep_processing<-function(ff_sheep, sheep_list, region, type){
  for ( i in 1:length(ff_sheep)){
    x<-read.csv(ff_sheep[i])
    x<- x[,1:7]
    
    colnames(x)<-c("District", "Male_upto1", "Male_over1above","Total_Male",
                   "Female_upto1", "Female_over1above", "Total_Female")
    x$District<-as.character(x$District)
    x$Male_upto1<-as.integer(x$Male_upto1)
    x$Male_over1above<-as.integer(x$Male_over1above)
    x$Total_Male<-as.integer(x$Total_Male)
    x$Female_upto1<-as.integer(x$Female_upto1)
    x$Female_over1above<-as.integer(x$Female_over1above)
    x$Total_Female<-as.integer(x$Total_Female)
    
    t1<-strsplit(ff_sheep[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    x$State_Name<-t2
    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"Sheep"
    x$Type<- type
    x$Region<- region
    
    #x<-x %>% filter(District != "Total Count")
    sheep_list[[i]]<-x
  }
  sheep_list
}

a<-sheep_processing(ff_sheep_exotic_urban, sheep_exotic_urban_list, "Urban", "Exotic")
b<-sheep_processing(ff_sheep_indi_urban, sheep_indi_urban_list, "Urban", "Indigenous")
c<-sheep_processing(ff_sheep_exotic_rural, sheep_exotic_rural_list, "Rural", "Exotic")
d<-sheep_processing(ff_sheep_indi_rural, sheep_indi_rural_list, "Rural", "Indigenous")

sheep_data<-bind_rows(a,b,c,d)
summary(sheep_data)
write.csv(sheep_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//sheep_data.csv")

remove(a,b,c,d,sheep_exotic_rural_list, sheep_exotic_urban_list, sheep_indi_rural_list, sheep_indi_urban_list, ff_sheep_exotic_rural,
       ff_sheep_exotic_urban, ff_sheep_indi_rural, ff_sheep_indi_urban)

##goat
ff_goat_urban <- list.files(urban_livestock_dir, pattern="\\_goat.csv$", full.names=TRUE)
goat_urban_list<-list()

ff_goat_rural <- list.files(rural_livestock_dir, pattern="\\_goat.csv$", full.names=TRUE)
goat_rural_list<-list()

goat_processing<-function(ff_goat, goat_list, region){
  for ( i in 1:length(ff_goat)){
    x<-read.csv(ff_goat[i])
    x<- x[,1:9]
    
    colnames(x)<-c("District", "Male_under1", "Male_over1above","Total_Male",
                   "Female_under1", "Female_over1above_Milk", "Female_over1above_Dry", "Female_over1above_notcalved", "Total_Female")
    x$District<-as.character(x$District)
    x$Male_under1<-as.integer(x$Male_under1)
    x$Male_over1above<-as.integer(x$Male_over1above)
    x$Total_Male<-as.integer(x$Total_Male)
    x$Female_under1<-as.integer(x$Female_under1)
    x$Female_over1above_Milk<-as.integer(x$Female_over1above_Milk)
    x$Female_over1above_Dry<-as.integer(x$Female_over1above_Dry)
    x$Female_over1above_notcalved<-as.integer(x$Female_over1above_notcalved)
    x$Total_Female<-as.integer(x$Total_Female)
    
    
    t1<-strsplit(ff_goat[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    x$State_Name<-t2
    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"goat"
    x$Region<- region
    
    x<-x %>% filter(District != "Total Amount")
    goat_list[[i]]<-x
  }
  goat_list
}

a<-goat_processing(ff_goat_urban, goat_urban_list, "Urban")
b<-goat_processing(ff_goat_rural, goat_rural_list, "Rural")

goat_data<-bind_rows(a,b)
summary(goat_data)
write.csv(goat_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//goat_data.csv")

remove(a,b,goat_rural_list, goat_urban_list, ff_goat_rural, ff_goat_urban)

##camel
ff_camel_urban <- list.files(urban_livestock_dir, pattern="\\_camel.csv$", full.names=TRUE)
camel_urban_list<-list()

ff_camel_rural <- list.files(rural_livestock_dir, pattern="\\_camel.csv$", full.names=TRUE)
camel_rural_list<-list()

camel_processing<-function(ff_camel, camel_list, region){
  for ( i in 1:length(ff_camel)){
    x<-read.csv(ff_camel[i])
    x<- x[,1:7]
    
    colnames(x)<-c("District", "Male_under4", "Male_over4above","Total_Male",
                   "Female_under4", "Female_over4above","Total_Female")
    x$District<-as.character(x$District)
    x$Male_under4<-as.integer(x$Male_under4)
    x$Male_over4above<-as.integer(x$Male_over4above)
    x$Total_Male<-as.integer(x$Total_Male)
    x$Female_under4<-as.integer(x$Female_under4)
    x$Female_over4above<-as.integer(x$Female_over4above)
    x$Total_Female<-as.integer(x$Total_Female)
    
    
    t1<-strsplit(ff_camel[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    if (dim(x)[1]==0){
      x[1,1]<-NA
      x$State_Name<-t2
    } else {
      x$State_Name<- t2
    }
   
    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"Camel"
    x$Region<- region
    
    x<-x %>% filter(District != "Total Amount") 
    camel_list[[i]]<-x
  }
  camel_list
}

a<-camel_processing(ff_camel_urban, camel_urban_list, "Urban")
b<-camel_processing(ff_camel_rural, camel_rural_list, "Rural")

camel_data<-bind_rows(a,b)
summary(camel_data)
write.csv(camel_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//camel_data.csv")

remove(i,a,b,camel_rural_list, camel_urban_list, ff_camel_rural, ff_camel_urban)

##horses and ponies
ff_horses_urban <- list.files(urban_livestock_dir, pattern="\\_horses_and_ponies.csv$", full.names=TRUE)
horses_urban_list<-list()

ff_horses_rural <- list.files(rural_livestock_dir, pattern="\\_horses_and_ponies.csv$", full.names=TRUE)
horses_rural_list<-list()

horses_processing<-function(ff_horses, horses_list, region){
  for ( i in 1:length(ff_horses)){
    x<-read.csv(ff_horses[i])
    x<- x[,1:7]
    
    colnames(x)<-c("District", "Male_under4", "Male_over4above","Total_Male",
                   "Female_under4", "Female_over4above","Total_Female")
    x$District<-as.character(x$District)
    x$Male_under4<-as.integer(x$Male_under4)
    x$Male_over4above<-as.integer(x$Male_over4above)
    x$Total_Male<-as.integer(x$Total_Male)
    x$Female_under4<-as.integer(x$Female_under4)
    x$Female_over4above<-as.integer(x$Female_over4above)
    x$Total_Female<-as.integer(x$Total_Female)
    
    
    t1<-strsplit(ff_horses[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    if (dim(x)[1]==0){
      x[1,1]<-NA
      x$State_Name<-t2
    } else {
      x$State_Name<- t2
    }
    
    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"horses_and_ponies"
    x$Region<- region
    
    x<-x %>% filter(District != "Total Amount") 
    horses_list[[i]]<-x
  }
  horses_list
}

a<-horses_processing(ff_horses_urban, horses_urban_list, "Urban")
b<-horses_processing(ff_horses_rural, horses_rural_list, "Rural")

horses_data<-bind_rows(a,b)
summary(horses_data)
write.csv(horses_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//horses_data.csv")

remove(a,b,horses_rural_list, horses_urban_list, ff_horses_rural, ff_horses_urban)

##mithun and yak
ff_mithun_urban <- list.files(urban_livestock_dir, pattern="\\_mithun.csv$", full.names=TRUE)
mithun_urban_list<-list()

ff_yak_urban <- list.files(urban_livestock_dir, pattern="\\_yak.csv$", full.names=TRUE)
yak_urban_list<-list()

ff_mithun_rural <- list.files(rural_livestock_dir, pattern="\\_mithun.csv$", full.names=TRUE)
mithun_rural_list<-list()

ff_yak_rural <- list.files(rural_livestock_dir, pattern="\\_yak.csv$", full.names=TRUE)
yak_rural_list<-list()

mithun_processing<-function(ff_mithun, mithun_list, region, type){
  for ( i in 1:length(ff_mithun)){
    x<-read.csv(ff_mithun[i])
    x<- x[,1:7]
    
    colnames(x)<-c("District", "Male_under3", "Female_under3","Total_under3",
                   "Male_3above", "Female_3above", "Total_3above")
    x$District<-as.character(x$District)
    x$Male_under3<-as.integer(x$Male_under3)
    x$Male_under3<-as.integer(x$Male_under3)
    x$Total_under3<-as.integer(x$Total_under3)
    x$Male_3above<-as.integer(x$Male_3above)
    x$Female_3above<-as.integer(x$Female_3above)
    x$Total_3above<-as.integer(x$Total_3above)
    
    t1<-strsplit(ff_mithun[[i]], "/")[[1]][16]
    t2<-strsplit(t1, "&")[[1]][1]
    t2
    if (dim(x)[1]==0){
      x[1,1]=NA
      x$State_Name<-t2
    } else {
      x$State_Name<-t2
    }

    x$State_Name<-gsub('%20', '_', x$State_Name)
    x$Livestock<-"mithunyak"
    x$Type<- type
    x$Region<- region
    
    #x<-x %>% filter(District != "Total Count")
    mithun_list[[i]]<-x
  }
  mithun_list
}

a<-mithun_processing(ff_mithun_urban, mithun_urban_list, "Urban", "Mithun")
b<-mithun_processing(ff_yak_urban, yak_urban_list, "Urban", "Yak")
c<-mithun_processing(ff_mithun_rural, mithun_rural_list, "Rural", "MIthun")
d<-mithun_processing(ff_yak_rural, yak_rural_list, "Rural", "Yak")

mithun_data<-bind_rows(a,b,c,d)
summary(mithun_data)
write.csv(mithun_data, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//mithun_data.csv")

remove(a,b,c,d,mithun_rural_list, mithun_urban_list, yak_rural_list, yak_urban_list, ff_mithun_rural, ff_mithun_urban, ff_yak_rural, ff_yak_urban)
remove(rural_livestock_dir, urban_livestock_dir)
remove(buffalo_processing, camel_processing, cattle_processing, goat_processing, horses_processing, mithun_processing, sheep_processing)

############################################################################################### EDA and further cleaning
############################################################################################### Cattle

summary(cattle_data)
cattle_data_nototal<- subset(cattle_data, select=-c(Total_Male, Total_Female))
head(cattle_data_nototal)

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)
length(unique(cattle_data_nototal$State_Name))

cattle_data_nototal1<-cattle_data_nototal %>% filter(State_Name!="Lakshadweep") #removed 4 records

cattle_data_nototal1<-cattle_data_nototal1 %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
cattle_data_nototal1<-cattle_data_nototal1 %>% mutate(State_Name=replace(State_Name, State_Name=="Puducherry", "Tamil_Nadu"))

length(unique(cattle_data_nototal1$State_Name))

#orissa_cattle<-cattle_data_nototal1 %>% filter(State_Name=="Orissa")
#odisha_cattle<-cattle_data_nototal1 %>% filter(State_Name=="Odisha")
#checked that Odiha and Orissa is exactly same data with same districts and numbers. So removed Orissa data from cattle data

cattle_data_nototal1<-cattle_data_nototal1 %>% filter(State_Name!="Orissa") #removed 120 records

remove(orissa_cattle, odisha_cattle, cattle_data, cattle_data_nototal)

head(cattle_data_nototal1)
unique(cattle_data_nototal1$Type)
unique(cattle_data_nototal1$Region)

state_cattle<- cattle_data_nototal1 %>% group_by(Type, Region, State_Name) %>%
   summarise_at(.vars = vars(Male_upto1.5, Male_over1.5_breeding, Male_over1.5_draught, Male_over1.5_draught_breeding, Male_over1.5_others,
                Female_under1, Female_1_2.5, Female_over2.5_Milk, Female_over2.5_Dry, Female_over2.5_notcalved, Female_over2.5_others),
                sum, na.rm=TRUE)
head(state_cattle)
state_cattle_pivot<-pivot_longer(state_cattle, 4:14)
write.csv(state_cattle, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_cattle.csv")

state_list<-split(state_cattle_pivot, state_cattle_pivot$State_Name)

Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_cattle.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)) {
  df1<-state_list[[i]]
  a<-ggplot(df1, aes(x=name, y=value,fill=name))+ggtitle(unique(df1$State_Name))+
    scale_x_discrete(labels=c("Male upto 1.5yrs", "Male over 1.5yrs Breeding", "Male over 1.5yrs Draught", "Male over 1.5yrs Draught & Breeding", "Male over 1.5yrs Others",
                              "Female under 1yr", "Female 1-2.5yrs", "Female over 2.5yrs in milk", "Female over 2.5yrs dry", "Female over 2.5yrs not calved once", "Female over 2.5yrs others"))+
    scale_fill_brewer(palette = "Set3")+
    geom_bar(stat="identity")+xlab("Cattle")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none")+facet_wrap(~Region+Type)
  plot(a)
}
Sys.time();dev.off()

national_cattle<- cattle_data_nototal1 %>% group_by(Type, Region) %>%
  summarise_at(.vars = vars(Male_upto1.5, Male_over1.5_breeding, Male_over1.5_draught, Male_over1.5_draught_breeding, Male_over1.5_others,
                            Female_under1, Female_1_2.5, Female_over2.5_Milk, Female_over2.5_Dry, Female_over2.5_notcalved, Female_over2.5_others),
               sum, na.rm=TRUE)
national_cattle_pivot<-pivot_longer(national_cattle, 3:13)
write.csv(national_cattle_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_cattle.csv")

b<-ggplot(national_cattle_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male upto 1.5yrs", "Male over 1.5yrs Breeding", "Male over 1.5yrs Draught", "Male over 1.5yrs Draught & Breeding", "Male over 1.5yrs Others",
                            "Female under 1yr", "Female 1-2.5yrs", "Female over 2.5yrs in milk", "Female over 2.5yrs dry", "Female over 2.5yrs not calved once", "Female over 2.5yrs others"))+
  scale_fill_brewer(palette = "Set3")+
  geom_bar(stat="identity")+xlab("Cattle")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none")+facet_wrap(~Region+Type)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_cattle.png")


remove(a,b,c,cattle_data_nototal1, df1, national_cattle,
       national_cattle, national_cattle_pivot, 
        state_cattle, state_cattle_pivot, state_list, i, cattle_processing)

############################################################################################### Buffalo
summary(buffalo_data)

buffalo_data_nototal<- subset(buffalo_data, select=-c(Total_Male, Total_Female))
head(buffalo_data_nototal)

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)
length(unique(buffalo_data_nototal$State_Name))

buffalo_data_nototal1<-buffalo_data_nototal %>% filter(State_Name!="Lakshadweep") #removed 3 records

buffalo_data_nototal1<-buffalo_data_nototal1 %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
buffalo_data_nototal1<-buffalo_data_nototal1 %>% mutate(State_Name=replace(State_Name, State_Name=="Puducherry", "Tamil_Nadu"))

length(unique(buffalo_data_nototal1$State_Name))

orissa_buffalo<- buffalo_data_nototal1 %>% filter(State_Name=="Orissa")
odisha_buffalo<- buffalo_data_nototal1 %>% filter(State_Name=="Odisha")

buffalo_data_nototal1<-buffalo_data_nototal1 %>% filter(State_Name!="Orissa") #removed 60 records

remove(orissa_buffalo, odisha_buffalo, buffalo_data, buffalo_data_nototal)

head(buffalo_data_nototal1)
unique(buffalo_data_nototal1$Region)

state_buffalo<- buffalo_data_nototal1 %>% group_by(Region, State_Name) %>%
  summarise_at(.vars = vars(Male_upto2, Male_over2_breeding, Male_over2_draught, Male_over2_draught_breeding, Male_over2_others,
                                Female_under1, Female_1_3, Female_over3_Milk, Female_over3_Dry, Female_over3_notcalved, Female_over3_others),
                   .funs = c(sum="sum"))
head(state_buffalo)
state_buffalo_pivot<- pivot_longer(state_buffalo,3:13)
write.csv(state_buffalo_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_buffalo.csv")
state_list<-split(state_buffalo_pivot, state_buffalo_pivot$State_Name)
Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_buffalo.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)) {
  df1<-state_list[[i]]
  a<-ggplot(df1, aes(x=name, y=value,fill=name))+ggtitle(unique(df1$State_Name))+
    scale_x_discrete(labels=c("Male upto 1.5yrs", "Male over 1.5yrs Breeding", "Male over 1.5yrs Draught", "Male over 1.5yrs Draught & Breeding", "Male over 1.5yrs Others",
                              "Female under 1yr", "Female 1-2.5yrs", "Female over 2.5yrs in milk", "Female over 2.5yrs dry", "Female over 2.5yrs not calved once", "Female over 2.5yrs others"))+
    scale_fill_brewer(palette = "Set1")+
    geom_bar(stat="identity")+xlab("Buffalo")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
  plot(a)
}
Sys.time(); dev.off()

national_buffalo<- buffalo_data_nototal1 %>% group_by(Region) %>%
  summarise_at(.vars = vars(Male_upto2, Male_over2_breeding, Male_over2_draught, Male_over2_draught_breeding, Male_over2_others,
                            Female_under1, Female_1_3, Female_over3_Milk, Female_over3_Dry, Female_over3_notcalved, Female_over3_others),
               .funs = c(sum="sum"))
national_buffalo_pivot<-pivot_longer(national_buffalo,2:12)
write.csv(national_buffalo_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_buffalo.csv")

b<-ggplot(national_buffalo_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male upto 1.5yrs", "Male over 1.5yrs Breeding", "Male over 1.5yrs Draught", "Male over 1.5yrs Draught & Breeding", "Male over 1.5yrs Others",
                            "Female under 1yr", "Female 1-2.5yrs", "Female over 2.5yrs in milk", "Female over 2.5yrs dry", "Female over 2.5yrs not calved once", "Female over 2.5yrs others"))+
  scale_fill_brewer(palette = "Set1")+
  geom_bar(stat="identity")+xlab("Buffalo")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_buffalo.png")

remove(a,b, buffalo_data_nototal1, df1, national_buffalo,
       national_buffalo_pivot, 
       state_buffalo, state_buffalo_pivot, state_list,i, buffalo_processing)

############################################################################################### Sheep
summary(sheep_data)

sheep_data_nototal<- subset(sheep_data, select=-c(Total_Male, Total_Female))
head(sheep_data_nototal)

sheep_data_nototal1<- sheep_data_nototal %>% drop_na("Male_upto1", "Male_over1above", "Female_upto1", "Female_over1above")

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)
length(unique(sheep_data_nototal1$State_Name))

sheep_data_nototal1<-sheep_data_nototal1 %>% filter(State_Name!="Lakshadweep") #removed 4 records

sheep_data_nototal2<-sheep_data_nototal1 %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
sheep_data_nototal2<-sheep_data_nototal2 %>% mutate(State_Name=replace(State_Name, State_Name=="Puducherry", "Tamil_Nadu"))
sheep_data_nototal2<-sheep_data_nototal2 %>% mutate(State_Name=replace(State_Name, State_Name=="Dadra_", "Gujarat"))

length(unique(sheep_data_nototal2$State_Name))

orissa_sheep<- sheep_data_nototal1 %>% filter(State_Name=="Orissa")
odisha_sheep<- sheep_data_nototal1 %>% filter(State_Name=="Odisha")

# Records in both are completely different. 19th Census says Odisha, so removing Orissa records
sheep_data_nototal2<-sheep_data_nototal2 %>% filter(State_Name!="Orissa") #removed 60 records

# The complication is that District has a value== Total Count for some states in which no data is provided for all the districts in the state
# just a Total Count. This varies per state for exotic and indigenous sheep 
# I need to keep the Total count row for a state if that type of sheep has no other rows (with different districts)

state_list<-split(sheep_data_nototal2, c(sheep_data_nototal2$State_Name))

dim(state_list[[13]])

state_list_clean<-list()
sheep_clean<-function(df){
  df<- df%>% group_by(Type, Region) %>% 
    mutate(has_districts = (length(District) > 1)) %>%
    filter((District != "Total Count" | has_districts == 0)) %>%
    ungroup()
  df
}

for (i in 1:length(state_list)){
  df<-state_list[[i]]
  df1<-sheep_clean(df)
  state_list_clean[[i]]<-df1
}

dim(state_list_clean[[13]])

sheep_data_clean<-bind_rows(state_list_clean)
head(sheep_data_clean)
sheep_data_clean<- subset(sheep_data_clean,select=-c(has_districts))

national_sheep<- sheep_data_clean %>% group_by(Type,Region) %>%
  summarise_at(.vars = vars(Male_upto1, Male_over1above, Female_upto1, Female_over1above), .funs = c(sum="sum"))
national_sheep_pivot<-pivot_longer(national_sheep, 3:6)

write.csv(national_sheep_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_sheep.csv")

a<-ggplot(national_sheep_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male upto 1 yrs", "Male over 1yrs",
                            "Female upto 1yrs", "Female over 1yrs"))+
  scale_fill_brewer(palette = "Dark2")+
  geom_bar(stat="identity")+xlab("Sheep")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region+Type)
plot(a)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_sheep.png")

state_sheep<- sheep_data_clean %>% group_by(Type,Region, State_Name) %>%
  summarise_at(.vars = vars(Male_upto1, Male_over1above, Female_upto1, Female_over1above), .funs = c(sum="sum"))
state_sheep_pivot<-pivot_longer(state_sheep, 4:7)
write.csv(state_sheep_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_sheep.csv")


Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_sheep.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list_clean)){
  x<-state_list_clean[[i]]
  x<- subset(x, select=-c(has_districts))
  x_pivot<-pivot_longer(x, 2:5)
  b<-ggplot(x_pivot, aes(x=name, y=value,fill=name))+ggtitle(unique(x_pivot$State_Name))+
    scale_x_discrete(labels=c("Male upto 1 yrs", "Male over 1yrs",
                              "Female upto 1yrs", "Female over 1yrs"))+
    scale_fill_brewer(palette = "Dark2")+
    geom_bar(stat="identity")+xlab("Sheep")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region+Type)
  plot(b)
}; Sys.time(); dev.off()

remove(a,b,df,df1,national_sheep,national_sheep_pivot,odisha_sheep, orissa_sheep, sheep_data, sheep_data_clean, sheep_data_nototal,
       sheep_data_nototal1, sheep_data_nototal2, state_list, state_list_clean, state_sheep, state_sheep_pivot, x, x_pivot, i,
       rural_livestock_dir, urban_livestock_dir, sheep_clean, sheep_processing)

############################################################################################### Goat
summary(goat_data)

goat_data_nototal<-subset(goat_data, select=-c(Total_Male, Total_Female))

length(unique(goat_data_nototal$State_Name))
unique(goat_data_nototal$State_Name)

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)
goat_data_nototal<-goat_data_nototal %>% filter(State_Name!="Lakshadweep") #removed 1 records

goat_data_nototal<-goat_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
goat_data_nototal<-goat_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Puducherry", "Tamil_Nadu"))

length(unique(goat_data_nototal$State_Name))

orissa_goat<- goat_data_nototal %>% filter(State_Name=="Orissa")
odisha_goat<- goat_data_nototal %>% filter(State_Name=="Odisha")

# Records exactly the same, keeping Odisha records
goat_data_nototal<-goat_data_nototal %>% filter(State_Name!="Orissa") #removed 60 records
head(goat_data_nototal)

state_goat<- goat_data_nototal %>% group_by(Region, State_Name) %>%
  summarise_at(.vars = vars(Male_under1, Male_over1above, Female_under1, Female_over1above_Milk, Female_over1above_Dry, Female_over1above_notcalved),
               .funs = c(sum="sum") )
state_goat_pivot<-pivot_longer(state_goat, 3:8)
write.csv(state_goat_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_goat.csv")

state_list<-split(state_goat, state_goat$State_Name)
Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_goat.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)){
  x<-state_list[[i]]
  x_pivot<-pivot_longer(x, 3:8)
  b<-ggplot(x_pivot, aes(x=name, y=value,fill=name))+ggtitle(unique(x_pivot$State_Name))+
    scale_x_discrete(labels=c("Male under 1 yrs", "Male over 1yrs",
                              "Female under 1yrs", "Female over 1yrs in Milk", "Female over 1yrs Dry", "Female over 1years not calved once"))+
    scale_fill_brewer(palette = "Accent")+
    geom_bar(stat="identity")+xlab("Goat")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
  plot(b)
}; Sys.time(); dev.off()

nation_goat<- goat_data_nototal %>% group_by(Region) %>%
  summarise_at(.vars = vars(Male_under1, Male_over1above, Female_under1, Female_over1above_Milk, Female_over1above_Dry, Female_over1above_notcalved),
               .funs = c(sum="sum") )
nation_goat_pivot<-pivot_longer(nation_goat, 2:7)
write.csv(nation_goat, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//nation_goat.csv")
a<-ggplot(nation_goat_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male under 1 yrs", "Male over 1yrs",
                            "Female under 1yrs", "Female over 1yrs in Milk", "Female over 1yrs Dry", "Female over 1yrs not calved once"))+
  scale_fill_brewer(palette = "Dark2")+
  geom_bar(stat="identity")+xlab("Goat")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
plot(a)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_goat.png")

remove(a,b,goat_data,goat_data_nototal,nation_goat, nation_goat_pivot, odisha_goat, orissa_goat, state_goat, state_goat_pivot,x,x_pivot,i, goat_processing, state_list)

############################################################################################### Camel
summary(camel_data)

camel_data_nototal<-subset(camel_data, select=-c(Total_Male, Total_Female))

length(unique(camel_data_nototal$State_Name))
unique(camel_data_nototal$State_Name)

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)

camel_data_nototal<-camel_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
camel_data_nototal<-camel_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Puducherry", "Tamil_Nadu"))

length(unique(camel_data_nototal$State_Name))

orissa_camel<- camel_data_nototal %>% filter(State_Name=="Orissa")
odisha_camel<- camel_data_nototal %>% filter(State_Name=="Odisha")

# Records exactly the same, keeping Odisha records
camel_data_nototal<-camel_data_nototal %>% filter(State_Name!="Orissa") #removed 60 records
head(camel_data_nototal)

state_camel<- camel_data_nototal %>% group_by(Region, State_Name) %>%
  summarise_at(.vars = vars(Male_under4, Male_over4above, Female_under4, Female_over4above),
               .funs = c(sum="sum") )
state_camel_pivot<-pivot_longer(state_camel, 3:6)
write.csv(state_camel_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_camel.csv")

state_list<-split(state_camel, state_camel$State_Name)
Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_camel.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)){
  x<-state_list[[i]]
  x_pivot<-pivot_longer(x, 3:6)
  b<-ggplot(x_pivot, aes(x=name, y=value,fill=name))+ggtitle(unique(x_pivot$State_Name))+
    scale_x_discrete(labels=c("Male under 4 yrs", "Male over 4yrs",
                              "Female under 4yrs", "Female over 4yrs"))+
    scale_fill_brewer(palette = "PiYG")+
    geom_bar(stat="identity")+xlab("Camel")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
  plot(b)
}; Sys.time(); dev.off()

nation_camel<- camel_data_nototal %>% group_by(Region) %>%
  summarise_at(.vars = vars(Male_under4, Male_over4above, Female_under4, Female_over4above),
               .funs = c(sum="sum") )
nation_camel_pivot<-pivot_longer(nation_camel, 2:5)
write.csv(nation_camel_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_camel.csv")
a<-ggplot(nation_camel_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male under 4 yrs", "Male over 4yrs",
                            "Female under 4yrs", "Female over 4yrs"))+
  scale_fill_brewer(palette = "PiYG")+
  geom_bar(stat="identity")+xlab("camel")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
plot(a)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_camel.png")

remove(a,b,camel_data,camel_data_nototal,nation_camel, nation_camel_pivot, odisha_camel, orissa_camel, state_camel, state_camel_pivot,x,x_pivot,i, camel_processing, state_list)

############################################################################################### Horses & Ponies
summary(horses_data)

horses_data_nototal<-subset(horses_data, select=-c(Total_Male, Total_Female))

length(unique(horses_data_nototal$State_Name))
unique(horses_data_nototal$State_Name)

state_horses<- horses_data_nototal %>% group_by(Region, State_Name) %>%
  summarise_at(.vars = vars(Male_under4, Male_over4above, Female_under4, Female_over4above),
               .funs = c(sum="sum") )
state_horses_pivot<-pivot_longer(state_horses, 3:6)
write.csv(state_horses_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_horses.csv")

state_list<-split(state_horses, state_horses$State_Name)
Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_horses.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)){
  x<-state_list[[i]]
  x_pivot<-pivot_longer(x, 3:6)
  b<-ggplot(x_pivot, aes(x=name, y=value,fill=name))+ggtitle(unique(x_pivot$State_Name))+
    scale_x_discrete(labels=c("Male under 4 yrs", "Male over 4yrs",
                              "Female under 4yrs", "Female over 4yrs"))+
    scale_fill_brewer(palette = "PiYG")+
    geom_bar(stat="identity")+xlab("Horses")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
  plot(b)
}; Sys.time(); dev.off()

nation_horses<- horses_data_nototal %>% group_by(Region) %>%
  summarise_at(.vars = vars(Male_under4, Male_over4above, Female_under4, Female_over4above),
               .funs = c(sum="sum") )
nation_horses_pivot<-pivot_longer(nation_horses, 2:5)
write.csv(nation_horses_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_horses.csv")
a<-ggplot(nation_horses_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male under 4 yrs", "Male over 4yrs",
                            "Female under 4yrs", "Female over 4yrs"))+
  scale_fill_brewer(palette = "PiYG")+
  geom_bar(stat="identity")+xlab("Horses")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region)
plot(a)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_horses.png")

remove(a,b,horses_data,horses_data_nototal,nation_horses, nation_horses_pivot, odisha_horses, orissa_horses, state_horses, state_horses_pivot,x,x_pivot,i, horses_processing, state_list)

############################################################################################### Mithun & Yak
summary(mithun_data)

mithun_data_nototal<-subset(mithun_data, select=-c(Total_under3, Total_3above))

length(unique(mithun_data_nototal$State_Name))
unique(mithun_data_nototal$State_Name)

#Decided to absorb the UTs into the state they are present in and exclude islands, liek first chapter (keeping J & K as a state)

mithun_data_nototal<-mithun_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Chandigarh", "Punjab"))
mithun_data_nototal<-mithun_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Dadra_", "Gujarat"))
mithun_data_nototal<-mithun_data_nototal %>% mutate(State_Name=replace(State_Name, State_Name=="Daman_and_Diu", "Gujarat"))


length(unique(mithun_data_nototal$State_Name))

state_mithun<- mithun_data_nototal %>% group_by(Type, Region, State_Name) %>%
  summarise_at(.vars = vars(Male_under3,Female_under3, Male_3above, Female_3above),
               .funs = c(sum="sum") )
state_mithun_pivot<-pivot_longer(state_mithun, 4:7)
write.csv(state_mithun_pivot, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_mithun.csv")

state_list<-split(state_mithun, state_mithun$State_Name)
Sys.time()
pdf("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_mithun.pdf", width=8, height=18)
par(mfrow=c(5,2))
for (i in 1:length(state_list)){
  x<-state_list[[i]]
  x_pivot<-pivot_longer(x, 4:7)
  b<-ggplot(x_pivot, aes(x=name, y=value,fill=name))+ggtitle(unique(x_pivot$State_Name))+
    scale_x_discrete(labels=c("Male under 3 yrs", "Female under 3 yrs",
                              "Male above 3 yrs", "Female above 3 yrs"))+
    scale_fill_brewer(palette = "Accent")+
    geom_bar(stat="identity")+xlab("Mithun and Yak")+ylab("Total")+
    coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region+Type)
  plot(b)
}; Sys.time(); dev.off()

nation_mithun<- mithun_data_nototal %>% group_by(Region, Type) %>%
  summarise_at(.vars = vars(Male_under3,Female_under3, Male_3above, Female_3above), sum, na.rm= TRUE )
nation_mithun_pivot<-pivot_longer(nation_mithun, 3:6)
write.csv(nation_mithun, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//nation_mithun.csv")
a<-ggplot(nation_mithun_pivot, aes(x=name, y=value,fill=name))+
  scale_x_discrete(labels=c("Male under 3 yrs",
                            "Female under 3yrs", "Male over 3yrs", "Female over 3yrs"))+
  scale_fill_brewer(palette = "Dark2")+
  geom_bar(stat="identity")+xlab("MithunYak")+ylab("Total")+
  coord_polar() +theme_bw() +theme(legend.position = "none") +facet_wrap(~Region+Type)
plot(a)
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//national_mithun.png")

remove(a,b,mithun_data,mithun_data_nototal,nation_mithun, nation_mithun_pivot, odisha_mithun, orissa_mithun, state_mithun, state_mithun_pivot,x,x_pivot,i, mithun_processing, state_list)

############################################################################################### 
###############################################################################################
# Data processing to obtain dry matter intake
# Global Grided Livestock raster data does not have the level of detail- sex, urban/rural, age group, milk, calves, draft etc
# as the tabular data does. So cannot apply the IPCC dry matter intake equations. 

#For raster analyses- Applying wolf et al., 2013 metabolic rate kg DM/day equation 2 from Table 1

#Body weight - from Table 1 Singhal et al., 2005
# Adult cattle Crossbred male Working bulls range, Crossbred female milking cows range, indigenous cattle male working bulls range, indigenous cattle female milking cows
cattle_bw<-c((280+354)/2, (260+320)/2, (300+352)/2, (200+333)/2)
final_cattle_bw<-mean(cattle_bw)
# Adult buffalo - Buffalo male working bulls range, buffalo female milking buffaloes range
buffalo_bw<-c((475+550)/2, (400+516)/2)
final_buffalo_bw<-mean(buffalo_bw)
# Adult sheep- Sheep male 1-2 years range, sheep female 1-2 years range
sheep_bw<-c((30+40)/2, (25+30)/2)
final_sheep_bw<-mean(sheep_bw)
# Adult goat- Goat male 1-2 years range, Goat female 1-2 years range
goat_bw<-c((12+27)/2, (12+25.6)/2)
final_goat_bw<- mean(goat_bw)
# Adult horse- >3 years
final_horse_bw<-300

##1. cattle- 
cattle_global<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//cattle_files//6_Ct_2010_Aw.tif")
cattle_global
plot(cattle_global)

india_shp<-st_read(dsn="C:/Users/Trisha_Gopalakrishna/OneDrive - Nexus365/Paper2/Data/Admin/gadm36_IND_shp", 
                   layer="gadm36_IND_0")

cattle_india<-crop(cattle_global, india_shp)
cattle_india_mask<-mask(cattle_india, india_shp)
plot(cattle_india_mask)
cattle_india_mask

cattle_india_DM<- cattle_india_mask * (0.021*final_cattle_bw^0.716)* 365 # Assume eating is everyday
cattle_india_DM # total kgDM/yr
cattle_india_DM_Mt<-cattle_india_DM/10^9
writeRaster(cattle_india_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//cattle_DM.tif")

##2. buffalo-
buffalo_global<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//buffalo_files//6_Bf_2010_Aw.tif")
buffalo_global
plot(buffalo_global)

buffalo_india<-crop(buffalo_global, india_shp)
buffalo_india_mask<-mask(buffalo_india, india_shp)
plot(buffalo_india_mask)
buffalo_india_mask

buffalo_india_DM<- buffalo_india_mask * (0.021*final_buffalo_bw^0.716)* 365 # Assume eating is everyday
buffalo_india_DM# total kgDM/yr
buffalo_india_DM_Mt<-buffalo_india_DM/10^9
writeRaster(buffalo_india_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//buffalo_DM.tif")

##3. goat
goat_global<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//goat_files//6_Gt_2010_Aw.tif")
goat_global
plot(goat_global)

goat_india<-crop(goat_global, india_shp)
goat_india_mask<-mask(goat_india, india_shp)
plot(goat_india_mask)
goat_india_mask

goat_india_DM<- goat_india_mask * (0.021*final_goat_bw^0.716)* 365 # Assume eating is everyday
goat_india_DM # total kgDM/yr
goat_india_DM_Mt<- goat_india_DM/10^9
writeRaster(goat_india_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//goat_DM.tif")

##4. sheep
sheep_global<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//sheep_files//6_Sh_2010_Aw.tif")
sheep_global
plot(sheep_global)

sheep_india<-crop(sheep_global, india_shp)
sheep_india_mask<-mask(sheep_india, india_shp)
plot(sheep_india_mask)
sheep_india_mask

sheep_india_DM<- sheep_india_mask * (0.021*final_sheep_bw^0.716)* 365 # Assume eating is everyday
sheep_india_DM # total kgDM/yr
sheep_india_DM_Mt<-sheep_india_DM/10^9
writeRaster(sheep_india_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//sheep_DM.tif")

##5. horse
horses_global<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//horses_files//6_Ho_2010_Aw.tif")
horses_global
plot(horses_global)

horses_india<-crop(horses_global, india_shp)
horses_india_mask<-mask(horses_india, india_shp)
plot(horses_india_mask)
horses_india_mask

horses_india_DM<- horses_india_mask * (0.021*final_horse_bw^0.716)* 365 # Assume eating is everyday
horses_india_DM # total kgDM/yr
horses_india_DM_Mt<-horses_india_DM/10^9
writeRaster(horses_india_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//horse_DM.tif")


asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()

cattle_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(cattle_india_DM_Mt) + tm_raster(style='quantile', n=8,palette = "YlOrRd",title="Total Cattle Dry Matter Intake (Mt/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
cattle_map

buffalo_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(buffalo_india_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Purples",title="Total Buffalo Dry Matter Intake (Mt/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
buffalo_map

goat_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(goat_india_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Reds",title="Total Goat Dry Matter Intake (Mt/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
goat_map

sheep_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(sheep_india_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Greys",title="Total Sheep Dry Matter Intake (Mt/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sheep_map

horse_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(horses_india_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Blues",title="Total Horse Dry Matter Intake (Mt/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
horse_map

livestock_DM<-tmap_arrange(cattle_map, buffalo_map, goat_map,sheep_map, horse_map, ncol=3)
tmap_save(livestock_DM,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//livestock_DM.png", dpi = 300)

remove(livestock_DM, horse_map, sheep_map, goat_map, cattle_map, buffalo_map)
remove(sheep_global, sheep_india, sheep_india_DM, sheep_india_DM_Mt, sheep_india_mask)
remove(horses_global, horses_india, horses_india_DM, horses_india_DM_Mt, horses_india_mask)
remove(goat_global, goat_india, goat_india_DM, goat_india_DM_Mt, goat_india_mask)
remove(cattle_global, cattle_india, cattle_india_DM, cattle_india_DM_Mt, cattle_india_mask)
remove(buffalo_global, buffalo_india, buffalo_india_DM, buffalo_india_DM_Mt, buffalo_india_mask)
###############################################################################################
# Data processing to obtain dry matter intake
# Tabular census data does has lots of details like sex, urban/rural, age group, milk, calves, draft etc
# So applying IPCC guidelines for cattle only. For remaining livestock, using Wolf et al. equation 

##1. Cattle
cattle1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_cattle.csv")
head(cattle1)
names(cattle1)


# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Crossbred== exotic
# Male up to 1.5 years- <1 year in Singh et al
# Male over 1.5 years breeding - Breeding bulls in Singh et al
# Male over 1.5 years draught- working bulls
# Male over 1.5 years draught_breeding- Breeding+working bulls in Singh et al
# Male over 1.5 years others- Others in Singh et al

# Female under 1 year- Calves <1 year in Singh et al
# Female 1-2.5- Calves 1-3 years in Singh et al
# Female over 2.5 years Milk- Miling Cows in Singh et al
# Female over 2.5 dry- Dry Cows
# Female over 2.5 years no calved- heifer in Singh et al
# Female over 2.5 others- Others in Singh et al

cattle1<- cattle1 %>% pivot_longer(5:15)
head(cattle1)
str(cattle1)
unique(cattle1$name)
cattle1<- cattle1 %>% mutate(name=str_replace_all(name, c("Male_upto1.5" ="Male calves<1yr","Male_over1.5_breeding"= "Breeding bulls",
                                        "Male_over1.5_draught"= "Working bulls","Male_over1.5_draught_breeding" = "Breeding_working bulls",
                                        "Male_over1.5_others"="Male others", "Female_under1"="Female calves<1yr",
                                        "Female_1_2.5"="Female calves1_3yr", "Female_over2.5_Milk"="Milking cows",
                                        "Female_over2.5_Dry"="Dry cows", "Female_over2.5_notcalved"="Heifers",
                                        "Female_over2.5_others"= "Female others")))
unique(cattle1$name)
# from Singh et al., 2005
cattle1<- cattle1 %>% mutate(BW=case_when(
  name=="Male calves<1yr"~mean(70,88.5),
  name=="Breeding bulls"~mean(280,354),
  name=="Working bulls"~ mean(280, 354),
  name=="Working bulls_breeding"~ mean(280,354),
  name=="Male others"~ mean(266,336),
  name== "Female calves<1yr"~ mean(75,88),
  name=="Female calves1_3yr" ~ mean(165,194),
  name=="Milking cows" ~ mean(300,352),
  name== "Dry cows" ~ mean(300, 352),
  name== "Heifers" ~ mean(165, 194),
  name== "Female others"~ mean (200, 330)
) )

# Using IPCC Feed intake edtimates using a simplified Tier 2 method (Pg 29/209 in PDF viewer)
# Dry matter intake for calves Eqn 10.17- # Assume that this is for both male and female calves and low quality forage (eg: straws and mature grasses)
# Dry matter intake for growing cattle Eqn 10.18 # Assume that this is for female calves 1-3 years
# Dry matter intake for steers and bulls Eqn 10.18a # Assume that this is for Working and Breeding bulls
# Dry matter intake for heifers Eqn 10.18a # Assume that this is for heifers
# Dry matter intake for lactating dairy cows Eqn 10.18b # Assume that this is for Milking cows
###### No eqns for other females and males,dry cows, breeding and working bulls #######

cattle1 <- cattle1 %>% mutate (kgDM_perday = case_when(
  name =="Male calves<1yr" | name== "Female calves<1yr" ~ BW^0.75*((0.0582*mean(3.5,5.5)-0.00266*mean(3.5,5.5)^2-0.1128)/0.239*mean(3.5,5.5)),
  name =="Female calves1_3yr" ~ BW^0.75*((0.0582*mean(3.5,5.5)-0.00266^mean(3.5,5.5)^2-0.0869)/0.239*mean(3.5,5.5)),
  name=="Breeding bulls" | name=="Working bulls" ~ 3.83+(0.0143*BW*0.96),
  name=="Heifers"~ 3.184+(0.01536*BW*0.96),
  name=="Milking cows" ~0.0185*BW+0.305*(3.5/100),
  name=="Male others" | name=="Working bulls_breeding"| name=="Female others"| name=="Dry cows"~0.021*BW^0.716
))
head(cattle1)

cattle1<-cattle1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
cattle1<-cattle1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
cattle1<-cattle1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(cattle1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//cattle_state_DM.csv")

national_cattle_MtDM_peryear<- cattle1 %>% summarise(sum=sum(totalMtDM_peryear))
national_cattle_MtDM_peryear #~928 MtDM per year

state_cattle_DM<- cattle1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_cattle_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle("Cattle Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_cattle_totalDM_peryear.png", dpi=300)

##2. Buffalo
buffalo1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_buffalo.csv")
head(buffalo1)
names(buffalo1)
unique(buffalo1$name)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Male up to 2 years- <1 year in Singh et al
# Male over 2 years breeding - Breeding bulls in Singh et al
# Male over 2 years draught- working bulls
# Male over 2 years draught_breeding- Breeding+working bulls in Singh et al
# Male over 2 years others- Others in Singh et al

# Female under 1 year- Calves <1 year in Singh et al
# Female 1-3- Calves 1-3 years in Singh et al
# Female over 3 years Milk- Milking buffalos in Singh et al
# Female over 3 dry- Dry buffalos
# Female over 3 years no calved- heifer in Singh et al
# Female over 3 others- Others in Singh et al

buffalo1<- buffalo1 %>% mutate(name=str_replace_all(name, c("Male_upto2_sum" ="Male calves<1yr",
                                                            "Male_over2_breeding_sum"= "Breeding bulls",
                                                          "Male_over2_draught_sum"= "Working bulls",
                                                          "Male_over2_draught_breeding_sum" = "Working bulls_breeding",
                                                          "Male_over2_others_sum"="Male others", 
                                                          "Female_under1_sum"="Female calves<1yr",
                                                          "Female_1_3_sum"="Female calves1_3yr", 
                                                          "Female_over3_Milk_sum"="Milking cows",
                                                          "Female_over3_Dry_sum"="Dry cows",
                                                          "Female_over3_notcalved_sum"="Heifers",
                                                          "Female_over3_others_sum"= "Female others")))
unique(buffalo1$name)
# from Singh et al., 2005
buffalo1<- buffalo1 %>% mutate(BW=case_when(
  name=="Male calves<1yr"~mean(65,80),
  name=="Breeding bulls"~mean(260,320),
  name=="Working bulls"~ mean(260, 320),
  name=="Working bulls_breeding"~ mean(260,320),
  name=="Male others"~ mean(247,285),
  name== "Female calves<1yr"~ mean(65,75),
  name=="Female calves1_3yr" ~ mean(136,157),
  name=="Milking cows" ~ mean(200,333),
  name== "Dry cows" ~ mean(200, 363),
  name== "Heifers" ~ mean(200, 250),
  name== "Female others"~ mean (200, 330)
) )
head(buffalo1)

buffalo1 <- buffalo1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(buffalo1)

buffalo1<-buffalo1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
buffalo1<-buffalo1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
buffalo1<-buffalo1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(buffalo1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//buffalo_state_DM.csv")

national_buffalo_MtDM_peryear<- buffalo1 %>% summarise(sum=sum(totalMtDM_peryear))
national_buffalo_MtDM_peryear #~24.9377 MtDM per year

state_buffalo_DM<- buffalo1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_buffalo_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("Buffalo Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_buffalo_totalDM_peryear.png", dpi=300)

##3. Sheep
sheep1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_sheep.csv")
head(sheep1)
names(sheep1)
unique(sheep1$name)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Male up to 1 years- <1 year in Singh et al
# Male over 1 years- 1-2 Singh et al

# Female under 1 year- Calves <1 year in Singh et al
# Female over 1 years- 1-2 years in Singh et al

sheep1<- sheep1 %>% mutate(name=str_replace_all(name, c("Male_upto1_sum" ="Male<1yr",
                                                            "Male_over1above_sum"= "Male_1_2yrs",
                                                            "Female_upto1_sum"="Female<1yr",
                                                           "Female_over1above_sum"="Female_1_2yrs")))
head(sheep1)
unique(sheep1$name)
# from Singh et al., 2005
sheep1<- sheep1 %>% mutate(BW=case_when(
  name=="Male<1yr"~mean(14,22),
  name=="Male_1_2yrs"~ mean(30,40),
  name=="Female<1yr"~mean(14,22),
  name=="Female_1_2yrs"~mean(25,30)
))
head(sheep1)

sheep1 <- sheep1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(sheep1)

sheep1<-sheep1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
sheep1<-sheep1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
sheep1<-sheep1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(sheep1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//sheep_state_DM.csv")

national_sheep_MtDM_peryear<- sheep1 %>% summarise(sum=sum(totalMtDM_peryear))
national_sheep_MtDM_peryear #~4.060651 MtDM per year

state_sheep_DM<- sheep1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_sheep_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("sheep Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_sheep_totalDM_peryear.png", dpi=300)

##4. Goat
goat1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_goat.csv")
head(goat1)
names(goat1)
unique(goat1$name)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Male up to 1 years- <1 year in Singh et al
# Male over 1 years- 1-2 Singh et al

# Female under 1 year- Calves <1 year in Singh et al
# Female over 1 years- 1-2 years in Singh et al
# No distinction between females over 1 year that are in milk, dry or not calved. so assumed BW as female over 1 year for all three categories

goat1<- goat1 %>% mutate(name=str_replace_all(name, c("Male_under1_sum" ="Male<1yr",
                                                        "Male_over1above_sum"= "Male_1_2yrs",
                                                        "Female_under1_sum"="Female<1yr",
                                                        "Female_over1above_Milk_sum"="Female_1_2yrs",
                                                        "Female_over1above_Dry_sum"="Female_1_2yrs",
                                                        "Female_over1above_notcalved_sum"="Female_1_2yrs")))
head(goat1)
unique(goat1$name)
# from Singh et al., 2005
goat1<- goat1 %>% mutate(BW=case_when(
  name=="Male<1yr"~mean(8.8,21.7),
  name=="Male_1_2yrs"~ mean(12,27),
  name=="Female<1yr"~mean(8.8,21.7),
  name=="Female_1_2yrs"~mean(12,25.6)
))
head(goat1)

goat1 <- goat1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(goat1)

goat1<-goat1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
goat1<-goat1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
goat1<-goat1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(goat1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//goat_state_DM.csv")

national_goat_MtDM_peryear<- goat1 %>% summarise(sum=sum(totalMtDM_peryear))
national_goat_MtDM_peryear #~4.88 MtDM per year

state_goat_DM<- goat1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_goat_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("Goat Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_goat_totalDM_peryear.png", dpi=300)

##5. Camel
camel1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_camel.csv")
head(camel1)
names(camel1)
unique(camel1$name)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# No data for camels (female or male) below 4 years. Only over 4 years, no female or male specific BW
camel1<- camel1 %>% mutate(name=str_replace_all(name, c("Male_under4_sum" ="Male<4yr",
                                                      "Male_over4above_sum"= "Male>4yrs",
                                                      "Female_under4_sum"="Female<4yr",
                                                      "Female_over4above_sum"="Female>4yrs")))
head(camel1)
unique(camel1$name)
# from Singh et al., 2005
camel1<- camel1 %>% mutate(BW=case_when(
  name=="Male<4yr"~0,
  name=="Male>4yrs"~ 300,
  name=="Female<4yr"~0,
  name=="Female>4yrs"~300
))
head(camel1)

camel1 <- camel1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(camel1)

camel1<-camel1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
camel1<-camel1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
camel1<-camel1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(camel1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//camel_state_DM.csv")

national_camel_MtDM_peryear<- camel1 %>% summarise(sum=sum(totalMtDM_peryear))
national_camel_MtDM_peryear #~0.1308 MtDM per year

state_camel_DM<- camel1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_camel_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("Camel Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_camel_totalDM_peryear.png", dpi=300)

##6. Horses
horse1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_horses.csv")
head(horse1)
names(horse1)
unique(horse1$name)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Male_under4_sum and Female_under_4_sum- Assume this is Horses below 3 yrs
# Above 4 years in above 3 years
# No split between male and female
horse1<- horse1 %>% mutate(name=str_replace_all(name, c("Male_under4_sum" ="Male<3yr",
                                                        "Male_over4above_sum"= "Male>3yrs",
                                                        "Female_under4_sum"="Female<3yr",
                                                        "Female_over4above_sum"="Female>3yrs")))
head(horse1)
unique(horse1$name)
# from Singh et al., 2005
horse1<- horse1 %>% mutate(BW=case_when(
  name=="Male<3yr"~200,
  name=="Male>3yrs"~ 300,
  name=="Female<3yr"~200,
  name=="Female>3yrs"~300
))
head(horse1)

horse1 <- horse1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(horse1)

horse1<-horse1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
horse1<-horse1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
horse1<-horse1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(horse1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//horse_state_DM.csv")

national_horse_MtDM_peryear<- horse1 %>% summarise(sum=sum(totalMtDM_peryear))
national_horse_MtDM_peryear #~0.0081197 MtDM per year

state_horse_DM<- horse1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_horse_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("Horse Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_horse_totalDM_peryear.png", dpi=300)

##7. Yak
yak1<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//state_mithun.csv")
head(yak1)
names(yak1)
unique(yak1$name)
unique(yak1$Type)

# Redoing the categories of age and breed and sex according to Singh et al., 2005 Table 1, to easily extract body weight 
# Only yak and mithun BW provided in Singh et al i.e. no data by sex or age. So assigning BW based on Tupe column

# from Singh et al., 2005
yak1<- yak1 %>% mutate(BW=case_when(
  Type=="Mithun"~400,
  Type=="MIthun"~400,
  Type=="Yak"~300
))
head(yak1)
summary(yak1)
yak1[is.na(yak1$value),]

yak1 <- yak1 %>% mutate (kgDM_perday=0.021*BW^0.716) 
head(yak1)

yak1<-yak1 %>% mutate(totalkgDM_perday= value*kgDM_perday)
yak1<-yak1 %>% mutate(totalkgDM_peryear= totalkgDM_perday*365)
yak1<-yak1 %>% mutate(totalMtDM_peryear= totalkgDM_peryear/10^9)
write.csv(yak1, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//yak_state_DM.csv")

national_yak_MtDM_peryear<- yak1 %>% summarise(sum=sum(totalMtDM_peryear, na.rm = TRUE))
national_yak_MtDM_peryear #~0.06498 MtDM per year

state_yak_DM<- yak1 %>% group_by(State_Name) %>% summarise(TotalMtDM_peryear=sum(totalMtDM_peryear))

p<- ggplot(data = state_yak_DM, aes(x= State_Name, y= TotalMtDM_peryear))+ geom_bar(stat = "identity")+ggtitle ("Mithun and Yak Dry Matter intake (Mt/yr)")+coord_flip()
p
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//state_yak_totalDM_peryear.png", dpi=300)

remove(yak1,buffalo1, cattle1, camel1, goat1, horse1, national_buffalo_MtDM_peryear, national_camel_MtDM_peryear,
       national_cattle_MtDM_peryear, national_goat_MtDM_peryear, national_sheep_MtDM_peryear, national_yak_MtDM_peryear,p,sheep1,
       national_horse_MtDM_peryear)
remove(national)
###############################################################################################
#Comparison of GLW3 DM calculations vs India Census DM calculations for each livestock

##1. Cattle 
india_states<-readOGR("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Admin//gadm36_IND_shp//gadm36_IND_1.shp")
india_shp
(india_states)
proj4string(india_states)<-CRS("+proj=longlat +datum=WGS84 +no_defs") 
india_states    
india_states<-st_as_sf(india_states)
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

india_states_prepped<-india_states %>% group_by(Final_State_Name) %>%summarise()
unique(india_states_prepped$Final_State_Name)
st_write(india_states_prepped, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Admin//india_states_prepped.shp")
remove(india_states)

cattle_state<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//cattle_state_DM.csv")
state_cattle<- cattle_state %>% group_by(State_Name) %>% summarise(totalMtDM_peryear=sum(totalMtDM_peryear, na.rm = TRUE))
remove(cattle_state)
head(state_cattle)
state_cattle<- state_cattle %>% mutate(name=str_replace_all(State_Name, c("Andhra_Pradesh" ="Andhra Pradesh",
                                                                          "Arunachal_Pradesh"= "Arunachal Pradesh",
                                                                          "Himachal_Pradesh"="Himachal Pradesh",
                                                                          "Jammu_and_Kashmir"="Jammu and Kashmir",
                                                                          "Madhya_Pradesh"="Madhya Pradesh",
                                                                          "Tamil_Nadu"="Tamil Nadu",
                                                                          "Uttar_Pradesh"= "Uttar Pradesh")))

cattle1<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//cattle_DM.tif")
cattle1
s_cattle_MtDM_year<-raster::extract(cattle1, india_states_prepped, fun=sum,na.rm=TRUE, df=TRUE,sp=TRUE )
sum(s_cattle_MtDM_year$cattle_DM) ##90.129 MtDM/year as opposed to 928 MtDM per year. This difference is huge!
s_cattle_MtDM_year<-st_as_sf(s_cattle_MtDM_year)

s_cattle_data<-st_drop_geometry(s_cattle_MtDM_year) 
remove(s_cattle_MtDM_year)
head(s_cattle_data)

s_cattle_data<-left_join(s_cattle_data, state_cattle, by=c("Final_State_Name"= "name"))
head(s_cattle_data)
s_cattle_data_pivot<-pivot_longer(s_cattle_data, c(2,4))
head(s_cattle_data_pivot)
s_cattle_data_pivot<-s_cattle_data_pivot %>% mutate(name=str_replace_all(name,c("cattle_DM"="GLW 3+ Wolf et al., 2013",
                                                                                "totalMtDM_peryear"="India Census Data+ IPCC equations")))

a<-ggplot(s_cattle_data_pivot, aes(Final_State_Name, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("States")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ggtitle("Cattle")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_cattleDM_GLW3_IndiaCensus.png", dpi=300)

s_cattle_data<- s_cattle_data %>% mutate(percentagediff=ifelse(is.na(totalMtDM_peryear),NA,
                                                               (cattle_DM-totalMtDM_peryear)/(cattle_DM)*100))

remove(cattle1, s_cattle_data_filter, state_cattle)
###All values from GLW3 and Wolf et al equation with average BW of adult females and males is 
### less than india census method by a lot! Ask someone

##2. Buffalo
buffalo1<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//buffalo_DM.tif")
buffalo1
s_buffalo_MtDM_year<-raster::extract(buffalo1, india_states_prepped, fun=sum,na.rm=TRUE, df=TRUE,sp=TRUE )
sum(s_buffalo_MtDM_year$buffalo_DM) ##69.1916 MtDM/year as opposed to 24.9377 MtDM per year. Not too bad, but not too good either
s_buffalo_MtDM_year<-st_as_sf(s_buffalo_MtDM_year)

s_buffalo_data<-st_drop_geometry(s_buffalo_MtDM_year) 
remove(s_buffalo_MtDM_year)
head(s_buffalo_data)

buffalo_state<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//buffalo_state_DM.csv")
state_buffalo<- buffalo_state %>% group_by(State_Name) %>% summarise(totalMtDM_peryear=sum(totalMtDM_peryear, na.rm = TRUE))
remove(buffalo_state)
head(state_buffalo)
state_buffalo<- state_buffalo %>% mutate(name=str_replace_all(State_Name, c("Andhra_Pradesh" ="Andhra Pradesh",
                                                                          "Arunachal_Pradesh"= "Arunachal Pradesh",
                                                                          "Himachal_Pradesh"="Himachal Pradesh",
                                                                          "Jammu_and_Kashmir"="Jammu and Kashmir",
                                                                          "Madhya_Pradesh"="Madhya Pradesh",
                                                                          "Tamil_Nadu"="Tamil Nadu",
                                                                          "Uttar_Pradesh"= "Uttar Pradesh")))

s_buffalo_data<-left_join(s_buffalo_data, state_buffalo, by=c("Final_State_Name"= "name"))
head(s_buffalo_data)
s_buffalo_data_pivot<-pivot_longer(s_buffalo_data, c(2,4))
head(s_buffalo_data_pivot)
s_buffalo_data_pivot<-s_buffalo_data_pivot %>% mutate(name=str_replace_all(name,c("buffalo_DM"="GLW 3+ Wolf et al., 2013 (average BW)",
                                                                                "totalMtDM_peryear"="India Census Data+ Wolf et al., 2013(specific BW)")))

b<-ggplot(s_buffalo_data_pivot, aes(Final_State_Name, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("States")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ ggtitle("Buffalo")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_buffaloDM_GLW3_IndiaCensus.png", dpi=300)

remove(buffalo1, state_buffalo)
##3. Sheep
sheep1<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//sheep_DM.tif")
sheep1
s_sheep_MtDM_year<-raster::extract(sheep1, india_states_prepped, fun=sum,na.rm=TRUE, df=TRUE,sp=TRUE )
sum(s_sheep_MtDM_year$sheep_DM) ##6.4488MtDM/year as opposed to 4.060651 MtDM per year. Not too bad, but not too good either
s_sheep_MtDM_year<-st_as_sf(s_sheep_MtDM_year)

s_sheep_data<-st_drop_geometry(s_sheep_MtDM_year) 
remove(s_sheep_MtDM_year)
head(s_sheep_data)

sheep_state<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//sheep_state_DM.csv")
state_sheep<- sheep_state %>% group_by(State_Name) %>% summarise(totalMtDM_peryear=sum(totalMtDM_peryear, na.rm = TRUE))
remove(sheep_state)
head(state_sheep)
state_sheep<- state_sheep %>% mutate(name=str_replace_all(State_Name, c("Andhra_Pradesh" ="Andhra Pradesh",
                                                                            "Arunachal_Pradesh"= "Arunachal Pradesh",
                                                                            "Himachal_Pradesh"="Himachal Pradesh",
                                                                            "Jammu_and_Kashmir"="Jammu and Kashmir",
                                                                            "Madhya_Pradesh"="Madhya Pradesh",
                                                                            "Tamil_Nadu"="Tamil Nadu",
                                                                            "Uttar_Pradesh"= "Uttar Pradesh")))

s_sheep_data<-left_join(s_sheep_data, state_sheep, by=c("Final_State_Name"= "name"))
head(s_sheep_data)
s_sheep_data_pivot<-pivot_longer(s_sheep_data, c(2,4))
head(s_sheep_data_pivot)
s_sheep_data_pivot<-s_sheep_data_pivot %>% mutate(name=str_replace_all(name,c("sheep_DM"="GLW 3+ Wolf et al., 2013 (average BW)",
                                                                                  "totalMtDM_peryear"="India Census Data+ Wolf et al., 2013(specific BW)")))

c<-ggplot(s_sheep_data_pivot, aes(Final_State_Name, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("States")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ ggtitle("Sheep")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_sheepDM_GLW3_IndiaCensus.png", dpi=300)

remove(sheep1, state_sheep)

##4. Goat
goat1<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//goat_DM.tif")
goat1
s_goat_MtDM_year<-raster::extract(goat1, india_states_prepped, fun=sum,na.rm=TRUE, df=TRUE,sp=TRUE)
sum(s_goat_MtDM_year$goat_DM) ##8.865389 MtDM/year as opposed to 4.88 MtDM per year MtDM per year. Its twice! 
s_goat_MtDM_year<-st_as_sf(s_goat_MtDM_year)

s_goat_data<-st_drop_geometry(s_goat_MtDM_year) 
remove(s_goat_MtDM_year)
head(s_goat_data)

goat_state<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//goat_state_DM.csv")
state_goat<- goat_state %>% group_by(State_Name) %>% summarise(totalMtDM_peryear=sum(totalMtDM_peryear, na.rm = TRUE))
remove(goat_state)
head(state_goat)
state_goat<- state_goat %>% mutate(name=str_replace_all(State_Name, c("Andhra_Pradesh" ="Andhra Pradesh",
                                                                        "Arunachal_Pradesh"= "Arunachal Pradesh",
                                                                        "Himachal_Pradesh"="Himachal Pradesh",
                                                                        "Jammu_and_Kashmir"="Jammu and Kashmir",
                                                                        "Madhya_Pradesh"="Madhya Pradesh",
                                                                        "Tamil_Nadu"="Tamil Nadu",
                                                                        "Uttar_Pradesh"= "Uttar Pradesh")))

s_goat_data<-left_join(s_goat_data, state_goat, by=c("Final_State_Name"= "name"))
head(s_goat_data)
s_goat_data_pivot<-pivot_longer(s_goat_data, c(2,4))
head(s_goat_data_pivot)
s_goat_data_pivot<-s_goat_data_pivot %>% mutate(name=str_replace_all(name,c("goat_DM"="GLW 3+ Wolf et al., 2013 (average BW)",
                                                                              "totalMtDM_peryear"="India Census Data+ Wolf et al., 2013(specific BW)")))

d<-ggplot(s_goat_data_pivot, aes(Final_State_Name, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("States")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ ggtitle("Goat")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_goatDM_GLW3_IndiaCensus.png", dpi=300)

remove(state_goat, goat1)
##5. Horse
horse1<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Processed//horse_DM.tif")
horse1
s_horse_MtDM_year<-raster::extract(horse1, india_states_prepped, fun=sum,na.rm=TRUE, df=TRUE,sp=TRUE)
sum(s_horse_MtDM_year$horse_DM) ##0.3572556 MtDM/year as opposed to 0.0081197 MtDM per year MtDM per year. Ok not so close still
s_horse_MtDM_year<-st_as_sf(s_horse_MtDM_year)

s_horse_data<-st_drop_geometry(s_horse_MtDM_year) 
remove(s_horse_MtDM_year)
head(s_horse_data)

horse_state<-read.csv("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//Indian_Census_Processed//horse_state_DM.csv")
state_horse<- horse_state %>% group_by(State_Name) %>% summarise(totalMtDM_peryear=sum(totalMtDM_peryear, na.rm = TRUE))
remove(horse_state)
head(state_horse)
state_horse<- state_horse %>% mutate(name=str_replace_all(State_Name, c("Andhra_Pradesh" ="Andhra Pradesh",
                                                                      "Arunachal_Pradesh"= "Arunachal Pradesh",
                                                                      "Himachal_Pradesh"="Himachal Pradesh",
                                                                      "Jammu_and_Kashmir"="Jammu and Kashmir",
                                                                      "Madhya_Pradesh"="Madhya Pradesh",
                                                                      "Tamil_Nadu"="Tamil Nadu",
                                                                      "Uttar_Pradesh"= "Uttar Pradesh")))

s_horse_data<-left_join(s_horse_data, state_horse, by=c("Final_State_Name"= "name"))
head(s_horse_data)
s_horse_data_pivot<-pivot_longer(s_horse_data, c(2,4))
head(s_horse_data_pivot)
s_horse_data_pivot<-s_horse_data_pivot %>% mutate(name=str_replace_all(name,c("horse_DM"="GLW 3+ Wolf et al., 2013 (average BW)",
                                                                            "totalMtDM_peryear"="India Census Data+ Wolf et al., 2013(specific BW)")))

e<-ggplot(s_horse_data_pivot, aes(Final_State_Name, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("States")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ ggtitle("Horse")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_horseDM_GLW3_IndiaCensus.png", dpi=300)

remove(state_horse, horse1)

library(ggpubr)
ggarrange(a,b,c,d,e, ncol = 2, nrow = 3)

##Looking at the graphs, it seems like except for cattle, the GLW data with average BW provides higher total dry matter intake (pink)
## than indian census data. Additionally, GLW data is more complete than Indian census data. 
## For cattle, Indian census data with IPCC equations predicts a lot more dry matter intake (by a lot more). Lot of state variation 
## but overall this is the bottom line

## Discrpeency for cattle is clear. It is the IPCC equations! They seem to predict a lot more DM for different age groups of cattle
## But for the remaining, its interesting that avergae BW of adult male and female provides more DM than finer data. I guess
## this is expected. However considering the completness of GW data, I am inclined to use it instead of India census data. 

remove(a,b,c,d,e)

head(s_cattle_data)
national_cattle<-s_cattle_data %>% summarise_at(.vars = vars(cattle_DM, totalMtDM_peryear),
                                                .funs = c(sum="sum"), na.rm= TRUE)
national_cattle
national_cattle<-national_cattle %>%  rename(GLW_WolfEqn_AvBW=cattle_DM_sum,
                                             IndiaCensus_WolfEqn_SpecificBW=totalMtDM_peryear_sum)
national_cattle<- national_cattle %>% mutate(Type="Cattle")


national_buffalo<-s_buffalo_data %>% summarise_at(.vars = vars(buffalo_DM, totalMtDM_peryear),
                                                      .funs = c(sum="sum"), na.rm=TRUE)
national_buffalo<-national_buffalo %>%  rename(GLW_WolfEqn_AvBW=buffalo_DM_sum,
                                             IndiaCensus_WolfEqn_SpecificBW=totalMtDM_peryear_sum)
national_buffalo<- national_buffalo %>% mutate(Type="Buffalo")

national_sheep<-s_sheep_data %>% summarise_at(.vars = vars(sheep_DM, totalMtDM_peryear),
                                                  .funs = c(sum="sum"), na.rm=TRUE)
national_sheep<-national_sheep %>%  rename(GLW_WolfEqn_AvBW=sheep_DM_sum,
                                             IndiaCensus_WolfEqn_SpecificBW=totalMtDM_peryear_sum)
national_sheep<- national_sheep %>% mutate(Type="Sheep")

national_goat<-s_goat_data %>% summarise_at(.vars = vars(goat_DM, totalMtDM_peryear),
                                                  .funs = c(sum="sum"), na.rm=TRUE)
national_goat<-national_goat %>%  rename(GLW_WolfEqn_AvBW=goat_DM_sum,
                                             IndiaCensus_WolfEqn_SpecificBW=totalMtDM_peryear_sum)
national_goat<- national_goat %>% mutate(Type="Goat")

national_horse<-s_horse_data %>% summarise_at(.vars = vars(horse_DM, totalMtDM_peryear),
                                                  .funs = c(sum="sum"), na.rm=TRUE)
national_horse<-national_horse %>%  rename(GLW_WolfEqn_AvBW=horse_DM_sum,
                                             IndiaCensus_WolfEqn_SpecificBW=totalMtDM_peryear_sum)
national_horse<- national_horse %>% mutate(Type="Horse")

national_livestockDM<-bind_rows(national_cattle, national_buffalo, national_sheep, national_goat, national_horse)
head(national_livestockDM)

national_livestockDM_pivot<-pivot_longer(national_livestockDM, 1:2)
national_livestockDM_pivot

ggplot(national_livestockDM_pivot, aes(Type, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity")+theme(axis.text.x = element_text(angle=90))+
  xlab("Livestock Type")+ylab("Mt Dry Matter Intake per year")+scale_fill_manual(values = c("#CC0066", "#666666"))+
  theme(legend.title = element_blank())+ ggtitle("National Estimates of Dry Matter Intake")
ggsave("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//descrepancy_national_allDM_GLW3_IndiaCensus.png", dpi= 300)
#####################################################################################################################

## Confirmed from Gilbert et al., 2018 that to calculate density I need to divide each raster which is absolute animal numbers
## (when summed within census polygon) with other area raster provided for each livestock 

##1. Cattle
cattle<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//cattle_files//6_Ct_2010_Aw.tif")
cattle
plot(cattle)
cattle_area<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//cattle_files//8_Areakm.tif")
cattle_area #sqkm/pixel

cattle_density_stack<-stack(cattle, cattle_area)
cattle_density_stack_india<-crop(cattle_density_stack, india_shp)
cattle_density_stack_india<-mask(cattle_density_stack_india, india_shp)

cattle_density_final<-cattle_density_stack_india[[1]]/cattle_density_stack_india[[2]]
cattle_density_final #animals/sq km
plot(cattle_density_final)

##Dry Matter- Wolf eqn 
cattle_density_DM<- cattle_density_final * (0.021*final_cattle_bw^0.716)* 365 # Assume eating is everyday
cattle_density_DM # total kgDM/yr/sqkm
cattle_density_DM_Mt<-cattle_density_DM/10^9
plot(cattle_density_DM_Mt)
writeRaster(cattle_density_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//cattle_density_DM.tif")

remove(cattle, cattle_area, cattle_density_stack, cattle_density_stack_india, cattle_density_final)

##2. Buffalo
buffalo<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//buffalo_files//6_Bf_2010_Aw.tif")
buffalo
plot(buffalo)
buffalo_area<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//buffalo_files//8_Areakm.tif")
buffalo_area #sqkm/pixel

buffalo_density_stack<-stack(buffalo, buffalo_area)
buffalo_density_stack_india<-crop(buffalo_density_stack, india_shp)
buffalo_density_stack_india<-mask(buffalo_density_stack_india, india_shp)

buffalo_density_final<-buffalo_density_stack_india[[1]]/buffalo_density_stack_india[[2]]
buffalo_density_final #animals/sq km
plot(buffalo_density_final)

##Dry Matter- Wolf eqn 
buffalo_density_DM<- buffalo_density_final * (0.021*final_buffalo_bw^0.716)* 365 # Assume eating is everyday
buffalo_density_DM # total kgDM/yr/sqkm
buffalo_density_DM_Mt<-buffalo_density_DM/10^9
plot(buffalo_density_DM_Mt)
writeRaster(buffalo_density_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//buffalo_density_DM.tif")

remove(buffalo, buffalo_area, buffalo_density_stack, buffalo_density_stack_india, buffalo_density_final)

##3. sheep
sheep<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//sheep_files//6_Sh_2010_Aw.tif")
sheep
plot(sheep)
sheep_area<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//sheep_files//8_Areakm.tif")
sheep_area #sqkm/pixel

sheep_density_stack<-stack(sheep, sheep_area)
sheep_density_stack_india<-crop(sheep_density_stack, india_shp)
sheep_density_stack_india<-mask(sheep_density_stack_india, india_shp)

sheep_density_final<-sheep_density_stack_india[[1]]/sheep_density_stack_india[[2]]
sheep_density_final #animals/sq km
plot(sheep_density_final)

##Dry Matter- Wolf eqn 
sheep_density_DM<- sheep_density_final * (0.021*final_sheep_bw^0.716)* 365 # Assume eating is everyday
sheep_density_DM # total kgDM/yr/sqkm
sheep_density_DM_Mt<-sheep_density_DM/10^9
plot(sheep_density_DM_Mt)
writeRaster(sheep_density_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//sheep_density_DM.tif")

remove(sheep, sheep_area, sheep_density_stack, sheep_density_stack_india, sheep_density_final)

##4. goat
goat<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//goat_files//6_Gt_2010_Aw.tif")
goat
plot(goat)
goat_area<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//goat_files//8_Areakm.tif")
goat_area #sqkm/pixel

goat_density_stack<-stack(goat, goat_area)
goat_density_stack_india<-crop(goat_density_stack, india_shp)
goat_density_stack_india<-mask(goat_density_stack_india, india_shp)

goat_density_final<-goat_density_stack_india[[1]]/goat_density_stack_india[[2]]
goat_density_final #animals/sq km
plot(goat_density_final)

##Dry Matter- Wolf eqn 
goat_density_DM<- goat_density_final * (0.021*final_goat_bw^0.716)* 365 # Assume eating is everyday
goat_density_DM # total kgDM/yr/sqkm
goat_density_DM_Mt<-goat_density_DM/10^9
plot(goat_density_DM_Mt)
writeRaster(goat_density_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//goat_density_DM.tif")

remove(goat, goat_area, goat_density_stack, goat_density_stack_india, goat_density_final)

##4. horse
horse<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//horses_files//6_Ho_2010_Aw.tif")
horse
plot(horse)
horse_area<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//GriddedLivestock//horses_files//8_Areakm.tif")
horse_area #sqkm/pixel

horse_density_stack<-stack(horse, horse_area)
horse_density_stack_india<-crop(horse_density_stack, india_shp)
horse_density_stack_india<-mask(horse_density_stack_india, india_shp)

horse_density_final<-horse_density_stack_india[[1]]/horse_density_stack_india[[2]]
horse_density_final #animals/sq km
plot(horse_density_final)

##Dry Matter- Wolf eqn 
horse_density_DM<- horse_density_final * (0.021*final_horse_bw^0.716)* 365 # Assume eating is everyday
horse_density_DM # total kgDM/yr/sqkm
horse_density_DM_Mt<-horse_density_DM/10^9
plot(horse_density_DM_Mt)
writeRaster(horse_density_DM_Mt, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//horse_density_DM.tif")

remove(horse, horse_area, horse_density_stack, horse_density_stack_india, horse_density_final)


asia<- st_read("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper1//Data//Admin//Longitude_Graticules_and_World_Countries_Boundaries-shp//asian_countries_map2.shp") 
india_boundary_roy<-st_read("C:\\Users\\Trisha_Gopalakrishna\\OneDrive - Nexus365\\Paper1\\Data\\Admin\\gadm36_IND_shp\\India_bound.shp")

map_extent<- st_bbox(c(xmin=63.7, xmax=98.3,
                       ymin=5.8, ymax=39), crs=4326) %>% st_as_sfc()

cattle_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(cattle_density_DM_Mt) + tm_raster(style='quantile', n=8,palette = "YlOrRd",title="Total Cattle Dry Matter Intake (Mt/sqkm/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
cattle_map

buffalo_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(buffalo_density_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Purples",title="Total Buffalo Dry Matter Intake (Mt/sqkm/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
buffalo_map

goat_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(goat_density_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Reds",title="Total Goat Dry Matter Intake (Mt/sqkm/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
goat_map

sheep_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(sheep_density_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Greys",title="Total Sheep Dry Matter Intake (Mt/sqkm/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
sheep_map

horse_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(horse_density_DM_Mt) + tm_raster(style='quantile', n=8,palette = "Blues",title="Total Horse Dry Matter Intake (Mt/sqkm/yr)")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(legend.show = TRUE)
horse_map

livestock_density_DM<-tmap_arrange(cattle_map, buffalo_map, goat_map,sheep_map, horse_map, ncol=3)
tmap_save(livestock_density_DM,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//livestock_density_DM.png", dpi = 300)


################################################################################################################
# Figuring out the proportion of grass in the Dry Matter intake by livestock type

# From Heraro et al., 2013 Fig S1- India has majority MIA- Mixed Irrigated- Arid and SemiArid Tropics and Subtropics,
# MIH- Mixed Irrigated-  Humid and Subhumid Tropics systems of livestock distribution systems (shades of blue). Only a bit of NW i.e.
# part of Rajasthan and Punjab is yellow- LGA- Livestock only Arid and SemiArid Tropics and Subtropics. But I am going to assume
# that it is mostly MIA and MIH.

#From Herraro et al., 2013 Table S10-

##1 Dairy Cattle (BOVD) in SAS (South Asia which is mostly India) for the above two systems- Grass %  30 & 35.
##2 Beef Cattle and dairy (BOVO)- Grass % 33 & 42
##3 Small Ruminants dairy (SGTD)- Grass % 10 & 72 (very wide)
##4 Small Ruminants meat (SGTO)- Grass % 36 % 48 

## Cattle==Dairy Cattle
## Buffalo== cannot assign
## Small ruminants- sheep and goat= Small Ruminants Meat and Diary
## Horse== cannot assign (assume stover)
####Mean values

cattle_density_DM_Mt<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//cattle_density_DM.tif")
buffalo_density_DM_Mt<- raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//buffalo_density_DM.tif")
sheep_density_DM_Mt<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//sheep_density_DM.tif")
goat_density_DM_Mt<-raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//goat_density_DM.tif")
horse_density_DM_Mt<- raster("C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_Density_Processed//horse_density_DM.tif")

plot(cattle_density_DM_Mt)
plot(buffalo_density_DM_Mt)
plot(sheep_density_DM_Mt)
plot(goat_density_DM_Mt)
plot(horse_density_DM_Mt)

cattle_density_DMgrass_t<-cattle_density_DM_Mt*10^6 *mean(0.3,0.35)
writeRaster(cattle_density_DMgrass_t, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//cattle_density_DMgrass_t.tif")
buffalo_density_DM_t<-buffalo_density_DM_Mt*10^6
writeRaster(buffalo_density_DM_t, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//buffalo_density_DM_t.tif")
sheep_density_DMgrass_t<-sheep_density_DM_Mt*10^6 * mean(0.1,0.72,0.36,0.48)
writeRaster(sheep_density_DMgrass_t, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//sheep_density_DMgrass_t.tif")
goat_density_DMgrass_t<-goat_density_DM_Mt*10^6 * mean(0.1,0.72,0.36,0.48)
writeRaster(goat_density_DMgrass_t, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//goat_density_DMgrass_t.tif")
horse_density_DM_t<-horse_density_DM_Mt*10^6
writeRaster(horse_density_DM_t, "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//horse_density_DM_t.tif")

cattle_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(cattle_density_DMgrass_t) + tm_raster(style='quantile', n=6,palette = "YlOrRd", title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout( title="Total Cattle Grass Dry Matter Intake (t/sqkm/yr)",legend.show = TRUE)
cattle_map

buffalo_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(buffalo_density_DM_t) + tm_raster(style='quantile', n=6,palette = "Purples",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title="Total Buffalo Dry Matter Intake (t/sqkm/yr)",legend.title.size=1, legend.show = TRUE)
buffalo_map

goat_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(goat_density_DMgrass_t) + tm_raster(style='quantile', n=6,palette = "Reds",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title="Total Goat Grass Dry Matter Intake (t/sqkm/yr)",legend.title.size=1,legend.show = TRUE)
goat_map

sheep_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(sheep_density_DMgrass_t) + tm_raster(style='quantile', n=6,palette = "Greys",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title="Total Sheep Grass Dry Matter Intake (t/sqkm/yr)",legend.title.size=1, legend.show = TRUE)
sheep_map

horse_map<- tm_shape(asia, bbox = map_extent)+ tm_borders()+tm_shape(india_boundary_roy)+tm_borders()+tm_shape(india_shp)+ tm_fill()+
  tm_shape(horse_density_DM_t) + tm_raster(style='quantile', n=6,palette = "Blues",title="")+ tm_compass(type = "arrow", position = c("left", "top"))+tm_scale_bar(position=c("right","bottom"))+
  tm_layout(title="Total Horse Dry Matter Intake (t/sqkm/yr)",legend.title.size=1,legend.show = TRUE)
horse_map

livestock_density_DM<-tmap_arrange(cattle_map, buffalo_map, goat_map,sheep_map, horse_map, ncol=3)
tmap_save(livestock_density_DM,"C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Output//Domestic_Livestock_Processed//GriddedLivestock_DensityGrass_Processed//livestock_density_DMgrass.png", dpi = 300)
