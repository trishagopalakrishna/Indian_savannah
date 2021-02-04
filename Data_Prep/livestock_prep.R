### In this script I clean the web scraped livestock data from animal husbandry statistics website, for each district
### Started on- 03/02/2021
### Last edit made on- 
### Last edit made-

###############################################################################################

library(RColorBrewer)
library(lattice)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)



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
