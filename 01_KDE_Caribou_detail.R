# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



# script to prepare Wolf telemetry data and run MCP and kernal densitiy values
# written by genevieve perkins (genevieve.perkins@gov.bc.ca)

# overall process: 
#- Explore data to see what we have 
#- Define the area of interest per animal. availabiliy/ vs use (MCp or Kernal)
#- Align spatial layers and 


# help files: 
#https://www.ckwri.tamuk.edu/sites/default/files/publication/pdfs/2017/leonard_analyzing_wildlife_telemetry_data_in_r.pdf
#https://jamesepaterson.weebly.com/blog/spatialformatting    &   https://github.com/jamesepaterson/trackingworkshop
#https://cran.r-project.org/web/packages/adehabitatHS/adehabitatHS.pdf
#https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation

# instal packages and run libraries

## Saved here:
## I:\ES\General\Wildlife\Wildlife Species\Caribou\Tweedsmuir caribou\2015-2019\KHR_Analysis\KDE
## C:\Temp\TweedTelkwa\KDE

#install.packages("lubridate")
#install.packages("devtools")
#devtools::install_github("tidyverse/lubridate")

# run libraries
library(lattice)  
library(ggplot2)
library(scales)
library(dplyr)
library(xlsx)
library(readxl) 
library(GGally)
library(lubridate)
library(sf)
library(sp)
library(adehabitatHR)
library(ggmap)
library(scales)
library(here)
library(ggmap)
library(tidyr)

# set up a key 
register_google(key = "Insert Key Here",  # your Static Maps API key
                account_type = "standard")

# set up working directories
here()

## load data into R
# <- read.csv("5177_TEC GPS telemetry 2014-ongoing_April2_2019.csv",header = TRUE,stringsAsFactors = TRUE) 
twdata <- read.csv("5177_TEC GPS telemetry 2014-ongoing_April18_2019.csv",header = TRUE,stringsAsFactors = TRUE) 

# select the columns of interst 
twdata<- twdata %>% dplyr::select(Animal.ID, Date.Time,Latitude..ø., Longitude..ø., Height..m.,DOP,FixType,LMT_Time,Month,Year)
twdata <- twdata %>%  mutate(Latitude = Latitude..ø., Longitude =Longitude..ø.,Month.0 = Month, Year.0 = Year )
twdata <- twdata[ , -which(names(twdata) %in% c("Year","Month",'Latitude..ø.', 'Longitude..ø.'))]

#head(twdata)

##############################################################
#### PART 1: DATA EXPLORATION #####
twdata$Date2=as.Date(twdata$Date.Time, format = '%Y-%m-%d') 
twdata$Year<- year(twdata$Date2)
twdata$Month <- month(twdata$Date2) # break out DMY columns
twdata$Day <- day(twdata$Date2)
twdata$Date.j <- julian(twdata$Date2)#Add julian day
twdata$Hours <- as.numeric(format(as.POSIXct(strptime(twdata$Date.Time,"%Y-%m-%d %H:%M",tz="")) ,format = "%H"))

# remove data with missing ID no 
#length(twdata$Animal.ID)
twdata <- twdata[twdata$Animal.ID != "",]
#length(twdata$Animal.ID)

#if the month and year is missing then fill in wih original dataset values for Month and year 
nomonthdata <- twdata %>% filter(is.na(Month)) %>% mutate(Month = Month.0,Year = Year.0); length(nomonthdata$Animal.ID)
twdata <- twdata %>% drop_na(Month); length(twdata$Month)
twdata <- rbind(twdata, nomonthdata)

## PROBLEM 2: STill need to fix calculate the time gap between sorted date time per animal 
       ids <- as.character(unique(twdata$Animal.ID))
        
        # calculate the difference between dates; STILL TO COMPLETE
        #for(i in 1:length(ids)){
        #  x <- twdata %>% 
        #    group_by(Animal.ID) %>% 
        #    arrange(Date.Time) %>% 
        #    mutate(date.dif = difftime(twdata$DateTime - lag(twdata$DateTime),units = "hours"))  
        #    mutate(date.dif = difftime(as.POSIXct(twdata$Date.Time, format = "%Y-%m-%d %H:%M"), lag(as.POSIXct(twdata$Date.Time), format = "%Y-%m-%d %H:%M"),units = "hours"))
        #    mutate(date.dif = difftime(as.POSIXct(Date.Time, format = "%Y-%m-%d %H:%M"), lag(as.POSIXct(Date.Time), format = "%Y-%m-%d %H:%M"),units = "hours"))
          
        #x =  as.data.frame(x)
 

# Check the multiple counts of anaimals per day 
counts.per.day <- twdata %>% group_by(Animal.ID,Date.j) %>% 
  summarise(total = n(),unique = unique(Date.j)) %>% 
  group_by(Animal.ID,total) %>% 
  summarise(total.j = n()) 

pp = ggplot(data = counts.per.day, aes(total,total.j,col = Animal.ID)) + geom_point() + ggtitle('Number of fixes per julien day per animal')


      # Follow up on the 12 records (Jan 26th 2015)
      counts.per.day <- twdata %>% group_by(Animal.ID,Date.j) %>% 
        summarise(total = n(),unique = unique(Date.j)) %>% 
        filter(Animal.ID == "TWC1015")
      #bad.dates = twdata %>% dplyr::filter(Animal.ID == "TWC1015") 
      #bad.dates =bad.dates %>% dplyr::filter(Date.j == 16461)


# group caribou into seasons acccordeing to Debs grouping 
twdata <- twdata %>% mutate(season = ifelse(Month %in% c(4,5),"SM",
                                     ifelse(Month == 6,"C",
                                     ifelse(Month %in% c(7,8,9),"S",
                                     ifelse(Month == 10,"R", 
                                     ifelse(Month == 11,"FM",
                                     ifelse(Month %in% c(12,1,2,3),"W", NA)))))))

## Add a caribou year (december = following year)
twdata <- twdata %>% mutate(year.C = ifelse(Month %in% c(1:11),Year,
                                     ifelse(Month == 12,Year + 1, NA)))
                                     
          # Q : HOw many fixes do we have per animal ??
          p1 <- ggplot(twdata) + geom_bar(aes(Animal.ID)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)); p1
          id.date <- twdata %>% group_by(Animal.ID,year.C,Month) %>% summarise(count = n()) ; id.date
          id.season.yr <- twdata %>% group_by(Animal.ID,year.C,season) %>% summarise(count = n()) ; id.season.yr
          id.season <- twdata %>% group_by(Animal.ID,season) %>% summarise(count = n()) ; id.season
          id.jdate <- twdata %>% group_by(Animal.ID,year.C) %>% summarise(count = length(unique(Date.j))) ; id.jdate 
          
          # Q:  what about temporal variability - number of years? time of year? 
          p2 <- ggplot(id.date,aes(x = year.C,count)) + geom_bar(stat ="identity") + facet_wrap(~Animal.ID) + ggtitle("Total Telemetry fixes per animal per caribou year (all months)"); p2
          #ggsave(here("outputs","Count.per.cyear.jpeg"))
          
          #p3 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity") + facet_wrap(~Animal.ID)+ ggtitle("Total Telemetry fixes per animal per month (all years)"); p3
          #ggsave(here("Count.per.month.jpeg"))
          
          p3.1 <- ggplot(id.season,aes(x = season,count)) + geom_bar(stat ="identity") +  geom_hline(yintercept=50, linetype="dashed", color = "red") +facet_wrap(~Animal.ID) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Total Telemetry fixes per animal per season "); p3.1
          ggsave(here("outputs","Count.per.season.jpeg"))
          
          p3.2 <- ggplot(id.jdate,aes(x = year.C,count)) + geom_bar(stat ="identity") +  geom_hline(yintercept=50, linetype="dashed", color = "red") +facet_wrap(~Animal.ID) + ggtitle("Total Telemetry days per animal per caribou year (all months)"); p3.2
          ggsave(here("outputs","telem.days.per.cyear.jpeg"))
          
          ## questions?  - what do we do with the low count individuals? Do we have enough for seasonal blocks? 
          # Q how much data do we have when we combine all years and all animals? 
          #p4 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity"); p4
          #p4 <- ggplot(id.season,aes(x = season,count)) + geom_bar(stat ="identity"); p4
          #p4 <- ggplot(id.jdate,aes(x = year.C,count)) + geom_bar(stat ="identity"); p4

# check the spread of Caribou with maped locations 
basemap <- get_map(location = c(-126.5654,53.36959), zoom =8)

        #p5 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude))+facet_wrap(~Animal.ID); p5
        #ggsave(here("outputs","Map.per.animal.google.jpeg"))
        
        #p5.1 = ggplot() + geom_point(data = twdata, aes(x = Longitude, y = Latitude))+facet_wrap(~Animal.ID); p5.1
        #ggsave(here("outputs","Map.per.animal.jpeg"))
        
        p6 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude,y = Latitude,colour = Animal.ID)) ; p6
        #ggsave(here("outputs","Map.all.animals.google.jpeg"))
        
        p7 =  ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,colour = season))  ; p7
        #ggsave(here("outputs","Map.per.season.google.jpeg"))
        
        p7.1 = ggplot(twdata, aes(Longitude,Latitude,colour = season)) + geom_point() ; p7.1
        #ggsave(here("outputs","Map.per.season.jpeg"))
        
        p8 =  ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,colour = season)) + facet_wrap(~year.C)   ; p8
        #ggsave(here("outputs","Map.per.year.season.google.jpeg"))
        
        p8.1 = ggplot(twdata, aes(Longitude,Latitude,colour = season)) + facet_wrap(~year.C) + geom_point() ; p8.1
        ggsave(here("outputs","Map.per.year.season.jpeg"))

####################################################################
###################################################################
## How are animals using the area? 
## 
## Depends on scale of interest : 
##  - Hierachial order of selection 
##  - Entire study area? / Population level? // individaual levels 

######################################################################

## Calculate Kernal density estiamtes ################

aoi <- unique(twdata$Animal.ID)
yrs.to.map <- unique(twdata$year.C)  
seasons.to.map <-unique(twdata$season) 
  
###################################
## Testing with Winter 
####################################

#for(i in 1:length(season.to.map)){ 
  i = 1 # winter season 
  seas <-seasons.to.map[i]
  tdf.0 <- twdata %>% filter(season == seas)
  
  ## drop animals with less than 5 records 
  tdf.0$Animal.ID <- droplevels(tdf.0$Animal.ID)
  tdf <- tdf.0[, c("Animal.ID", "Longitude", "Latitude")]  #; length(tdf$Animal.ID)
  
  # filter for individuals with more than 50 locations 
  tdf.u <- unique(tdf)
  
  #tdf.u.df <- tdf.u %>% group_by(Animal.ID) %>%  summarise (count = n ())
  tdf <- tdf.u [tdf.u$Animal.ID %in% names(table(tdf.u$Animal.ID)) [table(tdf.u$Animal.ID) >= 50], ]
  tdf$Animal.ID <- droplevels(tdf$Animal.ID)
  
  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdf) <- c("Longitude", "Latitude")
  proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  tdfgeo <- spTransform(tdf, CRS("+init=epsg:3005")) # Transform to UTM 
  
  ##################################################################################################
  # Test a number of KDE versions: href, h = 1000,2000,4000, LSCV
  
  ##################################################################################################
  # KDE 1) run kernal density for individual animals 
  kde1  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95 <- getverticeshr(kde1,95) ;   ver95.sf<- st_as_sf(ver95)
  ver75 <- getverticeshr(kde1,75) ;   ver75.sf<- st_as_sf(ver75)
  ver50 <- getverticeshr(kde1,50) ;   ver50.sf<- st_as_sf(ver50)
  st_write(ver95.sf,here("outputs",paste("KDE95",seas,"Hrefid.shp",sep = ""))) # write out shapefile
    
  #plot(st_geometry(ver95.sf),col = "red")
  #plot(st_geometry(ver75.sf),col = "orange", add = TRUE)
  #plot(st_geometry(ver50.sf),col = "yellow", add = TRUE)
  #plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ##############################################################################################
  # KDE 2) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde2  <- kernelUD(tdfgeo, h = 1000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.2 <- getverticeshr(kde2,95) ;   ver95.2.sf<- st_as_sf(ver95.2)
  #ver75.2 <- getverticeshr(kde2,75) ;   ver75.2.sf<- st_as_sf(ver75.2)
  #ver50.2 <- getverticeshr(kde2,50) ;   ver50.2.sf<- st_as_sf(ver50.2)
  st_write(ver95.2.sf,here("outputs",paste("KDE95",seas,"1000id.shp",sep = "")))
  ver95.2.sf <- st_read(here("outputs",paste("KDE95",seas,"1000id.shp",sep = "")))
  
  
  #plot(st_geometry(ver95.2.sf),col = "green", add = TRUE)
  #plot(st_geometry(ver75.2.sf),col = "blue", add = TRUE)
  #plot(st_geometry(ver50.2.sf),col = "purple", add = TRUE)
  #plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ##################################################################################################
  # KDE 3) run kernal density for inidividual animasl with LSCV outputs 
  kde3  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.3 <- getverticeshr(kde3,95) ;   ver95.3.sf<- st_as_sf(ver95.3)
  ver75.3 <- getverticeshr(kde3,75) ;   ver75.3.sf<- st_as_sf(ver75.3)
  ver50.3 <- getverticeshr(kde3,50) ;   ver50.3.sf<- st_as_sf(ver50.3)
  st_write(ver95.4.sf,here("outputs","KDER95WLSCVid.shp"))
  
  str(kde3[2]) # check the h values 
  plot(st_geometry(ver95.3.sf),col = "hot pink", add = T)
  plot(st_geometry(ver75.2.sf),col = "green", add = TRUE)
  plot(st_geometry(ver50.2.sf),col = "forest green", add = TRUE)
  
  ##################################################################################################
  # KDE 4) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde4  <- kernelUD(tdfgeo, h = 4000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.4 <- getverticeshr(kde4,95) ;   ver95.4.sf<- st_as_sf(ver95.4)
  #ver75.4 <- getverticeshr(kde4,75) ;   ver75.4.sf<- st_as_sf(ver75.4)
  #ver50.4 <- getverticeshr(kde4,50) ;   ver50.4.sf<- st_as_sf(ver50.4)
  st_write(ver95.4.sf,here("outputs","KDER95W4000id.shp"))
  
  plot(st_geometry(ver95.4.sf),col = "blue", add = TRUE)
  plot(st_geometry(ver75.4.sf),col = "blue", add = TRUE)
  plot(st_geometry(ver50.4.sf),col = "purple", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ################################################################################################## 
  # KDE 5) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde5  <- kernelUD(tdfgeo, h = 2000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.5 <- getverticeshr(kde5,95) ;   ver95.5.sf<- st_as_sf(ver95.5)
  ver75.5 <- getverticeshr(kde5,75) ;   ver75.5.sf<- st_as_sf(ver75.5)
  ver50.5 <- getverticeshr(kde5,50) ;   ver50.5.sf<- st_as_sf(ver50.5)
  st_write(ver95.5.sf,here("outputs","KDER95W2000id.shp"))
  
  plot(st_geometry(ver95.5.sf),col = "forest green", add = TRUE)
  plot(st_geometry(ver75.4.sf),col = "blue", add = TRUE)
  plot(st_geometry(ver50.4.sf),col = "purple", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ####################################################################################################
  ###################################################################################################
  ## PART 2: 
  
  ## Run KDE for all animals combined 
  ##################################################################################################
  
  ## drop animals with less than 5 records 
  tdf <- tdf.0[, c("Longitude", "Latitude")]  #; length(tdf$Animal.ID)
  
  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdf) <- c("Longitude", "Latitude")
  proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  tdfgeo <- spTransform(tdf, CRS("+init=epsg:3005")) # Transform to UTM 
  
  ##################################################################################################
  # KDE 1) run kernal density for individual animals 
  kde1  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95 <- getverticeshr(kde1,95) ;   ver95.sf<- st_as_sf(ver95)
  ver75 <- getverticeshr(kde1,75) ;   ver75.sf<- st_as_sf(ver75)
  ver50 <- getverticeshr(kde1,50) ;   ver50.sf<- st_as_sf(ver50)
  
  st_write(ver95.sf,here("outputs",paste("KDE95",seas,"Hrefall.shp",sep = ""))) # write out shapefile
  
  plot(st_geometry(ver95.sf),col = "red")
  plot(st_geometry(ver75.sf),col = "orange", add = TRUE)
  plot(st_geometry(ver50.sf),col = "yellow", add = TRUE)
  #plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  #plot(st_geometry(ver95.sf),col = "red")
  #plot(st_geometry(ver75.sf),col = "orange", add = TRUE)
  #plot(st_geometry(ver50.sf),col = "yellow", add = TRUE)
  #plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ##############################################################################################
  # KDE 2) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde2  <- kernelUD(tdfgeo, h = 1000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.2 <- getverticeshr(kde2,95) ;   ver95.2.sf<- st_as_sf(ver95.2)
  st_write(ver95.2.sf,here("outputs",paste("KDE95",seas,"1000all.shp",sep = "")))
  
  plot(st_geometry(ver95.2.sf),col = "green", add = TRUE)
  plot(st_geometry(ver75.2.sf),col = "blue", add = TRUE)
  plot(st_geometry(ver50.2.sf),col = "purple", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ##################################################################################################
  # KDE 3) run kernal density for inidividual animasl with LSCV outputs 
  kde3  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.3 <- getverticeshr(kde3,95) ;   ver95.3.sf<- st_as_sf(ver95.3)
  st_write(ver95.3.sf,here("outputs","KDER95WLSCVall.shp"))
  plot(st_geometry(ver95.3.sf),col = "hot pink", add = T)
 
  ##################################################################################################
  # KDE 4) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde4  <- kernelUD(tdfgeo, h = 4000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.4 <- getverticeshr(kde4,95) ;   ver95.4.sf<- st_as_sf(ver95.4)
  st_write(ver95.4.sf,here("outputs","KDER95W4000all.shp"))
  
  plot(st_geometry(ver95.4.sf),col = "blue", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  ################################################################################################## 
  # KDE 5) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde5  <- kernelUD(tdfgeo, h = 2000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.5 <- getverticeshr(kde5,95) ;   ver95.5.sf<- st_as_sf(ver95.5)
  st_write(ver95.5.sf,here("outputs","KDER95W2000all.shp"))
  
  plot(st_geometry(ver95.5.sf),col = "forest green", add = TRUE)
  plot(st_geometry(ver75.4.sf),col = "blue", add = TRUE)
  plot(st_geometry(ver50.4.sf),col = "purple", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  
  
  
  ####################################################################################################
  ###################################################################################################
  ## PART 3: 
  
  ## Run KDE for years all animals 
  ##################################################################################################
  
  
  ## drop animals with less than 5 records 
  tdf.0$Animal.ID <- droplevels(tdf.0$Animal.ID)
  tdf <- tdf.0[, c("year.C","Longitude", "Latitude")]  #; length(tdf$Animal.ID)
  
  # filter for individuals with more than 50 locations 
  tdf.u <- unique(tdf)
  years.c.sum <- tdf %>% group_by(year.C) %>% summarise(count = n())

  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdf) <- c("Longitude", "Latitude")
  proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  tdfgeo <- spTransform(tdf, CRS("+init=epsg:3005")) # Transform to UTM 
  
  # KDE 5) run kernal density for inidividual animasl with user selected h ref (after testing)
  kde5  <- kernelUD(tdfgeo, h = 2000, kern = c("bivnorm"), grid = 500,extent = 2)
  ver95.5 <- getverticeshr(kde5,95) ;   ver95.5.sf<- st_as_sf(ver95.5)
  st_write(ver95.5.sf,here("outputs","KDE95W2000pyr.shp"))
  #plot(st_geometry(ver95.5.sf),col = "forest green", add = TRUE)
  #plot(st_geometry(ver75.4.sf),col = "blue", add = TRUE)
  #plot(st_geometry(ver50.4.sf),col = "purple", add = TRUE)
  plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  
  ####################################################################################################
  ###################################################################################################
  ## PART 4: 
  
  ## Run KDE all animals per year. 
  ##################################################################################################
  
  ## drop animals with less than 5 records 
  #tdf.0$Animal.ID <- droplevels(tdf.0$Animal.ID)
  tdf <- tdf.0[, c("Animal.ID","year.C","Longitude", "Latitude")]  #; length(tdf$Animal.ID)
  
  # filter for individuals with more than 50 locations 
  tdf.u <- unique(tdf)
  
  # get a list of animal id and year when unique locations is >50 
  years.c.id.sum <- tdf.u %>% group_by(year.C,Animal.ID) %>% summarise(count = n())
  
  # filter the dataset 
  tdf.u <- left_join(tdf.u, years.c.id.sum , by = c('year.C','Animal.ID'))
  tdf.u <- tdf.u[tdf.u$count > 50,]   #;  unique(tdf.u$count)
  
  yrs.to.map <- unique(tdf.u$year.C )  
  
## loop this through the years of interest 
  for(ii in 4:length(yrs.to.map )){ 
    #ii = 2 # 2015 or first year 
    yrs <-yrs.to.map [ii]
    tdf.yr <-  tdf.u  %>% filter(year.C == yrs)
    sum.yr <- tdf.yr %>% group_by(year.C,Animal.ID) %>% summarise(count = n())
    tdf <- tdf.yr %>% dplyr::select('Animal.ID','Longitude', 'Latitude')
    tdf$Animal.ID <- droplevels(tdf$Animal.ID)
    
    #sum.yr <- tdf.yr %>% group_by(year.C,Animal.ID) %>% summarise(count = n())
    
    # Create a SpatialPointsDataFrame by defining the coordinates
    coordinates(tdf) <- c("Longitude", "Latitude")
    proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
    tdfgeo <- spTransform(tdf, CRS("+init=epsg:3005")) # Transform to UTM 
    
    # KDE 5) run kernal density for inidividual animasl with user selected h ref (after testing)
    kde5  <- kernelUD(tdfgeo, h = 2000, kern = c("bivnorm"), grid = 500,extent = 2)
    ver95.5 <- getverticeshr(kde5,95) ;   ver95.5.sf<- st_as_sf(ver95.5)
    st_write(ver95.5.sf,here("outputs", paste("KDE95",seas,"2000ID_",yrs,".shp",sep = "")))
    
    plot(st_geometry(ver95.5.sf),col = "forest green", add = TRUE)
    #plot(st_geometry(ver75.4.sf),col = "blue", add = TRUE)
    #plot(st_geometry(ver50.4.sf),col = "purple", add = TRUE)
    plot(tdfgeo, pch = 1, size = 0.5, add = TRUE)     # Add points 
  
  } 
  
  
  
##############################################################################################################
## See mxd : 
 ## I:\ES\General\Wildlife\Wildlife Species\Caribou\Tweedsmuir caribou\2015-2019\KHR_Analysis\KDE
  
  
  
  
  
  
  
  ver95.2.sf <-st_read(paste("outputs", "KDE95W2000ID_2018.shp",sep = "/"))
  
  
  
  
  
  
  list.files(paste("outputs"))
  
  
  
  
  
  
  
  
  
  
 ################################################################################################################ 
  # Testing difference levels of smoothing 
###############################################################################################################

    # Ran these and compared with Tyler Muhlys KDE calculated for RSF analysis. Same conclusion that LSCV is too fine 
    # Href is too over estimate 
  
  #################### 
  #Testing extents 1,2,5, 10 
  ####################
  
  # run kernal density for all animals # test wider extent
  kde1  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95 <- getverticeshr(kde1,95) ;   ver95.sf<- st_as_sf(ver95)
  ver75 <- getverticeshr(kde1,75) ;   ver75.sf<- st_as_sf(ver75)
  ver50 <- getverticeshr(kde1,50) ;   ver50.sf<- st_as_sf(ver50)
  
  plot(st_geometry(ver95.sf),col = "red",add = T)
  plot(st_geometry(ver75.sf),col = "orange", add = TRUE)
  plot(st_geometry(ver50.sf),col = "yellow", add = TRUE)
  
  st_write(ver95.sf,here("outputs","KernalhR95_W.shp"))
  
  # run kernal density for all animals # reduce extent to 1. (Slight buffer and more inclusive of large areas but drop small isolated patches )
  # this appears to have very little  difference in extent changes (tested 1,2,5,10 )
  
  kde2  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000,extent = 5)
  ver95.2 <- getverticeshr(kde2,95) ;   ver95.2.sf<- st_as_sf(ver95.2)
  ver75.2 <- getverticeshr(kde2,75) ;   ver75.2.sf<- st_as_sf(ver75.2)
  ver50.2 <- getverticeshr(kde2,50) ;   ver50.2.sf<- st_as_sf(ver50.2)
      plot(st_geometry(ver95.2.sf),col = "blue",add = T)
      plot(st_geometry(ver75.2.sf),col = "green", add = TRUE)
      plot(st_geometry(ver50.2.sf),col = "forest green", add = TRUE)
      
  # note when testing 10 this appears to drop some isolated patches and     
  kde3  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 1000,extent = 10)
  ver95.3 <- getverticeshr(kde3,95) ;   ver95.3.sf<- st_as_sf(ver95.3)
  ver75.3 <- getverticeshr(kde3,75) ;   ver75.3.sf<- st_as_sf(ver75.3)
  ver50.3 <- getverticeshr(kde3,50) ;   ver50.3.sf<- st_as_sf(ver50.3)
  str(kde1) # check the h values 
      plot(st_geometry(ver95.3.sf),col = "hot pink", add = TRUE)
      plot(st_geometry(ver75.2.sf),col = "green", add = TRUE)
      plot(st_geometry(ver50.2.sf),col = "forest green", add = TRUE)
  
  
  ######################    
  #  Testing h bandwidth  h = 500, 750, 1000 and LSCV 
  ######################  
      
  # run kernal density for all animals # test wider extent
  kde4  <- kernelUD(tdfgeo, h = 500, kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95.4 <- getverticeshr(kde4,95) ;   ver95.4.sf<- st_as_sf(ver95.4)
  #ver75.4 <- getverticeshr(kde4,75) ;   ver75.4.sf<- st_as_sf(ver75.4)
  #ver50.4 <- getverticeshr(kde4,50) ;   ver50.4.sf<- st_as_sf(ver50.4)
      
  plot(st_geometry(ver95.4.sf),col = "grey", add = TRUE)
  #plot(st_geometry(ver75.4.sf),col = "light blue", add = TRUE)
  #plot(st_geometry(ver50.4.sf),col = "forest green", add = TRUE)
  
  st_write(ver95.4.sf,here("outputs","KernalhR95_500W.shp"))
  
  ### Testing with a 750 band width 
  # run kernal density for all animals # test wider extent
  kde5  <- kernelUD(tdfgeo, h = 750, kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95.5 <- getverticeshr(kde5,95) ;   ver95.5.sf<- st_as_sf(ver95.5)
  #ver75.5 <- getverticeshr(kde5,75) ;   ver75.5.sf<- st_as_sf(ver75.5)
  #ver50.5 <- getverticeshr(kde5,50) ;   ver50.5.sf<- st_as_sf(ver50.5)
  
  plot(st_geometry(ver95.5.sf),col = "blue", add = TRUE)
  #plot(st_geometry(ver75.5.sf),col = "yellow", add = TRUE)
  #plot(st_geometry(ver50.5.sf),col = "orange", add = TRUE)
  
  st_write(ver95.5.sf,here("outputs","KernalhR95_750W.shp"))
  
  ### Testing with a 1000 band width 
  # run kernal density for all animals # test wider extent
  kde6  <- kernelUD(tdfgeo, h = 1000, kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95.6 <- getverticeshr(kde6,95) ;   ver95.6.sf<- st_as_sf(ver95.6)
  #ver75.6 <- getverticeshr(kde6,75) ;   ver75.6.sf<- st_as_sf(ver75.6)
  #ver50.6 <- getverticeshr(kde6,50) ;   ver50.6.sf<- st_as_sf(ver50.6)
  
  plot(st_geometry(ver95.6.sf),col = "green", add = TRUE)
  #plot(st_geometry(ver75.6.sf),col = "blue", add = TRUE)
  #plot(st_geometry(ver50.6.sf),col = "purple", add = TRUE)
 
  st_write(ver95.6.sf,here("outputs","KernalhR95_1000W.shp"))
  
  
  ##################################################################################################
  
  # run kernal density with LSCV techniques 
  kde7  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 1000,extent = 2)
  ver95.7 <- getverticeshr(kde7,95) ;   ver95.7.sf<- st_as_sf(ver95.7)
  plot(st_geometry(ver95.7.sf),col = "grey", add = TRUE)      # plot figure for comparison s
  st_write(ver95.7.sf,here("outputs","KernalhR95_LSCVW.shp")) # write out 
  
  
  #############################################################
  # Run winter kde for each animal and check against tylers outputs 
  

  for ( ii in 2:length(aoi)){ 
    ii = 2
    tdf.aoi <-tdf %>% dplyr::filter(Animal.ID == paste(aoi[ii]))
    tdf.aoi$Animal.ID <- droplevels( tdf.aoi$Animal.ID)
    
    
    tdf.aoi <- tdf
    # get the bandwidth varibles 
    library(ks)
    tdf.bw <- tdf[,c(2,3)]
    Hpi.diag(tdf.bw)

    Hpi(tdf.bw)
      
    )
    tdf.bw <- coordinates(tdfgeo)
    
    # Create a SpatialPointsDataFrame by defining the coordinates
    coordinates(tdf.aoi) <- c("Longitude", "Latitude")
    proj4string(tdf.aoi) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
    tdfgeo <- spTransform(tdf.aoi, CRS("+proj=longlat")) # Transform the point and MCP objects. 
    
   
        # run the kernal density 
        #kud_points  <- kernelUD(tdfgeo, h = "href", same4all = TRUE,grid = 500, kern = c("bivnorm"))
        kud_points  <- kernelUD(tdfgeo, h = "LSCV", same4all = FALSE, grid = 500, kern = c("bivnorm")) ;  plotLSCV(kud_points)
        kud_points.1  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"))
        kud_points.4  <- kernelUD(tdfgeo, h = "LSCV", same4all = FALSE, grid = 1000, kern = c("bivnorm"))
        kud_points.5  <- kernelUD(tdfgeo, h = "LSCV", same4all = FALSE, grid = 250, kern = c("bivnorm"))
        kud_points.6  <- kernelUD(tdfgeo, h = "LSCV", same4all = TRUE, kern = c("bivnorm"))
        
        #kud_points.7  <- kernelUD(tdfgeo, h = "LSCV", same4all = FALSE, grid = 10000, kern = c("bivnorm"))
        
        plotLSCV(kud_points)
        #plotLSCV(kud_points.7)
        
        # H ref versions pretty broad 
        kud_points.2  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"))
        kud_points.3  <- kernelUD(tdfgeo, h = "href", same4all = TRUE,grid = 500, kern = c("bivnorm"))
        
        # using the least-square cross validation technique with a bivariate normal kernal 
        # other option is a "href" with a "epa" Epanechnikov kernal 
      
        # https://ecosystems.psu.edu/research/labs/walter-lab/manual/home-range-estimation/4.2-kde-with-least-squares-cross-validation-bandwidth-selection-hlscv
        # http://www.spatialecology.com/gme/kde.htm
        
        image(kud_points)# view the outputs (note if the same4all = TRUE it will be the full extent not zoomed per individual)
        image(kud_points.2)
        
        # get vertices
        ver95 <- getverticeshr(kud_points,95) # need to enlarge the extent to get to a 95 kernal density 
        ver75 <- getverticeshr(kud_points,75)
        ver50 <- getverticeshr(kud_points,50)
        ver50.sf <- st_as_sf(ver50)
        ver75.sf <- st_as_sf(ver75)
        
        
        ver.95 <- st_as_sf(ver95)
        ver95.1 <- getverticeshr(kud_points.1,95)
        ver95.sf.1 <- st_as_sf(ver95.1)
        ver95.2 <- getverticeshr(kud_points.2,95)
        ver95.sf.2 <- st_as_sf(ver95.2)
        ver95.3 <- getverticeshr(kud_points.3,95)
        ver95.sf.3 <- st_as_sf(ver95.3)
        ver95.4 <- getverticeshr(kud_points.4,95)
        ver95.sf.4 <- st_as_sf(ver95.4)
        ver95.5 <- getverticeshr(kud_points.5,95)
        ver95.sf.5 <- st_as_sf(ver95.5)
        ver95.6 <- getverticeshr(kud_points.6,95)
        ver95.sf.6 <- st_as_sf(ver95.6)
        #ver95.7 <- getverticeshr(kud_points.7,95)
        #ver95.sf.7 <- st_as_sf(ver95.7)
        
        plot()
        
        plot(st_geometry(ver95.sf),col = "red") # LDSV , grid 500 kernelUD(tdfgeo, h = "LSCV", same4all = FALSE, grid = 500, kern = c("bivnorm")) ;  plotLSCV(kud_points)
        plot(st_geometry(ver95.sf.1),add = T) #kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"))
        plot(st_geometry(ver95.sf.2),col = "green", add = T) #2  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm","epa"))
        plot(st_geometry(ver95.sf.3),add = T,col = " dark green")#kernelUD(tdfgeo, h = "href", same4all = TRUE,grid = 500, kern = c("bivnorm"))
        
        plot(st_geometry(ver95.sf),add = T,col = "red")
        plot(st_geometry(ver95.sf.4),add = T,col = "purple") # slightlty wider than orange 
        plot(st_geometry(ver95.sf.5),add = T,col = "orange")
        plot(st_geometry(ver95.sf.6),add = T,col = "yellow")
        plot(st_geometry(ver95.sf.7),add = T,col = "blue")
        
        plot(st_geometry(ver95.sf),col = "red") 
        plot(st_geometry(ver75.sf),col = "blue", add = T) 
        plot(st_geometry(ver50.sf),col = "yellow",add = T) 
        
        
        # write out shapefile
        st_write(ver95.sf,here("outputs",paste("KDE_95_",aoi[ii],"_",seas,".shp",sep ="")))
        st_write(ver75.sf,here("outputs",paste("KDE_75_",aoi[ii],"_",seas,".shp",sep ="")))
        st_write(ver50.sf,here("outputs",paste("KDE_50_",aoi[ii],"_",seas,".shp",sep ="")))
          
        tdf.sf  <- tdf.0 %>% dplyr::select(Animal.ID,Latitude,Longitude)
        tdf.sf <- tdf.sf %>% dplyr::filter(Animal.ID == paste(aoi[ii]))
        tdf.sf  <-  st_as_sf(tdf.sf, coords = c("Longitude","Latitude"),crs = 4326)                   
        tdf.sf <- tdf.sf %>% mutate(id = Animal.ID)
  
          # add a plot per animal 
          
           p1 = ggplot() + 
             geom_sf(data = ver95.sf) +
             geom_sf(data = tdf.sf) + 
             ggtitle(paste(paste(aoi[ii])," 95% KDE for ",seas," (All years)",sep = ""));p1
             
           #write out the plot 
           ggsave(here("outputs",paste("Map.KDE.",seas,".jpeg",sep = "")))
          
        } # end of animal Loop 
   

  # # test KDE Codes with all animals 
  # tdf <- twdata[, c("Longitude", "Latitude")] 
  # 
  # # Create a SpatialPointsDataFrame by defining the coordinates
  # coordinates(tdf) <- c("Longitude", "Latitude")
  # proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  # tdfgeo <- spTransform(tdf, CRS("+proj=longlat")) # Transform the point and MCP objects. 
  # 
  # 
  # crs(tdf.s,3005)  
  # 
  # 
  # # test the href bandwidth 
  # kde1  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"))
  # ver95 <- getverticeshr(kde1,95) ;   ver95.sf<- st_as_sf(ver95)
  # plot(st_geometry(ver95.sf),col = "red")
  # 
  # # experiment with range in grid size 
  # kde2  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 250) # changing the grid size has minimal change the output extent but a large change on processing time. 
  # ver95.2 <- getverticeshr(kde2,95) ;   ver95.2.sf<- st_as_sf(ver95.2)
  # ver75.2 <- getverticeshr(kde2,75) ;   ver75.2.sf<- st_as_sf(ver75.2)
  # ver50.2 <- getverticeshr(kde2,50) ;   ver50.2.sf<- st_as_sf(ver50.2)
  # plot(st_geometry(ver95.2.sf),col = "orange", add = TRUE)
  # plot(st_geometry(ver75.2.sf),col = "blue", add = TRUE)
  # plot(st_geometry(ver50.2.sf),col = "pink", add = TRUE)
  # plot(tdfgeo, pch = 1, add = TRUE)
  # 
  # #Test the bandwidth estimator function() # cant get this working properly 
  # library(ks)
  # tdf.bw <- tdf
  # Hpi.diag(tdf)
  # Hpi(tdf.bw)
  # 
  # kde3  <- kernelUD(tdfgeo, h = 0.1, kern = c("bivnorm")) # tried with manual manipulation of h value # grid too small error
  # ver95.3<- getverticeshr(kde3,95) ;   ver95.3.sf<- st_as_sf(ver95.3)
  # ver75.3<- getverticeshr(kde3,75) ;   ver75.3.sf<- st_as_sf(ver75.3)
  # ver50.3<- getverticeshr(kde3,50) ;   ver50.3.sf<- st_as_sf(ver50.3)
  # plot(st_geometry(ver95.3.sf),col = "orange")
  # plot(st_geometry(ver75.3.sf),col = "blue", add = TRUE)
  # plot(st_geometry(ver50.3.sf),col = "pink", add = TRUE)
  # plot(tdfgeo, pch = 1, add = TRUE)
  # 
  # ## test the calving and compare to previous KDE from Brain hernden 
  # #kde4  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm")) # failed to converg 
  # #kde4  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"),grid = 250, hlim = c(5,10)) # failed to converg 
  # 
  # kde4  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"),grid = 250, hlim = c(10,100)) # failed to converg 
  # 
  # # these models fail to converg. 
  # # try another option to add a 25 m jitter and re-run with LSCV method. 
  # 
  # twdata$jitterY <- jitter(twdata$Latitude, factor=25)
  # twdata$jitterX <- jitter(twdata$Longitude, factor=50)
  # locjitter <- data.frame("x"=twdata$jitterX,"y"=twdata$jitterY)
  # tdf <- locjitter
  # 
  # coordinates(tdf) <- c("x", "y")
  # proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  # tdfgeo <- spTransform(tdf, CRS("+proj=longlat")) # Transform the point and MCP objects. 
  # udbis3 <- kernelUD(tdfgeo, h = "LSCV")#, hlim = c(1, 5),extent=1)
  
  # 
  # proj4string <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  # 
  # jitterspdf <- SpatialPointsDataFrame(locjitter,twdata, proj4string = proj4string)
  # plot(jitterspdf, col=twdata$Animal.ID)
  # points(pantherspdf, col="blue")
  # udbis3 <- kernelUD(tdfgeo, h = "LSCV")#, hlim = c(1, 5),extent=1)
  # image(udbis3)
  # 
  # # tried with manual manipulation of h value # grid too small error
  # ver95.4<- getverticeshr(kde4,95) ;   ver95.4.sf<- st_as_sf(ver95.4)
  # ver75.4<- getverticeshr(kde4,75) ;   ver75.4.sf<- st_as_sf(ver75.4)
  # ver50.4<- getverticeshr(kde4,50) ;   ver50.4.sf<- st_as_sf(ver50.4)
  # 
  
   
   
   
   
   
   
   















### OR 


# Individual levels  # using a subset of the data 
# Create individual buffers around each of the individual animals 

## load data into R
twd =as.data.frame(read.csv(paste(data.folder,"TweedsmuirWolvesUnique.csv",sep = ""), header = T,stringsAsFactors = T))
twd$Date2=dmy(twd$Date) 

w.ltraj <- as.ltraj(xy = twd[,c("Longitude", "Latitude")], 
                    date =  as.POSIXct(paste(twd$Date2, twd$Time, sep = " ")), 
                    id = twd$Animal_ID)

plot(w.ltraj)
#w.ltraj   # data.ltraj is a list               

# Each element of the list is a dataframe for one individual
head(w.ltraj[[1]])  # The first six locations of the first animal                  

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(w.ltraj[[1]], id = attr(w.ltraj[[1]], "id"))

# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(w.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(w.ltraj[[i]], id = attr(w.ltraj[[i]], "id")))
}

# Calculate distance travelled per day and add it to the dataframe
total.path.df$distperday <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per day for each turtle
path.summary <- aggregate(distperday~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperday~id, data = total.path.df, FUN = sd)$distperday

# Look at summmary dataframe
path.summary

# Make a graph to visualize data using ggplot
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperday + path.summary$sd, 
              ymin = path.summary$distperday - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperday, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per day (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot




        