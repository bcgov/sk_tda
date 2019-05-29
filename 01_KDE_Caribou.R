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
register_google(key = "INSERTYOURKEYHERE",  # your Static Maps API key
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
  
  
  
 
  