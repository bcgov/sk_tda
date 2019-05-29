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

#install.packages("lubridate")
#install.packages("devtools")
#devtools::install_github("tidyverse/lubridate")

# run libraries
#library(lattice)  
library(ggplot2)
library(scales)
library(dplyr)
#library(xlsx)
library(readxl) 
library(GGally)
library(lubridate)
library(sf)
library(sp)
#library(adehabitatHR)
library(ggmap)
library(scales)
library(ggmap)

# set up working directories
here()

# set up a key for google basemap 
register_google(key = "AIzaSyA1TiLkUKEnyq8HzmxThgndAy5gvDmRnNc",  # your Static Maps API key
                account_type = "standard")

## load data into R
twdata <- read.csv("5177_TEC GPS telemetry 2014-ongoing_April18_2019.csv",header = TRUE,stringsAsFactors = TRUE) 

# error check the data 
head(twdata); 
names(twdata) ; 
str(twdata)

unique(twdata$Animal.ID) # 45? individuals (with blank) 

## fix the misssing Aniaml_ID value 

# select the columns of interst 
twdata<- twdata %>% dplyr::select(Animal.ID, Date.Time,Latitude..ø., Longitude..ø., Height..m.,DOP,FixType,LMT_Time,Mort..Status)
twdata <- twdata %>%  mutate(Latitude = Latitude..ø., Longitude =Longitude..ø. )

##########################################
# Mort status 
twdata<- twdata %>% dplyr::select(Animal.ID, Date.Time, Latitude, Longitude,Mort..Status,Height..m.)
length(twdata$Animal.ID)
twdata <- twdata[twdata$Animal.ID != "",]
morts <- twdata %>% group_by(Mort..Status) %>% summarise(mort.no = n())

## check these = "" = Normal 
## 1033
mort.no = c('Mortality no fix','Mortlity no radius','Mortality no radius')
morts <- twdata %>% dplyr::filter(Mort..Status %in% mort.no)
#length(morts$Animal.ID)
twdata <- morts

##############################################################
#### PART 1: DATA EXPLORATION #####
twdata$Date2=as.Date(twdata$Date.Time, format = '%Y-%m-%d') 
twdata$Year<- year(twdata$Date2)
twdata$Month <- month(twdata$Date2) # break out DMY columns
twdata$Day <- day(twdata$Date2)
twdata$Date.j <- julian(twdata$Date2)#Add julian day
twdata$Hours <- as.numeric(format(as.POSIXct(strptime(twdata$Date.Time,"%Y-%m-%d %H:%M",tz="")) ,format = "%H"))

# group cariou into seasons  
twdata <- twdata %>% mutate(season = ifelse(Month %in% c(4,5),"SM",
                                     ifelse(Month == 6,"C",
                                     ifelse(Month %in% c(7,8,9),"S",
                                     ifelse(Month == 10,"R", 
                                     ifelse(Month == 11,"FM",
                                     ifelse(Month %in% c(12,1,2,3),"W", NA)))))))

## Add a caribou year (december = following year)
twdata <- twdata %>% mutate(year.C = ifelse(Month %in% c(1:11),Year,
                                     ifelse(Month == 12,Year + 1, NA)))

id.date <- twdata %>% group_by(year.C,Month) %>% summarise(count = n()) ; id.date
id.season <- twdata %>% group_by(year.C,season) %>% summarise(count = n()) ; id.season

# make a plot 
# Q:  what about temporal variability - number of years? time of year? 
p2 <- ggplot(id.date,aes(x = year.C,count)) + geom_bar(stat ="identity") 
p3 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity") ; p3
p3 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity")+ facet_wrap(~year.C) + 
    scale_x_discrete(labels=c("J","F","M","A","M","J","J","A", "S","O","N","D")) 

#p3 <- ggplot(id.date,aes(x = Month,count,fill = year.C)) + geom_bar(stat='identity',position = ) ; p3 #+ facet_wrap(~year.C) ; p3
#p3 <- ggplot(id.season,aes(x = year.C,count)) + geom_bar(stat ="identity")+ facet_wrap(~season) ; p3

# check the spread of Caribou with maped locations 
basemap <- get_map(location = c(-126.0654,53.36959), zoom =8)

### TO FIX : Size of points increase and decreast 

#p5 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude))+facet_wrap(~Animal.ID); p5
#p6 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,colour = season),pch = 16); p6
p6 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,colour = season),pch = 16,size = 3) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Caribou mortality sites by season (2015 - 2018)" ); p6
  ggsave("Map.caribou.morts.season.google.jpeg")
    
#p6 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,colour = Animal.ID, size= season))+ 
# ggtitle("Mortality site per season (2015 - 2019")
#p7 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude,size = 2,colour = Animal.ID)) + facet_wrap(~season) ; p7# + 

p8 = ggmap(basemap) + geom_point(data = twdata, aes(x = Longitude, y = Latitude),size = 3) +
  xlab("Longitude") + ylab("Latitude") + ggtitle(" Caribou Mortality sites (2015 - 2018)" ); p8

  ggsave("Map.caribou.morts.google.jpeg")



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

        