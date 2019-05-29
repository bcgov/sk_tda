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
# File to automate graphic for telemetry data

# help files/.
#http://rstudio-pubs-static.s3.amazonaws.com/175678_754df2122024463a89d9ebedf277714e.html
#https://www.littlemissdata.com/blog/maps
https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf


#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)

#devtools::session_info()
#install.packages("ggplot2")
#install.packages("anipaths")

library(ggplot2)
library(ggmap)
library(rjson)
library(digest)
library(glue)
library(purrr)
library(lubridate)
library(anipaths)
#remotes::install_github("dkahle/ggmap")

register_google(key = "INSERT KEY HERE",  # your Static Maps API key
                account_type = "standard")

map <- get_map(location = c(-125.5654,53.36959), zoom =7)

ggmap(map)

## load data into R

twdata=as.data.frame(read.csv(paste(data.folder,"TweedsmuirWolves_May2018_time.csv",sep = ""), header = T,stringsAsFactors = T))

aoi = c("TWW1009","TWW1010", "TWW1011", "TWW1012","TWW1013", "TWW1014","TWW1009", "TWW1010")
        TWW1011 TWW1012 TWW1013 TWW1014 TWW101
# Animal Id;s 

twdata = twdata %>% dplyr::filter(Animal_ID %in% aoi)

# fomat dates to make it easier to interogate by adding new columns
twdata$Date2=dmy(twdata$Date) 
twdata$Month <- month(twdata$Date2) # break out DMY columns
twdata$Day <- day(twdata$Date2)
twdata$Year<- year(twdata$Date2)
twdata$Time2 <- hms(twdata$Time) #check to see if better function 
twdata$DateTime <-ymd_hms(paste(twdata$Date2,twdata$Time)) 


twdata$POSIX <- as.POSIXct(twdata$DateTime,tz = "UTC") 

wolf_paths <- twdata
#wolf_paths <- twdata[format(twdata$POSIX, "%Y") == 2017, ] ## limit attention to 2009

# select the step interval 
delta.t <- "day"
delta.t <- "week"


#select backgroud map 
background <- get_map(location = c(-125.5654,53.24959), zoom =8)
#background <- list(location = c(-125,53),zoom = 9,maptype = "satellite")

#map <- get_map(location = c(-125.5654,53.24959), zoom =8,alpha=0.4) ; basemap = ggmap(map)

animate_paths(paths = wolf_paths, 
              delta.t = delta.t,
              paths.proj = "+proj=longlat",
              coord = c("Longitude", "Latitude"),
              Time.name = "POSIX",
              ID.name = "Animal_ID", 
              background = background ,
              return.paths = FALSE,
              whole.path= FALSE,
              pt.colors = RColorBrewer::brewer.pal(n = 9, "RdYlGn"),
              interval = 1/6)


animate_paths(paths = wolf_paths, 
              delta.t = delta.t,
              paths.proj = "+proj=longlat",
              coord = c("Longitude", "Latitude"),
              Time.name = "POSIX",
              ID.name = "Animal_ID", 
              background = background ,
              return.paths = FALSE,
              whole.path= TRUE, tail.length = 1,
              pt.colors = RColorBrewer::brewer.pal(n = 9, "RdYlGn"),
              interval = 1/6)


animate_paths(paths = cbind(vultures_paths, COVARIATE),
              delta.t = "week",
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX", covariate = "COVARIATE", 
              covariate.colors = RColorBrewer::brewer.pal(n = 9, "RdYlGn"),
              ID.name = "individual.local.identifier", 
              background = background)

