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



# Create available points to match our telemtry data
# Firstly we must review the data (script 01_data_prep.r) then or before decide at the heirachial level we are interested in

# Q: do we analyse the data per population (MCP) or by individual step (buffer) or both? 

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
#register_google(key = "INSERTAPI",  # your Static Maps API key
#                account_type = "standard")


# set up folders: 

getwd()
data.folder = "C:/Temp/TweedWolf/Data/"

## load data into R
twdata=as.data.frame(read.csv(paste(data.folder,"TweedsmuirWolves_May2018_time.csv",sep = ""), header = T,stringsAsFactors = T))


#######################################
## Population level Domain: 
#######################################

w.sp <- twdata[, c("Animal_ID", "Longitude", "Latitude")] 
# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(w.sp) <- c("Longitude", "Latitude")
proj4string(w.sp) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )

# Calculate MCPs for each wolf
w.mcp <- mcp(w.sp, percent = 100)
plot(w.mcp)
#w.mcp.sf <- st_as_sf(w.mcp, coords = c("Longitude","Latitude"), crs = 4326)
# convert to sf ??

w.avail.pts <- spsample(w.mcp, 100,type = "stratified")

plot(w.mcp) 
plot(w.avail.pts, add = T)

## add a bunch of random points : still to be done. 
#https://gis.stackexchange.com/questions/108046/how-to-create-randomly-points-within-polygons-for-each-row-of-a-dataframe-matchi



##OR 


#########################################
## Individual level Domain: 
#########################################

tw$ID = seq(1:length(tw$Animal_ID))
# convert to spatial data and change to projected data to calculate correct buffer by areaa
tw.sf = st_as_sf(tw, coords = c("Longitude","Latitude"), crs = 4326)
tw.3005 <- st_transform(tw.sf,3005)
#st_write(tw.3005,paste(data.folder,"Wolf_locations.shp",sep = "")) 

tw.buf <- st_buffer(tw.3005,5000) # currently set to buffer at 5m but can change to distance per animal/season/

# get subset of points to work with 
tw.buf <- tw.buf[1:3,]
plot(st_geometry(tw.buf[1:4,])) 
plot(st_geometry(tw.3005[1:3,]),col = "red",add = TRUE,pch= 16)

# create random points within each polygon 
tw.rpts <- st_sample(tw.buf,30,add = TRUE)
plot(st_geometry(tw.rpts),add = T, col = "blue",pch = 3)# plot to see the locations 

# write out the data table 
# random pts 
rpts <- as.data.frame(st_coordinates(tw.rpts)) 
rpts$type <- 0

# used pts 
pts <-as.data.frame(st_coordinates(tw.3005))
pts$type <- 1

# add together 
all.pts <- rbind(rpts,pts)

write.csv(all.pts,paste(data.folder,"wolf_random_pts.csv",sep = ""))




