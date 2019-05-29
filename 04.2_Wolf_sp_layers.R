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

# read in the data layers and extract values where the points intersect 

library(raster)

# set up folders

setwd("C:/Temp/TweedWolf/")          #check the home directory  # set up work directory 
field.data.folder = ("Data")         
input.folder = ("C:/Temp/TweedWolf/layers/")                

pts = read.csv(paste(field.data.folder,"/","wolf_random_pts.csv",sep = ''),stringsAsFactors = FALSE)

LatLon <- pts %>% dplyr::select(c(X,Y,type))     # extract the Lat/Longs. 
LatLon <- na.omit(LatLon)         #length(LatLon$Longitude) # error check:check the length of the files

# get co-ordinates and convert from WGs to albers to match the base layers
coordinates(LatLon)=~X + Y
proj4string(LatLon)=CRS("+init=epsg:3005") # set it to lat-long

#plot(pts)

###################################################################
#### Step 2) Select layers you want to extract by data you want to use. 
###################################################################

# list contains all raster files we want to extract the attributes from. 
layers.list = as.list(list.files(input.folder,full.names=TRUE))

## check the list of rasters to extract from:
Covariates <-  list.files(path=input.folder,recursive=TRUE, full.names=TRUE, all.files=TRUE, pattern ="\\.tif$")
Covariates <-  stack(Covariates,quick = TRUE)
#plot(Covariates)
  
proj4string(Covariates) <- CRS("+init=epsg:3005") 
attributes <- raster::extract(Covariates, coordinates(LatLon), df=TRUE)
  
attributes <- as.data.frame(attributes)
LatLon <- as.data.frame(LatLon)
training <- cbind(LatLon,attributes)
  
write.csv(training, paste(field.data.folder,"wolf_pts_att3005.csv",sep = "/"))

########################################################################






