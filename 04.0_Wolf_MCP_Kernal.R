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

# set up working directories
getwd()

data.folder = "C:/Temp/TweedWolf/Data/" # point to your data directory

## load data into R
twdata=as.data.frame(read.csv(paste(data.folder,"TweedsmuirWolves_May2018_time.csv",sep = ""), header = T,stringsAsFactors = T))

# error check the data 
head(twdata); 
names(twdata) ; 
str(twdata)

unique(twdata$Animal_ID) # 12 individuals

# select the columns of interst 
twdata<- twdata %>% dplyr::select( Obj_ID,Animal_ID, Date,Time,Latitude,Longitude)

##############################################################
#### PART 1: DATA EXPLORATION #####

# fomat dates to make it easier to interogate by adding new columns
twdata$Date2=dmy(twdata$Date) 
twdata$Month <- month(twdata$Date2) # break out DMY columns
twdata$Day <- day(twdata$Date2)
twdata$Year<- year(twdata$Date2)
twdata$Time2 <- hms(twdata$Time) #check to see if better function 
twdata$DateTime <-ymd_hms(paste(twdata$Date2,twdata$Time)) 

# Q : HOw many fixes do we have per animal ??
p1 <- ggplot(twdata) + geom_bar(aes(Animal_ID)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)); p1

id.date <- twdata %>% group_by(Animal_ID,Year,Month) %>% summarise(count = n()) ; id.date

# Q:  what about temporal variability - number of years? time of year? 
p2 <- ggplot(id.date,aes(x = Year,count)) + geom_bar(stat ="identity") + facet_wrap(~Animal_ID) ; p2
p3 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity") + facet_wrap(~Animal_ID); p3

## questions?  - what do we do with the low count individuals? Do we have enough for seasonal blocks? 
# Q how much data do we have when we combine all years and all animals? 
p4 <- ggplot(id.date,aes(x = Month,count)) + geom_bar(stat ="identity"); p4

# Q : how do the individuals overlap spatially? 
p5 = ggplot(twdata, aes(Longitude,Latitude)) +geom_point()+facet_wrap(~Animal_ID); p5
p6 = ggplot(twdata, aes(Longitude,Latitude,colour = Animal_ID)) +geom_point() ; p6


####################################################################
###################################################################
## How are animals using the area? 
## 
## Depends on scale of interest : 
##  - Hierachial order of selection 
##  -   Entire study area? / Population level? // individaual levels 

######################################################################

# Population level 

# minimum convex polygon? 
w.sp <- twdata[, c("Animal_ID", "Longitude", "Latitude")] 
# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(w.sp) <- c("Longitude", "Latitude")
proj4string(w.sp) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )

# Calculate MCPs for each wolf
w.mcp <- mcp(w.sp, percent = 100)
w.mcp.95 <- mcp(w.sp, percent = 95)

w.mcp # Examine output

# Plot
plot(w.sp, col = as.factor(w.sp@data$Animal_ID), pch = 16)
plot(w.mcp, col = alpha(1:5, 0.5), add = TRUE)
plot(w.mcp.95, col = alpha(1:5, 0.5), add = TRUE)

# Transform the point and MCP objects. 
w.spgeo <- spTransform(w.sp, CRS("+proj=longlat"))
w.mcpgeo <- spTransform(w.mcp, CRS("+proj=longlat"))

# Turn the spatial data frame of points into just a dataframe for plotting in ggmap
w.geo <- data.frame(w.spgeo@coords, id = w.spgeo@data$Animal_ID )
mymap.hr <- ggplot() + 
  geom_polygon(data = fortify(w.mcpgeo),  
               # Polygon layer needs to be "fortified" to add geometry to the dataframe
               aes(long, lat, colour = id, fill = id),
               alpha = 0.3) + # alpha sets the transparency
  geom_point(data = w.geo, 
             aes(x = Longitude, y = Latitude, colour = id)) 

mymap.hr

# Calculate the MCP by including 50 to 100 percent of points
hrs <- mcp.area(w.sp, percent = seq(50, 100, by = 5))
hrs # examine dataframe

# create a density plot for all individuals
plot1 <- ggplot() + stat_density2d(
  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 0.25),
  size = 0.01, bins = 10, data = twdata,
  geom = "polygon"
) 
plot1



## Calculate Kernal density estiamtes ################

w.sp <- twdata[, c("Animal_ID", "Longitude", "Latitude")] 

# get summary of points per animal 
indiv <- w.sp %>% group_by(Animal_ID) %>% summarise(count = n())
indiv

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(w.sp) <- c("Longitude", "Latitude")
proj4string(w.sp) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
# Transform the point and MCP objects. 
w.spgeo <- spTransform(w.sp, CRS("+proj=longlat"))

# using Href method
kud_points  <- kernelUD(w.spgeo, h = "href", same4all = TRUE,grid = 500, kern = c("bivnorm"))
# or 
# using LSVC method 
#kud_points  <- kernelUD(w.spgeo, h = "LSCV", same4all = TRUE, kern = c("bivnorm"))

image(kud_points)# view the outputs (note if the same4all = TRUE it will be the full extent not zoomed per individual)

# get vertices
ver95 <- getverticeshr(kud_points,95) # need to enlarge the extent to get to a 95 kernal density 
ver50 <- getverticeshr(kud_points,50)
plot(ver95) 
plot(ver50,add = TRUE,col = "red")

#convert sp to sf object to make easier to writeout as shapefiles::
ver95.sf <- st_as_sf(ver95)
ver50.sf <- st_as_sf(ver50)

# write out as a shapefile

st_write(ver95.sf,paste(data.folder,"KernalhR95.shp",sep = ""))
st_write(ver50.sf,paste(data.folder,"KernalhR50.shp",sep = ""))










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




        