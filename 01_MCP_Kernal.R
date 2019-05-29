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

# set up working directories
here()

## load data into R
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
twdata <- twdata[twdata$Animal.ID != "",]

#if the month and year is missing then fill in wih original dataset values for Month and year 
nomonthdata <- twdata %>% filter(is.na(Month)) %>% mutate(Month = Month.0,Year = Year.0); length(nomonthdata$Animal.ID)
twdata <- twdata %>% drop_na(Month); length(twdata$Month)
twdata <- rbind(twdata, nomonthdata)

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
                                     
       
######################################################################

## Calculate Minimum convex polygons ################

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
 
  
  # minimum convex polygon? 
  tdf.0$Animal.ID <- droplevels(tdf.0$Animal.ID)
  tdf.u <- tdf.0[, c("Animal.ID", "Longitude", "Latitude","season","year.C")] 
  # filter for individuals with more than 50 locations 
  tdf.u <- tdf
  
  #tdf.u.df <- tdf.u %>% group_by(Animal.ID) %>%  summarise (count = n ())
  tdf <- tdf.u [tdf.u$Animal.ID %in% names(table(tdf.u$Animal.ID)) [table(tdf.u$Animal.ID) >= 50], ]
  tdf$Animal.ID <- droplevels(tdf$Animal.ID)
  
  
  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdf) <- c("Longitude", "Latitude")
  proj4string(tdf) <- CRS( "+proj=longlat +datum=WGS84 +units=m +no_defs" )
  tdfgeo <- spTransform(tdf, CRS("+init=epsg:3005")) # Transform to UTM 
  
  # Calculate MCPs for each wolf
  w.mcp <- mcp(tdfgeo, percent = 100)
  w.mcp.95 <- mcp(tdfgeo , percent = 95)
  
  #w.mcp # Examine output
  
  # Plot
  #plot(tdfgeo, col = as.factor(tdfgeo$Animal_ID), pch = 16)
  plot(w.mcp, col = alpha(1:5, 0.5), add = TRUE)
  plot(w.mcp.95, col = alpha(1:5, 0.5), add = TRUE)
  
  # Transform the point and MCP objects. 
  w.spgeo <- spTransform(tdfgeo, CRS("+proj=longlat"))
  w.mcpgeo <- spTransform(w.mcp, CRS("+proj=longlat"))
  
  # Turn the spatial data frame of points into just a dataframe for plotting in ggmap
  #w.geo <- data.frame(w.spgeo@coords, id = w.spgeo$Animal_ID )
  #mymap.hr <- ggplot() + 
  #  geom_polygon(data = fortify(w.mcpgeo),  
  #               # Polygon layer needs to be "fortified" to add geometry to the dataframe
  #               aes(long, lat, colour = id, fill = id),
  #               alpha = 0.3) + # alpha sets the transparency
  #  geom_point(data = w.geo, 
  #             aes(x = Longitude, y = Latitude, colour = id)) 
  # mymap.hr
  
  # Calculate the MCP by including 50 to 100 percent of points
  hrs <- mcp.area(tdf, percent = seq(50, 100, by = 10))
  hrs # examine dataframe
  
  # create a density plot for all individuals
  plot1 <- ggplot() + stat_density2d(
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 0.25),
    size = 0.01, bins = 10, data = twdata,
    geom = "polygon"
  ) 
  plot1
  
  
  

###############################################################################################
  
#### OLD EXAMPLE OF SCRIPT
   


# Population level 

# minimum convex polygon? 
w.sp <- twdata[, c("Animal.ID", "Longitude", "Latitude","season","Year")] 
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




        