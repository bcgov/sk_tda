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


# run logistic models to see if 


#install.packages(c("arm","lme4","glmmTMB","bbmle"),dep = TRUE)
library(arm)
library(tidyr)  
library(dplyr)                                       # for summarizing data
library(stringr)
library(lubridate)
library(maptools)
library(rgdal)                                      # export to SpatialLite
library(ggplot2)
library(reshape2)  
library(base)
library(glmmTMB)
library(lme4)
options(stringsAsFactors=FALSE)


# read in data sheet 

pts <- read.csv(paste(field.data.folder,"wolf_pts_att3005_test.csv",sep = "/"),header = TRUE)
pts <- pts [complete.cases(pts), ] # remove NA values 

pts <- pts 

#OR  remove the NA from Presence/ Absence column
#row.has.na <- apply(pts, 1, function(x){any(is.na(x))}); sum(row.has.na)
#pts <- pts[!row.has.na,]


## Explore the layers to check we have coverage 

copts<- pts %>% dplyr::select(type,AspectAOI,DEM50m3005,SlopeAOI,Animal_ID)
p1 <- ggplot(copts, aes(AspectAOI)) + geom_histogram() + facet_grid(~type) ; p1
p2 <- ggplot(copts, aes(DEM50m3005)) + geom_histogram() + facet_grid(~type) ; p2
p3 <- ggplot(copts, aes(SlopeAOI)) + geom_histogram() + facet_grid(~type) ; p3


# correlation plot
cor.matrix = cor(training, method = c("pearson"))
cor.matrix


## may need to standardise/centre the data 
## look at spread of data 

# convert grouping variables to factors
copts <- mutate(copts, Animal_ID.f = as.factor(Animal_ID))

# scale numeric variables of interest
copts$elev.s = scale(copts$DEM50m3005)
copts$slope.s = scale(copts$SlopeAOI)
copts$aspect.s = scale(copts$AspectAOI)




# run logistic regression models: 

# null model

m1 <- glmer(type ~ 1 + (1|Animal_ID),family = binomial,data = copts) 

summary(m1)
ranef(m1)
coef(m1)

# full model set 
m2 <- glmer(type ~ elev.s + slope.s + aspect.s + (1|Animal_ID),family = binomial,data = copts) 

AIC(m1,m2)

# run series of models:




library(lme4)
############################################################################
# Prep data for modelling: 

#1) convert grouping variables to factors
mdata <- mutate(mdata, BEC_ss.f = as.factor(BEC_ss))
mdata <- mutate(mdata,BEC_sz.f = as.factor(BEC_sz))
mdata <- mutate(mdata,Block.f = as.factor(Block))
mdata <- mutate(mdata,Edge.t.f = as.factor(Edge.T))

# 2) remove the NA from Presence/ Absence column
row.has.na <- apply(mdata, 1, function(x){any(is.na(x))}); sum(row.has.na)
mdata <- mdata[!row.has.na,]

# 3) scale numeric variables of interest
mdata$elev.s = scale(mdata$Elevation)
mdata$edge.s = scale(mdata$Edge)
mdata$Hdist.s = scale(mdata$Hdist)

#### STILL TO DO 
# dont use the HSR as very skewed. 

# remove the two BEC_ss.f (31 and 32 - as outlier samples)
mdata <- mdata[mdata$BEC_ss < 30,] 
unique(mdata$BEC_ss)

############################################################################

# Run candidate model - Presence/Absence 

m1 <- glmer(pPA ~ Edge + (1|Block.f), data = mdata, family = 'binomial')
m2 <- glmer(pPA ~ Edge + elev.s + (1|Block.f), data = mdata, family = 'binomial')
m3 <- glmer(pPA ~ Edge + elev.s + Edge.T +(1|Block.f), data = mdata, family = 'binomial')
m4 <- glmer(pPA ~ edge.s + Edge.T + (1|Block.f),data = mdata, family= 'binomial')
m5 <- glmer(pPA ~ Edge.T + (1|Block.f),data = mdata, family = 'binomial')
m6 <- glmer(pPA ~ Edge + elev.s + Edge.t.f +(1|Block.f), data = mdata, family = 'binomial')
m7 <- glmer(pPA ~ edge.s + BEC_ss + (1|Block.f), data = mdata,family = 'binomial')
m8 <- glmer(pPA ~ edge.s + elev.s + BEC_ss + (1|Block.f), data = mdata,family = 'binomial')
m9 <- glmer(pPA ~ edge.s + Edge.T + BEC_ss + (1|Block.f), data = mdata,family = 'binomial')

## Model average??? as none are greater then 2AIC unit
AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9)

# Take a look at the outputs
summary(m1)
summary(m2)
summary(m7)
summary(m8)
summary(m9)
