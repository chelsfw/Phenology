rm(list=ls())
## Date: August 2020
## Author: C. Wilmer
## Project: WSFSFA Vegetation, Gradient Early Snowmelt Experiment
## Script: The purpose of this script is to generate soil water content summaries for the phenophase durations paper
## Packages:

library(tidyverse)
library(lubridate)

theme_set(theme_bw())

#### 1. Bring in data ####
# Set working directory
setwd("/Users/chelseawilmer/Desktop/CSU/Thesis")
swc<-read.csv("SM_SWC_summary.csv")

# get date into one column
swc$Date <- as.Date(with(swc, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

swc$DOY <- yday(swc$Date)

#### Soil Water Content Summaries ####



#### Create SnowCover/SnowFree data ####
#### Winter Regional Precip Data ####
## maximum snow
## snowmelt
#### Spring Regional Pricip Data ####
## spring snow
## spring rain
#### Summer Regional Precip Data ####
## summer rain
## monsoon
#### Fall Regional Precip Date ####
#### Soil Water Content for NL phase ####
## Difference calc
## Max/min calc
#### Soil Water Content for FLE phase ####
## Difference calc
## Max/min calc
#### Soil Water Content for FOF phase ####
## Difference calc
## Max/min calc
#### Soil Water Content for FLCC phase ####
## Difference calc
## Max/min calc