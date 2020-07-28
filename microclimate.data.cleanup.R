
## Date: 2019-01-27 
## Author: C. Livensperger
## Project: DOE Phenology Project
## Script: The purpose of this script is to bring in microclimate data, explore, and clean up dataset.
## Packages:
library(tidyverse)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

#### 1. Bring in data ####
# Set working directory
setwd("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno")

### 1a. Read in all .csv files together ####

## Get filenames from folder
file.names <- list.files("Microclimate", pattern = ".csv") # get all file names in directory

## Pull logger serial number, site name, block, and plot from file names
name.elements <- str_split(file.names, "_") #returns a list of each element separated by '_'
logger.nbr <- sapply(name.elements, function(x) x[1]) #returns the desired elements of the list 
site.name <- sapply(name.elements, function(x) x[3]); unique(site.name)
block.plot <- sapply(name.elements, function(x) x[4]); unique(block.plot)
#separate block and plot
blocks <- str_sub(block.plot, 1, 1)
plots <- str_sub(block.plot, 2, 2)

## Read in all .csv files as data frames 
file.names <- paste("Microclimate", file.names, sep = "/") #add folder pathway to file name
read.files <- function(x) {read.csv(x, skip = 1, header=TRUE, check.names = FALSE, strip.white = TRUE)} #set arguments for read.csv
all.files <- lapply(file.names, read.files) #apply to vector of file names, reading in all .csv's

## 1b. Read in logger and sensor number information ####
# Logger serial numbers identify plots and sensor serial numbers identify subplots
# This is eventually needed to join microclimate data to phenology data
sensors <- read.csv("Loggers_Sensors List.csv", strip.white = TRUE, col.names = c("Logger.SN", "Sensor.Type", "Sensor.SN", "Plot.Name", "Site", "Block", "Plot", "Treatment", "Subplot", "LR", "Comment"))

## 2. Clean up datasets ####
## 2a. Clean up columns in microclimate dataframe ####

# Change the column names in each dataframe in the list

# apply a function to each element of the list that subsets the column name to the sensor serial number, this is the unique identifier
change.names <- function(x) {setNames(x, sub(".*SEN S/N: ", "", colnames(x)))} #removes all characters before "SEN S/N:" if found
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

change.names <- function(x) {setNames(x, sub(",.*", "", colnames(x)))} #repeat to remove everything after a comma, this drops the ones with LBL
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

change.names <- function(x) {setNames(x, sub(")", "", colnames(x)))} #repeat again to remove remaining parentheses
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

# Drop the first column in every dataframe, these are just meaningless identifiers
all.files <- lapply(all.files, function(x) {x["#"] <- NULL; x})

# Add a column to every dataframe with the logger serial number
all.files <- mapply(cbind, all.files, "Logger"=logger.nbr, SIMPLIFY=F) #don't know why mapply

## 2b. Bind rows from all data frames in the list into one data frame ####
all.loggers <- bind_rows(all.files) # single data frame all data
## CHECK THIS, MIGHT NEED TO DROP LOTS OF EMPTY ROWS - HAVE SENSOR SERIAL NUMBERS WITH NO DATA IN LATER DATASETS

all.loggers$Batt <- NULL #drop the Batt column

# rearrange columns
all.loggers <- all.loggers[ ,c(1, 6, 2:5, 7:ncol(all.loggers))] #using bracket notation thanks to Joe, could also use rename()

# gather large data frame
all.loggers.vert <- gather(all.loggers, "Sensor.SN", "Measurement", 3:131) #beautiful vertical structure 

all.loggers.vert <- filter(all.loggers.vert, !is.na(Measurement)) #DROP NA'S bc each logger is repeated in gather which I don't want; this drops missing data but I think this is okay because it's a time series, can figure out missing days later if needed

## 2c. Clean up columns in sensor dataframe ####
sensors <- select(sensors, -c(Plot.Name)) # drop plot name
all.loggers.vert$Sensor.SN <- as.integer(all.loggers.vert$Sensor.SN) # match data types for join

## 3. Join microclimate and sensor information ####
## 3a. Join ####
### need to make sure all loggers and sensors serial numbers have associated info, i.e. block, plot, etc., otherwise will lose data in the join
### missing sensor serial numbers for USA B1 tmt and LSA A2 tmt
raw.microclimate <- left_join(all.loggers.vert, sensors, by = "Sensor.SN")

## 3b. Some quality control checks ####
# more sensor serial numbers in the all.loggers df than sensors, look at these obs
raw.data.sensors <- unique(all.loggers.vert$Sensor.SN)
sensor.list <- as.character(unique(sensors$Sensor.SN))
unknown.sensors <- setdiff(raw.data.sensors, sensor.list) #order matters! 

test <- subset(raw.microclimate, Logger == "2021995") #use to look at specific sensors
rm(test) #RECONCILED

# check that logger #'s from the raw data and loggers_sensors list match
match <- filter(raw.microclimate, raw.microclimate$Logger != raw.microclimate$Logger.SN) #only one of these - sensor SN 9719148
unique(match$Sensor.SN)
rm(match) #RECONCILED

## 3c. Some final cleanup ####
# drop Voltage observations
raw.microclimate <- filter(raw.microclimate, Sensor.Type != 'V')

# drop times
raw.microclimate$`Date Time` <- sub(" .*", "", raw.microclimate$`Date Time`) #strip hms, these are inconsistent and can't deal with right now, and actually unnecessary right because can still pull min and max temps and stuff

# establish date time as a date time data type
raw.microclimate$`Date Time` <- mdy(raw.microclimate$`Date Time`)

# separate into year, month, day
raw.microclimate$Year <- year(raw.microclimate$`Date Time`)
raw.microclimate$Month <- month(raw.microclimate$`Date Time`)
raw.microclimate$Day <- day(raw.microclimate$`Date Time`)

# rename Date Time column
raw.microclimate <- rename(raw.microclimate, Date =`Date Time`)

## 4.Make useful subsets of microclimate data ####
# Note - these subsets really need further quality control to check for out of range values and days with a small number of readings; would need to come up with a standard set of parameters for filtering this data

## 4a. Pull out ST and SWC
raw.st <- filter(raw.microclimate, Sensor.Type == "ST")
raw.swc <- filter(raw.microclimate, Sensor.Type == 'SWC')
raw.swc <- filter(raw.swc, Measurement > 0 & Measurement < 1) #quick filter to exclude out of range values but needs more exploration

# summarize with daily averages, min, and max
swc.daily <- raw.swc %>%
  group_by(Date, Year, Month, Day, Site, Block, Treatment) %>%
  summarise(mean = mean(Measurement, na.rm=T), 
            n = n())

ggplot(swc.daily, aes(Date, mean)) +
  geom_bar(stat='identity') +
  facet_grid(Site~Treatment) +
  labs(title = "All Sites, All Years", y = "Soil Water Content")

lsa.swc.2017 <- filter(swc.daily, Site == "LSA" & Year == '2017')

ggplot(lsa.swc.2017, aes(Date, mean)) +
  geom_bar(stat = 'identity') +
  labs(title = "Lower Subalpine 2017", y = "Soil Water Content")


## 5. Export datasets ####
#write.csv(swc.daily, "Daily SWC.csv")

#### what do do next ####
#this script is great for bringing in the microclimate data, joining it, cleaning it 
