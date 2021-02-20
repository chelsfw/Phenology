
## Date: June 2020
## Author: A. Henderson, adapted from C. Livensperger
## Project: WSFSFA Vegetation, Gradient Early Snowmelt Experiment
## Script: The purpose of this script is to bring in microclimate data, explore, and clean up dataset.
## Packages:

library(data.table)
library(tidyverse)
library(lubridate)
library(reshape2) ## for NDVI only

theme_set(theme_bw())

#### 1. Bring in data ####
# Set working directory
setwd("/Users/chelseawilmer/Desktop/Github/wsfsfa_veg_microclimate")
metalogger<-read.csv("Microclimate_Sensors.csv")
all_serial_numbers <- unique(metalogger$Logger)

file.names <- list.files("/Users/chelseawilmer/Desktop/Github/wsfsfa_veg_microclimate/SM_Microclimate_csv", pattern = ".csv")

name.elements <- str_split(file.names, "_") #returns a list of each element separated by '_'
logger.nbr <- sapply(name.elements, function(x) x[1]) #returns the desired elements of the list 
site.name <- sapply(name.elements, function(x) x[3]); unique(site.name)
block.plot <- sapply(name.elements, function(x) x[4]); unique(block.plot)

#separate block and plot
blocks <- str_sub(block.plot, 1, 1)
plots <- str_sub(block.plot, 2, 2)


## Read in all .csv files as data frames 
file.names <- paste("/Users/chelseawilmer/Desktop/Github/wsfsfa_veg_microclimate/SM_Microclimate_csv", file.names, sep = "/") #add folder pathway to file name
read.files <- function(x) {read.csv(x, skip = 1, header=TRUE, check.names = FALSE, strip.white = TRUE)} #set arguments for read.csv
all.files <- lapply(file.names, read.files) #apply to vector of file names, reading in all .csv's

## Change temperatures logged in Fahrenheit to Celcius
change.temp <- function(x) { if(length(grep("Temp, °F", colnames(x)))>0){
  x[,grep("Temp, °F", colnames(x))] <- (x[,grep("Temp, °F", colnames(x))] - 32) * (5/9);x} else {x}
}

all.files = sapply(all.files, change.temp)

## 2. Clean up datasets ####
## 2a. Clean up columns in microclimate dataframe ####

# Remove Voltage, Current, and Battery columns
all.files <- lapply(all.files, function(x) {x[grep("Voltage", colnames(x))] <- NULL; x})

all.files <- lapply(all.files, function(x) {x[grep("Current", colnames(x))] <- NULL; x})

all.files <- lapply(all.files, function(x) {x[grep("Batt", colnames(x))] <- NULL; x})

# apply a function to each element of the list that subsets the column name to the sensor serial number, this is the unique identifier
change.names <- function(x) {setNames(x, sub(".*SEN S/N: ", "", colnames(x)))} #removes all characters before "SEN S/N:" if found
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

change.names <- function(x) {setNames(x, sub(",.*", "", colnames(x)))} #repeat to remove everything after a comma, this drops the ones with LBL
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

change.names <- function(x) {setNames(x, sub(")", "", colnames(x)))} #repeat again to remove remaining parentheses
all.files <- lapply(all.files, change.names) #can simplify to one line of code by putting function in lapply

# Drop the first column in every dataframe, these are just meaningless identifiers
all.files <- lapply(all.files, function(x) {x["#"] <- NULL; x})

# Add a column to every dataframe with the logger serial number. Place new column after Date Time to simplify gather.
all.files <- mapply(add_column, all.files, "Logger"=logger.nbr, .after = 1) #don't know why mapply

# Loop for changing data structure file by file

for(i in 1:length(all.files)){
  all.files[[i]] = gather(all.files[[i]], "Sensor.SN", "Measurement", 3:ncol(all.files[[i]]))
  
}

## 2b. Bind rows from all data frames in the list into one data frame ####
all.plot <- bind_rows(all.files) # single data frame all data

## 2c. Remove duplicated data rows due to overlap in downloads
# Returns list of duplicated data
all.plot[duplicated(all.plot), ]
# Remove duplicated data
all.plot = all.plot[!duplicated(all.plot), ]

## 2d. Change error code to NA
all.plot$Measurement[which(all.plot$Measurement<= -100)] = NA

all.plot$Sensor.SN <- as.integer(all.plot$Sensor.SN) # match data types for join

## 3. Join microclimate and sensor information ####
## 3a. Join ####
### need to make sure all loggers and sensors serial numbers have associated info, i.e. block, plot, etc., otherwise will lose data in the join
### missing sensor serial numbers for USA B1 tmt and LSA A2 tmt
raw.microclimate <- left_join(all.plot, metalogger, by = "Sensor.SN") 

### If sensors don't move, metadata will be correct other than logger
match <- filter(raw.microclimate, raw.microclimate$Logger.x != raw.microclimate$Logger.y)
uniquematch = data.frame(match$Logger.x, match$Logger.y, match$Sensor.SN)
unique(uniquematch)

## 3c. Some final cleanup ####

# Change date time from character, need to use parse_date_time (lubridate)
# because files have different time formats

raw.microclimate$`Date Time` = parse_date_time(raw.microclimate$`Date Time`, orders = c("mdy IMS p", "mdY HM"), tz = "Etc/GMT-6")

raw.microclimate$DOY = yday(raw.microclimate$`Date Time`)

raw.microclimate$Year <- year(raw.microclimate$`Date Time`)
raw.microclimate$Month <- month(raw.microclimate$`Date Time`)
raw.microclimate$Day <- day(raw.microclimate$`Date Time`)
raw.microclimate$Hour <- hour(raw.microclimate$`Date Time`)

####### Part 2: Clean data ranges and create summary data files ########

## 1. Additional quality control ####
# check number of measurements per day, value ranges for each metric, and filter accordingly

## Soil Water Content ####
raw.swc <- filter(raw.microclimate, Sensor.Type == 'SWC')

raw.swc <- filter(raw.swc, Measurement > 0 & Measurement < 0.6) ### based on sensor range from onset
hist(raw.swc$Measurement) #looks good

ggplot(raw.swc, aes(x = DOY, y = Measurement, color = Treatment)) + 
  geom_point() + facet_grid(Site~Year)

swc.2020 = filter(raw.swc, Year == 2020)
ggplot(swc.2020, aes(x = `Date Time`, y = Measurement, color = Block, shape = Left.Right)) + 
  geom_point() + facet_grid(Site~Treatment)

swc.daily <- raw.swc %>%
  group_by(Year, Month, Day, Site, Plot, Block, Treatment, Left.Right) %>% 
  dplyr::summarise(mean = mean(Measurement, na.rm=T), 
            n = n(), 
            min = min(Measurement, na.rm=T), max = max(Measurement, na.rm=T),
            sd = sd(Measurement, na.rm=T))

write.csv(swc.daily, file="SM_SWC_summary.csv")

######### NDVI: PAR and Solar Radiation ##########
raw.ndvi = filter(raw.microclimate, Sensor.Type == 'PAR.up'| Sensor.Type == 'PAR.down' | Sensor.Type == 'SR.up'| Sensor.Type == 'SR.down')

## filter for +- 2 hours solar noon, time zone is MDT, UTC - 6
noon.ndvi = filter(raw.ndvi, between(Hour, 11, 15))

## Widen data to relate sensors at sub-plot level,
## do upward and downward separately
down.sensors = filter(noon.ndvi, Sensor.Type == "PAR.down" | Sensor.Type == "SR.down")
wide.down = dcast(down.sensors, `Date Time` + Site + Treatment + Left.Right ~ Sensor.Type , value.var = "Measurement")

up.sensors = filter(noon.ndvi, Sensor.Type == "PAR.up" | Sensor.Type == "SR.up")
wide.up = dcast(up.sensors, `Date Time` + Site + Treatment + Left.Right ~ Sensor.Type , value.var = "Measurement")

## Unit conversion
wide.down$PAR.down = wide.down$PAR.down *0.21
wide.up$PAR.up = wide.up$PAR.up *0.21

## Filter data based on sensor specs and visual inspection of histograms
wide.up = filter(wide.up, between(SR.up, 1000, 1275))
wide.up = filter(wide.up, between(PAR.up, 375, 530))
wide.down = filter(wide.down, between(SR.down, 0, 300))
wide.down = filter(wide.down, between(PAR.down, 0, 100))

### Summarize incoming PAR and SR at the site level for each DateTime
incoming.par <- wide.up %>%
  group_by(`Date Time`, Site) %>% 
  summarise(par.up = mean(PAR.up, na.rm=T))

incoming.sr = wide.up %>%
  group_by(`Date Time`, Site) %>%
  summarise(sr.up = mean(SR.up, na.rm=T))

### Joining down-facing sensors with incoming averages
### Note, if loggers have different time stamps (immediate start vs delayed start)
###     then data will be lost in the join
ndvi <- full_join(wide.down, incoming.par, by = c("Date Time" = "Date Time", 'Site' = 'Site'))
ndvi <- full_join(ndvi, incoming.sr, by = c("Date Time" = "Date Time", 'Site' = 'Site'))
ndvi$DOY = yday(ndvi$`Date Time`)
ndvi$Year = year(ndvi$`Date Time`)


###NDVI Calculations
ndvi$qPAR = ndvi$PAR.down/ndvi$par.up
ndvi$qOIR = (ndvi$SR.down - ndvi$PAR.down) / (ndvi$sr.up - ndvi$par.up)
ndvi$NDVI = (ndvi$qOIR - ndvi$qPAR) / (ndvi$qOIR + ndvi$qPAR)

ndvi = filter(ndvi, between(NDVI, 0, 1))

### Summarize for daily sub-plot level NDVI
ndvi.daily <- ndvi %>%
  group_by(Year, DOY, Site, Treatment, Left.Right) %>% 
  summarise(mean = mean(NDVI, na.rm=T), n = n(), 
            max = max(NDVI, na.rm=T),
            sd = sd(NDVI, na.rm=T))

## write file for Heidi
write.csv(ndvi.daily, file= "SM_NDVI_all.csv")

ggplot(ndvi.daily, aes(x=DOY, y=max, color=as.factor(Year))) +
  geom_point() + facet_grid(Site~Treatment)
ggplot(ndvi.daily, aes(x=DOY, y=max, color=Treatment)) +
  geom_point() + facet_grid(Year~Site)
  
