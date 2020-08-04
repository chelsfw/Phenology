library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

rm(list=ls())
#### Gather and combine soil water content files ####
#Set directory to the place where the files are saved
getwd()
setwd("/Users/chelseawilmer/Desktop/Github/WSFSFA-Veg/Microclimate/LM and LSA 2019 test")

#Read in the list of file names
ndvi.files <- list.files(path = ".")[grep("*.csv",(list.files(path = ".")))]
ndvi.files

#Using functions from dplyr and data.table to append files - any column that isn't duplicated across files auto fills with NA

#bind_rows 
combined_files <- bind_rows(lapply(ndvi.files, fread, skip=2, header=TRUE))


# establish date data type
combined_files$Date <- mdy(combined_files$Date)


## Make DOY and Year columns, convert time to numeric and filter

combined_files$DOY = yday(combined_files$Date)

combined_files$Year = year(combined_files$Date)
summary(combined_files)
str(combined_files)
combined_files$Time <- as.numeric(gsub("[:punct:]", "", combined_files$Time))

combined_files$SWC = rowMeans(combined_files[,c("SWC.1", "SWC.2")], na.rm=TRUE)

combined_files$SWC[which(combined_files$SWC<=-0.05 | combined_files$SWC>=0.35)] = NA # filtering out data that would be unreasonable for swc
ggplot(combined_files, aes(x=SWC)) +geom_histogram(color="black", fill="white", binwidth=.0001)
summary(combined_files$SWC)

site.labs <- c("Lower Subalpine", "Lower Montane")
names(site.labs) <- c("LSA", "LM")


daily_means <- combined_files %>%
  group_by(Year, Site, Treatment, Plot, DOY) %>%
  summarise(ST = mean(ST, na.rm = T),
            SWC = mean(SWC, na.rm = T))

daily_means$ST[which(daily_means$ST<=0 | daily_means$ST>=100)] = 0

#daily_means <- gather(daily_means, key = "Climate", value = "Value", 6:7) %>% 
#  group_by(Climate) #key

ggplot(daily_means, aes(DOY, ST, color = Plot))+
  geom_point()+
  facet_grid(Site~Treatment)

ggplot(daily_means, aes(DOY, SWC, color = Plot))+
  geom_point()
