rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

#add columns where a 1 is entered if there was an observation of a phenophase for that species
pheno$countNL <- ifelse(pheno$NL != 0, 1, 0)
pheno$countFLE <- ifelse(pheno$FLE != 0, 1, 0)
pheno$countFOF <- ifelse(pheno$FOF != 0, 1, 0)
pheno$countFLCC <- ifelse(pheno$FLCC != 0, 1, 0)

#make a dataframe with the counts of the number of species observed for each year/site/block/phenophase combination
species <- pheno%>%
  group_by(Year, Site, Block, Treatment)%>%
  summarise(speciesNL = sum(!is.na(countNL)),
            speciesFLE = sum(!is.na(countFLE)),
            speciesFOF = sum(!is.na(countFOF)),
            speciesFLCC = sum(!is.na(countFLCC)))

####YEAR####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
year <- year%>%
  group_by(Year, Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year <- year%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

#reorder
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "meanDOY") 

year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c('2018', '2017'))

year <- na.omit(year)

#early or late
year$early_late <- 0
year$early_late <- ifelse(test = year$yr_effect < 0, yes = "Early", no = "Late")
year$count <- 1

#summary of percent species early or late at each site
early_late <- year%>%
  group_by(Site, Block, Event, early_late)%>%
  summarise(count = sum(count))

early_late <- early_late%>%
  pivot_wider(names_from = early_late, values_from = count)

####TRMT --> haven't done anything with this yet####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

#reorder dataset to get events back into columns
treatment <- treatment%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

#add a column called count and add a 1 if any phenophase what observed
treatment$count <- ifelse(treatment$NL >0 | treatment$FLE >0 | treatment$FOF >0 | treatment$FLCC >0 , 1, 0)

#create a new dataframe that will count the 1's from the count column to total the number of species that were observed in the plot
species_per_plot_trmt <- treatment%>%
  group_by(Site, Block, Treatment)%>%
  summarise(number_of_species = sum(!is.na(count)))



