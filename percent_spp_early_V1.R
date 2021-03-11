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
  summarise(NL = sum(!is.na(countNL)),
            FLE = sum(!is.na(countFLE)),
            FOF = sum(!is.na(countFOF)),
            FLCC = sum(!is.na(countFLCC)))

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
year$early_late <- ifelse(test = year$yr_effect < 0, yes = "Advance", no = year$early_late)
year$early_late <- ifelse(test = year$yr_effect > 0, yes = "Delay", no = year$early_late)
year$early_late <- ifelse(test = year$yr_effect == 0, yes = "No_Change", no = year$early_late)
year$count <- 1

#combine species and year dataframes
species_yr <- filter(species, Treatment == "Control")

species_yr <- species_yr %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "number_of_species") 

species_yr <- species_yr%>%
  pivot_wider(names_from = Year, values_from = number_of_species)

yr_percents <- year%>%
  group_by(Site, Block, Treatment, Event, early_late)%>%
  summarise(count = sum(!is.na(early_late)))

yr_percents <- yr_percents%>%
  pivot_wider(names_from = early_late, values_from = count)

yr_percent <- left_join(yr_percents, species_yr, by = c("Site", "Block", "Treatment", "Event"))

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
  pivot_wider(names_from = Treatment, values_from = meanDOY)

treatment$trmt_effect <- treatment$Early - treatment$Control

treatment <- select(treatment, -c(Early, Control))

treatment <- na.omit(treatment)

#early or late
treatment$early_late <- 0
treatment$early_late <- ifelse(test = treatment$trmt_effect < 0, yes = "Advance", no = treatment$early_late)
treatment$early_late <- ifelse(test = treatment$trmt_effect > 0, yes = "Delay", no = treatment$early_late)
treatment$early_late <- ifelse(test = treatment$trmt_effect == 0, yes = "No Change", no = treatment$early_late)
treatment$count <- 1

#combine species and year dataframes
species_trmt <- filter(species, Year == "2018")
species_trmt <- filter(species_trmt, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

species_trmt <- species_trmt%>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "number_of_species") 

species_trmt <- species_trmt%>%
  pivot_wider(names_from = Treatment, values_from = number_of_species)

trmt_percents <- treatment%>%
  group_by(Site, Block, Event, early_late)%>%
  summarise(count = sum(!is.na(early_late)))

trmt_percents <- trmt_percents%>%
  pivot_wider(names_from = early_late, values_from = count)

trmt_percent <- left_join(trmt_percents, species_trmt, by = c("Site", "Block", "Event"))

trmt_percent <- select(trmt_percent, -c(Year))






