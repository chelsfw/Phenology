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

# #add columns where a 1 is entered if there was an observation of a phenophase for that species
# pheno$countNL <- ifelse(pheno$NL != 0, 1, 0)
# pheno$countFLE <- ifelse(pheno$FLE != 0, 1, 0)
# pheno$countFOF <- ifelse(pheno$FOF != 0, 1, 0)
# pheno$countFLCC <- ifelse(pheno$FLCC != 0, 1, 0)
# 
# #make a dataframe with the counts of the number of species observed for each year/site/block/phenophase combination
# species <- pheno%>%
#   group_by(Year, Site, Block, Treatment)%>%
#   summarise(NL = sum(!is.na(countNL)),
#             FLE = sum(!is.na(countFLE)),
#             FOF = sum(!is.na(countFOF)),
#             FLCC = sum(!is.na(countFLCC)))

####YEAR####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

year <- year%>%
  group_by(Year, Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c('2018', '2017'))

year <- na.omit(year)

year <- year%>%
  pivot_wider(names_from = Event, values_from = yr_effect)

species <- year
species$NL <- ifelse(species$NL < 0 | species$NL > 0 | species$NL == 0, yes = 1, no = 0)
species$FLE <- ifelse(species$FLE < 0 | species$FLE > 0 | species$FLE == 0, yes = 1, no = 0)
species$FOF <- ifelse(species$FOF < 0 | species$FOF > 0 | species$FOF == 0, yes = 1, no = 0)
species$FLCC <- ifelse(species$FLCC < 0 | species$FLCC > 0 | species$FLCC == 0, yes = 1, no = 0)

year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "yr_effect") 

#early or late
year$early_late <- 0
year$early_late <- ifelse(test = year$yr_effect < 0, yes = "Advance", no = year$early_late)
year$early_late <- ifelse(test = year$yr_effect > 0, yes = "Delay", no = year$early_late)
year$early_late <- ifelse(test = year$yr_effect == 0, yes = "No_Change", no = year$early_late)
year <- na.omit(year)
year$count <- 1

#combine species and year dataframes
species_yr <- species%>%
  group_by(Site, Block, Treatment)%>%
  summarise(NL = sum(!is.na(NL)),
            FLE = sum(!is.na(FLE)),
            FOF = sum(!is.na(FOF)),
            FLCC = sum(!is.na(FLCC)))

species_yr <- species_yr %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "number_of_species") 

yr_percents <- year%>%
  group_by(Site, Block, Treatment, Event, early_late)%>%
  summarise(count = sum(!is.na(early_late)))

yr_percents <- yr_percents%>%
  pivot_wider(names_from = early_late, values_from = count)

yr_percent <- left_join(yr_percents, species_yr, by = c("Site", "Block", "Treatment", "Event"))

yr_percent$percent_advanced <- (yr_percent$Advance/yr_percent$number_of_species)*100
yr_percent$percent_delayed <- (yr_percent$Delay/yr_percent$number_of_species)*100
yr_percent$percent_no_change <- (yr_percent$No_Change/yr_percent$number_of_species)*100

yr_percent$Site <- factor(yr_percent$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
yr_percent_graph <- yr_percent%>%
  group_by(Site, Treatment, Event)%>%
  summarise(mean = mean(percent_advanced, na.rm = T))
yr_percent_graph$Elevation <- 0
yr_percent_graph$Elevation <- ifelse(yr_percent_graph$Site == "LM", 2774, yr_percent_graph$Elevation)
yr_percent_graph$Elevation <- ifelse(yr_percent_graph$Site == "UM", 2957, yr_percent_graph$Elevation)
yr_percent_graph$Elevation <- ifelse(yr_percent_graph$Site == "LSA", 3169, yr_percent_graph$Elevation)
yr_percent_graph$Elevation <- ifelse(yr_percent_graph$Site == "USA", 3475, yr_percent_graph$Elevation)
yr_percent_graph$Elevation <- ifelse(yr_percent_graph$Site == "ALP", 3597, yr_percent_graph$Elevation)

yr_percent_graph$Site <- factor(yr_percent_graph$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(yr_percent_graph, aes(Elevation, mean, color = Event))+
  geom_point()+
  geom_line()+
  labs(title = "Year Effect on Timing of Phenophases", y = "% Species Early", x = "Elevation (m)")

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

treatment <- treatment%>%
  pivot_wider(names_from = Event, values_from = trmt_effect)

species_trmt <- treatment
species_trmt$NL <- ifelse(species_trmt$NL < 0 | species_trmt$NL > 0 | species_trmt$NL == 0, yes = 1, no = 0)
species_trmt$FLE <- ifelse(species_trmt$FLE < 0 | species_trmt$FLE > 0 | species_trmt$FLE == 0, yes = 1, no = 0)
species_trmt$FOF <- ifelse(species_trmt$FOF < 0 | species_trmt$FOF > 0 | species_trmt$FOF == 0, yes = 1, no = 0)
species_trmt$FLCC <- ifelse(species_trmt$FLCC < 0 | species_trmt$FLCC > 0 | species_trmt$FLCC == 0, yes = 1, no = 0)


treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "trmt_effect") 


#early or late
treatment$early_late <- 0
treatment$early_late <- ifelse(test = treatment$trmt_effect < 0, yes = "Advance", no = treatment$early_late)
treatment$early_late <- ifelse(test = treatment$trmt_effect > 0, yes = "Delay", no = treatment$early_late)
treatment$early_late <- ifelse(test = treatment$trmt_effect == 0, yes = "No Change", no = treatment$early_late)
treatment <- na.omit(treatment)
treatment$count <- 1

#combine species and year dataframes
species_trmt <- species_trmt%>%
  group_by(Site, Block)%>%
  summarise(NL = sum(!is.na(NL)),
            FLE = sum(!is.na(FLE)),
            FOF = sum(!is.na(FOF)),
            FLCC = sum(!is.na(FLCC)))

species_trmt <- species_trmt %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "number_of_species") 

trmt_percents <- treatment%>%
  group_by(Site, Block, Event, early_late)%>%
  summarise(count = sum(!is.na(early_late)))

trmt_percents <- trmt_percents%>%
  pivot_wider(names_from = early_late, values_from = count)

trmt_percent <- left_join(trmt_percents, species_trmt, by = c("Site", "Block", "Event"))

trmt_percent$percent_advanced <- (trmt_percent$Advance/trmt_percent$number_of_species)*100
trmt_percent$percent_delayed <- (trmt_percent$Delay/trmt_percent$number_of_species)*100
trmt_percent$percent_no_change <- (trmt_percent$No_Change/trmt_percent$number_of_species)*100

trmt_percent$Site <- factor(trmt_percent$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
trmt_percent_graph <- trmt_percent%>%
  group_by(Site, Event)%>%
  summarise(mean = mean(percent_advanced, na.rm = T))

trmt_percent_graph$Elevation <- 0
trmt_percent_graph$Elevation <- ifelse(trmt_percent_graph$Site == "LM", 2774, trmt_percent_graph$Elevation)
trmt_percent_graph$Elevation <- ifelse(trmt_percent_graph$Site == "UM", 2957, trmt_percent_graph$Elevation)
trmt_percent_graph$Elevation <- ifelse(trmt_percent_graph$Site == "LSA", 3169, trmt_percent_graph$Elevation)
trmt_percent_graph$Elevation <- ifelse(trmt_percent_graph$Site == "USA", 3475, trmt_percent_graph$Elevation)
trmt_percent_graph$Elevation <- ifelse(trmt_percent_graph$Site == "ALP", 3597, trmt_percent_graph$Elevation)

trmt_percent_graph$Site <- factor(trmt_percent_graph$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(trmt_percent_graph, aes(Elevation, mean, color = Event))+
  geom_point()+
  geom_line()+
  labs(title = "Treatment Effect on Timing of Phenophases", y = "% Species Early", x = "Elevation (m)")










