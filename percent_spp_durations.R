rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####Durations for YEAR####
year <- year%>%
  group_by(Year, Site, Block, Treatment, Species, EOS)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))

#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
year$NL <- year$FLE - year$NL
year$FLE <- year$FOF - year$FLE
year$FOF <- year$FLCC - year$FOF
year$FLCC <- year$EOS - year$FLCC

#reorganize data
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "duration") 

year <- select(year, -c(EOS))
year <- na.omit(year)

year <- year%>%
  pivot_wider(names_from = Year, values_from = duration)

year <- na.omit(year)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c("2017", "2018"))

year <- year%>%
  pivot_wider(names_from = Event, values_from = yr_effect)

#number of species
species <- year
species$NL <- ifelse(species$NL < 0 | species$NL > 0 | species$NL == 0, yes = 1, no = 0)
species$FLE <- ifelse(species$FLE < 0 | species$FLE > 0 | species$FLE == 0, yes = 1, no = 0)
species$FOF <- ifelse(species$FOF < 0 | species$FOF > 0 | species$FOF == 0, yes = 1, no = 0)
species$FLCC <- ifelse(species$FLCC < 0 | species$FLCC > 0 | species$FLCC == 0, yes = 1, no = 0)

year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "yr_effect") 

#early or late
year$increase_decrease <- 0
year$increase_decrease <- ifelse(test = year$yr_effect < 0, yes = "Decrease", no = year$increase_decrease)
year$increase_decrease <- ifelse(test = year$yr_effect > 0, yes = "Increase", no = year$increase_decrease)
year$increase_decrease <- ifelse(test = year$yr_effect == 0, yes = "No_Change", no = year$increase_decrease)
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
  group_by(Site, Block, Treatment, Event, increase_decrease)%>%
  summarise(count = sum(!is.na(increase_decrease)))

yr_percents <- yr_percents%>%
  pivot_wider(names_from = increase_decrease, values_from = count)

yr_percent <- left_join(yr_percents, species_yr, by = c("Site", "Block", "Treatment", "Event"))

yr_percent$percent_decreased <- (yr_percent$Decrease/yr_percent$number_of_species)*100
yr_percent$percent_increased <- (yr_percent$Increase/yr_percent$number_of_species)*100
yr_percent$percent_no_change <- (yr_percent$No_Change/yr_percent$number_of_species)*100

yr_percent$Site <- factor(yr_percent$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
yr_percent_graph <- yr_percent%>%
  group_by(Site, Treatment, Event)%>%
  summarise(mean = mean(percent_increased, na.rm = T))

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
  theme_bw()+
  scale_y_continuous(limits = c(0, 100))+
  labs(title = "Year Effect on Duration of Phenophases", y = "% Species Decreased Duration", x = "Elevation (m)")

ggplot(yr_percent_graph, aes(Elevation, mean, color = Event))+
  geom_point()+
  geom_line()+
  theme_bw()+
  scale_y_continuous(limits = c(0, 100))+
  labs(title = "Year Effect on Duration of Phenophases", y = "% Species Increased Duration", x = "Elevation (m)")
####Durations for TRMT####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, EOS)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))

#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
treatment$NL <- treatment$FLE - treatment$NL
treatment$FLE <- treatment$FOF - treatment$FLE
treatment$FOF <- treatment$FLCC - treatment$FOF
treatment$FLCC <- treatment$EOS - treatment$FLCC


treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "duration") 

treatment <- select(treatment, -c(EOS))
#reorder dataset to get events back into columns
treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = duration)

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
treatment$increase_decrease <- 0
treatment$increase_decrease <- ifelse(test = treatment$trmt_effect < 0, yes = "Decrease", no = treatment$increase_decrease)
treatment$increase_decrease <- ifelse(test = treatment$trmt_effect > 0, yes = "Increase", no = treatment$increase_decrease)
treatment$increase_decrease <- ifelse(test = treatment$trmt_effect == 0, yes = "No_Change", no = treatment$increase_decrease)
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
  group_by(Site, Block, Event, increase_decrease)%>%
  summarise(count = sum(!is.na(increase_decrease)))

trmt_percents <- trmt_percents%>%
  pivot_wider(names_from = increase_decrease, values_from = count)

trmt_percent <- left_join(trmt_percents, species_trmt, by = c("Site", "Block", "Event"))

trmt_percent$percent_decreased <- (trmt_percent$Decrease/trmt_percent$number_of_species)*100
trmt_percent$percent_increased <- (trmt_percent$Increase/trmt_percent$number_of_species)*100
trmt_percent$percent_no_change <- (trmt_percent$No_Change/trmt_percent$number_of_species)*100

trmt_percent$Site <- factor(trmt_percent$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
trmt_percent_graph <- trmt_percent%>%
  group_by(Site, Event)%>%
  summarise(mean = mean(percent_increased, na.rm = T))

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
  theme_bw()+
  scale_y_continuous(limits = c(0,100))+
  labs(title = "Treatment Effect on Duration of Phenophases", y = "% Species Decreased Duration", x = "Elevation (m)")

ggplot(trmt_percent_graph, aes(Elevation, mean, color = Event))+
  geom_point()+
  geom_line()+
  theme_bw()+
  scale_y_continuous(limits = c(0,100))+
  labs(title = "Treatment Effect on Duration of Phenophases", y = "% Species Increased Duration", x = "Elevation (m)")
