rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating the timing of events for the effect of the year and the treatment
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0)

####reorder dataset so that NL, FLE, FOF, and FLCC are in column 'Event' and the values are in column 'DOY'####
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

####Effect of year and year + treatment on mean date of events####
year <- pheno%>%
  group_by(Year, Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

#reorder dataset to calculate the difference in the timing of events for specific species between years
year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

#omit NA values after pivot wider
year <- na.omit(year)

#calculate difference between years
year$effect_of_year <- year$`2018`- year$`2017`

#remove years colums for pivot wider
year <- select(year, -c("2017", "2018"))

#reorder dataset to calculate the difference in timing of events for specific species between the treatment and control
treatment <- year%>%
  pivot_wider(names_from = Treatment, values_from = effect_of_year)

#omit observations with NA values after pivot
treatment <- na.omit(treatment)

#calculate difference in timing between treatments
treatment$effect_of_treatment <- treatment$Treatment - treatment$Control

treatment <- select(treatment, -c("Control", "Treatment"))

####Visuals####
#year
#filter year dataset for control for these figures but rerun lines 1-53 for treatment figures
year <- filter(year, Treatment == "Control")
year$Event<- factor(year$Event, levels = c("NL", "FLE", "FOF","FLCC"))

year%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Alpine", y = "number of days early (-) or delayed (+)")

year%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days early (-) or delayed (+)")

year%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Montane", y = "number of days early (-) or delayed (+)")

year%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect_of_year))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Subalpine", y = "number of days early (-) or delayed (+)")

year%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect_of_year, fill = Treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Subalpine", y = "number of days early (-) or delayed (+)")

#treatment
treatment$Event<- factor(treatment$Event, levels = c("NL", "FLE", "FOF","FLCC"))
treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days early (-) or delayed (+)")

treatment%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Montane", y = "number of days early (-) or delayed (+)")

treatment%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Subalpine", y = "number of days early (-) or delayed (+)")

treatment%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Subalpine", y = "number of days early (-) or delayed (+)")

####Next Steps####
#why doesn't LM treatment have any data?!?
#how to do summary stats (sd, se)
#difference from control
#investigating where the durations data is at
