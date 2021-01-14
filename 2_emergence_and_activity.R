rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating the duration of emergence and activity
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) #playing around with difference between & and | to filter. if '&' it cuts the data down to only observations that have values for ALL phenophases that year.

#SF and activity period
pheno$activity <- pheno$FLCC-pheno$FLE
pheno$emergence <- pheno$NL-pheno$DOYsf

####reorder dataset so that NL, FLE, FOF, and FLCC are in column 'Event' and the values are in column 'DOY'####
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("activity", "emergence"),
               names_to = "Event", values_to = "number_of_days") 

pheno <- select(pheno, -c(DOYsf, NL, FLE, FOF, FLCC))

pheno$Event<- factor(pheno$Event, levels = c("emergence", "activity"))

#compare duration of emergence and activity for all species at the lower montane between years
pheno%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, number_of_days, fill = Event))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Year)+
  labs(title = "Lower Montane", y = "number of days")

####Effect of year and year + treatment on mean date of events####
#summarise pheno data for mean number of days emergence and activity period happened
year <- pheno%>%
  group_by(Year, Site, Treatment, Species, Event)%>%
  summarise(mean_days = mean(number_of_days, na.rm = T))

#reorder dataset to calculate the difference in the timing of events for specific species between years
year <- year%>%
  pivot_wider(names_from = Year, values_from = mean_days)

#omit NA values after pivot wider
year <- na.omit(year)

#calculate difference between years
year$mean_days <- year$`2018`- year$`2017`

#remove years colums for pivot wider
year <- select(year, -c("2017", "2018"))

####Visuals####
#filter year dataset for control for year effects but rerun above code before doing treatment visuals
year <- filter(year, Treatment == "Control")
year$Event<- factor(year$Event, levels = c("emergence", "activity"))

year%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, mean_days))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days shorter (-) or longer (+)")

year%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, mean_days))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Montane", y = "number of days shorter (-) or longer (+)")

year%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, mean_days))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Subalpine", y = "number of days shorter (-) or longer (+)")

year%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, mean_days))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Subalpine", y = "number of days shorter (-) or longer (+)")

####reorder dataset to calculate the difference in timing of events for specific species between the treatment and control####
treatment <- year%>%
  pivot_wider(names_from = Treatment, values_from = mean_days)

#omit observations with NA values after pivot
treatment <- na.omit(treatment)

#calculate difference in timing between treatments
treatment$effect_of_treatment <- treatment$Treatment - treatment$Control

treatment <- select(treatment, -c("Control", "Treatment"))

treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Montane", y = "number of days shorter (-) or longer (+)")

treatment%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Montane", y = "number of days shorter (-) or longer (+)")

treatment%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Lower Subalpine", y = "number of days shorter (-) or longer (+)")

treatment%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect_of_treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~Event)+
  labs(title = "Upper Subalpine", y = "number of days shorter (-) or longer (+)")

####Next Steps####
#would be interesting to identify species as being either earlier (-) or later (+) and the duration of events for them as shorter (-) or longer (+) (both at some level of significance) and then do some kind of investigation of the proportion of species that were earlier or later or shorter or longer
#investigate how this changes over phenophases and across elevations