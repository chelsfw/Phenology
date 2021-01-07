rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Using means early on to do similar calcs with durations and deltas
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#calculate event means
event_means <- pheno%>%
  group_by(Year, Site, Block, Treatment, Functional.Type)%>%
  summarise(DOYSF = mean(DOYsf, na.rm = T),
            NL = mean(NL, na.rm = T),
            FLE = mean(FLE, na.rm = T),
            FOF = mean(FOF, na.rm = T),
            FLCC = mean(FLCC, na.rm = T))


event_means <- event_means %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#remove na
event_means <- na.omit(event_means)

####durations####
event_means_durations <- event_means %>% 
  group_by(Year, Site, Treatment, Functional.Type, Event) %>% #added Block to get sd, n, se
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T), #added 8/20 8:50am
            #sd = sd(DOY, na.rm = T), #added 8/20 8:50am
            n = n()) #added 8/20 8:50am
            #se = sd/sqrt(n)) #added 8/20 8:50am

#calculate durations
event_means_durations$durartion <- event_means_durations$max - event_means_durations$min


####deltas####
event_means_deltas <- event_means_durations %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = durartion)

#calculate deltas
event_means_deltas$delta_duration <- event_means_deltas$`2018`- event_means_deltas$`2017`
