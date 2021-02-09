rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: processing date to make figure like in Gugger et al. 
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the treatment year 2018
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment_duration <- treatment%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

treatment_duration$NL <- treatment_duration$FLE - treatment_duration$NL
treatment_duration$FLE <- treatment_duration$FOF - treatment_duration$FLE
treatment_duration$FOF <- treatment_duration$FLCC - treatment_duration$FOF
treatment_duration$FLCC <- 277 - treatment_duration$FLCC

treatment_duration <- treatment_duration%>%
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "Duration") 

timing_and_duration <- cbind(treatment, treatment_duration)

##to do
#nl=fle-nl ----> done
#fle=fof-fle ----> done
#fof=flcc-fof ----> done
#flcc=datesnowcovered/temperaturethreshold-flcc ----> done but used max date of FLCC, need better date
#get cbind to work

#visualize


