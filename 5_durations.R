rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating durations
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, Year == 2018)
pheno <- filter(pheno, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

pheno <- pheno%>%
  group_by(Site, Block, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))


####Calculate durations as event2 - event1, event3 - event2 etc####
#calculate NL-DOYsf
pheno$durationNL <- pheno$FLE - pheno$NL
pheno$durationFLE <- pheno$FOF - pheno$FLE
pheno$durationFOF <- pheno$FLCC - pheno$FOF
#pheno$duratinoFLCC <- pheno$x - pheno$FLCC

durations <- select(pheno, -c(NL, FLE, FOF, FLCC))

#remove NA
durations <- na.omit(durations)

#reorganize data
durations <- durations %>% 
  pivot_longer(cols = c("durationNL", "durationFLE", "durationFOF"),
               names_to = "Event", values_to = "duration") 

durations <- durations%>%
  pivot_wider(names_from = Treatment, values_from = duration)

durations <- na.omit(durations)

durations <- durations%>%
  pivot_longer(cols = c("Control", "Treatment"),
               names_to = "Treatment", values_to = "duration")


