rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Processing data to make figure like Post et al. 2008
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

####Number of days snowmelt advance####
#snowmelt dates dataframe
snowmelt_yr <- pheno%>%
  group_by(Year, Site, Block, Plot, Treatment)%>%
  summarise(DOYsf = mean(DOYsf, na.rm = T))

snowmelt_yr <- snowmelt_yr%>%
  pivot_wider(names_from = Year, values_from = DOYsf)

snowmelt_yr$advance_yr <- snowmelt_yr$`2017` - snowmelt_yr$`2018`

#snowmelt advance between early/control
snowmelt_trmt <- pheno%>%
  group_by(Year, Site, Block, Treatment)%>%
  summarise(DOYsf = mean(DOYsf, na.rm = T))

snowmelt_trmt <- filter(snowmelt_trmt, Year == 2018)

snowmelt_trmt <- snowmelt_trmt%>%
  pivot_wider(names_from = Treatment, values_from = DOYsf)

snowmelt_trmt$advance_trmt <- snowmelt_trmt$Control - snowmelt_trmt$Early

####Snow off to snow EOS####
snow_off_to_EOS <- pheno%>%
  group_by(Year, Site, Block, Plot, Treatment)%>%
  summarise(DOYsf = mean(DOYsf, na.rm = T),
            EOS = mean(EOS, na.rm = T))

snow_off_to_EOS$DOYsf_EOS <- snow_off_to_EOS$EOS - snow_off_to_EOS$DOYsf

snow_off_to_EOS <- select(snow_off_to_EOS, -c(DOYsf, EOS))

snow_off_to_EOS <- snow_off_to_EOS%>%
  pivot_wider(names_from = Year, values_from = DOYsf_EOS)

snow_off_to_EOS$delta_DOYsf_EOS <- snow_off_to_EOS$`2018` - snow_off_to_EOS$`2017`

snow_off_to_EOS <- select(snow_off_to_EOS, -c("2017", "2018"))



