rm(list=ls())
source("gradient_microclimate_summaries.R")
library(tidyverse)
library(plyr)

pheno_microclimate <- raw.swc %>%
  group_by(Year, Site, DOY, Treatment) %>%
  dplyr::summarise(daily_mean_swc = mean(Measurement, na.rm = T))

pheno_microclimate <- filter(pheno_microclimate, Year == 2017 | Year == 2018)
pheno_microclimate$Site <- factor(pheno_microclimate$Site, levels = c("ALP", "USA", "LSA", "UM", "LM"))
ggplot(pheno_microclimate, aes(x=DOY, y=daily_mean_swc, color = Treatment))+
  geom_point()+
  facet_grid(Site~Year)
                   