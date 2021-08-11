rm(list = ls())
#Date: 08-11-2021
#User: CWilmer
#SFA/CSU Thesis
#Purpose: scatterplots of timing vs. duration

####Bring in phenology data####
#load packages
library(tidyverse)

#load source code
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

piecewise <- select(pheno, c(Year, Site, Elevation, Block, Subplot, Treatment, Species, DOYsf, EOS, NL, FLE, FOF, FLCC))

piecewise_timing <- piecewise%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Timing")

#calculate duration of events
piecewise_duration <- mutate(piecewise, NL = FLE-NL, FLE = FOF-FLE, FOF = FLCC-FOF, FLCC = EOS-FLCC)
piecewise_duration <- piecewise_duration%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Duration")

piecewise <- left_join(piecewise_timing, piecewise_duration, by = c("Year", "Site", "Elevation", "Block", "Subplot", "Treatment", "Species", "DOYsf", "EOS", "Event"))

piecewise <- filter(piecewise, Duration > 0)
piecewise_yr <- filter(piecewise, Treatment == "Control")
piecewise_trmt <- filter(piecewise, Year == "2018")

piecewise_yr$Event <- factor(piecewise_yr$Event, levels = c("NL", "FLE", "FOF", "FLCC"))
ggplot(piecewise_yr, aes(Duration, Timing, color = Elevation))+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  facet_grid(Event~Year)

piecewise_trmt$Event <- factor(piecewise_trmt$Event, levels = c("NL", "FLE", "FOF", "FLCC"))
piecewise_trmt$Site <- factor(piecewise_trmt$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(piecewise_trmt, aes(Duration, Timing, color = Site))+
  geom_point()+
  scale_x_continuous(limits = c(0, 100))+
  facet_grid(Event~Treatment) #can do scales = "free" 
