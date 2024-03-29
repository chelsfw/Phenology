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

####YEAR####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#average over subplots
year <- year%>%
  group_by(Year, Site, Block, Treatment, Species, EOS, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year <- year%>%
  group_by(Year, Site, Treatment, Species, EOS, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

year <- year%>%
  group_by(Year, Site, Treatment, EOS, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

year_duration <- year%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

year_duration$NL <- year_duration$FLE - year_duration$NL
year_duration$FLE <- year_duration$FOF - year_duration$FLE
year_duration$FOF <- year_duration$FLCC - year_duration$FOF
year_duration$FLCC <- year_duration$EOS - year_duration$FLCC

year_duration <- year_duration%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Duration")

year <- year%>%
  group_by(Year, Site, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

year_duration <- year_duration%>%
  group_by(Year, Site, Event)%>%
  summarise(Duration = mean(Duration, na.rm = T))

#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment%>%
  group_by(Site, Block, Treatment, EOS, Species)%>%
  summarise(NL = mean(NL, na.rm = T),
            FLE = mean(FLE, na.rm = T),
            FOF = mean(FOF, na.rm = T),
            FLCC = mean(FLCC, na.rm = T))

treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, EOS, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, Species, EOS, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, EOS, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment_duration <- treatment%>%
  pivot_wider(names_from = Event, values_from = meanDOY)

treatment_duration$NL <- treatment_duration$FLE - treatment_duration$NL
treatment_duration$FLE <- treatment_duration$FOF - treatment_duration$FLE
treatment_duration$FOF <- treatment_duration$FLCC - treatment_duration$FOF
treatment_duration$FLCC <- treatment_duration$EOS - treatment_duration$FLCC

treatment_duration <- treatment_duration%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Duration")

treatment <- treatment%>%
  group_by(Site, Treatment, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment_duration <- treatment_duration%>%
  group_by(Site, Treatment, Event)%>%
  summarise(Duration = mean(Duration, na.rm = T))

#combine
trmt <- left_join(treatment, treatment_duration, by = c("Site", "Treatment", "Event"))

yr <- left_join(year, year_duration, by = c("Year", "Site", "Event"))

####Timing x Duration####
#year split by site
yr$Year <- as.factor(yr$Year)
lm <- yr%>%
  filter(Site == "LM")%>%
  ggplot(aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Lower Montane")
lm

um <- yr%>%
  filter(Site == "UM")%>%
  ggplot(aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Upper Montane")
um

lsa <- yr%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Lower Subalpine")
lsa

usa <- yr%>%
  filter(Site == "USA")%>%
  ggplot(aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Upper Subalpine")
usa

alp <- yr%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Alpine")
alp

ggarrange(lm, um, lsa, usa, alp, nrow = 3, ncol = 2, common.legend = T, legend = "right")

#try faceting
yr$Site <- factor(yr$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(yr, aes(Duration, meanDOY, color = Year, shape = Event))+
  geom_point()+
  facet_grid(~Site)+
  coord_fixed(ratio = 1, xlim = c(0,100), ylim = c(120,243))+
  labs(title = "Year Effect", y = "DOY")

#trmt split by site
lm <- trmt%>%
  filter(Site == "LM")%>%
  ggplot(aes(Duration, meanDOY, color = Treatment, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,80), ylim = c(120,230))+
  labs(title = "Lower Montane")
lm

um <- trmt%>%
  filter(Site == "UM")%>%
  ggplot(aes(Duration, meanDOY, color = Treatment, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,80), ylim = c(120,230))+
  labs(title = "Upper Montane")
um

lsa <- trmt%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Duration, meanDOY, color = Treatment, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,80), ylim = c(120,230))+
  labs(title = "Lower Subalpine")
lsa

usa <- trmt%>%
  filter(Site == "USA")%>%
  ggplot(aes(Duration, meanDOY, color = Treatment, shape = Event))+
  geom_point()+
  coord_fixed(ratio = 1, xlim = c(0,80), ylim = c(120,230))+
  labs(title = "Upper Subalpine")
usa

ggarrange(lm, um, lsa, usa, common.legend = T, legend = "right")

#try faceting
trmt$Site <- factor(trmt$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(trmt, aes(Duration, meanDOY, color = Treatment, shape = Event))+
  geom_point()+
  facet_grid(~Site)+
  coord_fixed(ratio = 1, xlim = c(0,80), ylim = c(120,230))+
  labs(title = "Treatment Effect", y = "DOY")

