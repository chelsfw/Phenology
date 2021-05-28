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

#boxplots of day of year start of phenophase faceted by phase
year$Elevation <- as.factor(year$Elevation)
year$Year <- as.factor(year$Year)
nl <- year%>%
  filter(Event == "NL")%>%
  ggplot(aes(Elevation, DOY, color = Year))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 300))+
  labs(title = "Timing of New Leaves", y = "Julian Day of Year")
nl

fle <- year%>%
  filter(Event == "FLE")%>%
  ggplot(aes(Elevation, DOY, color = Year))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 300))+
  labs(title = "Timing of Full Leaf Expansion", y = "Julian Day of Year")
fle

fof <- year%>%
  filter(Event == "FOF")%>%
  ggplot(aes(Elevation, DOY, color = Year))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 300))+
  labs(title = "Timing of First Open Flower", y = "Julian Day of Year")
fof

flcc <- year%>%
  filter(Event == "FLCC")%>%
  ggplot(aes(Elevation, DOY, color = Year))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 300))+
  labs(title = "Timing of Full Leaf Color Change", y = "Julian Day of Year")
flcc

ggarrange(nl, fle, fof, flcc, common.legend = T, legend = "right")

####TRMT####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

treatment$Elevation <- as.factor(treatment$Elevation)
treatment$Treatment <- as.factor(treatment$Treatment)

nl <- treatment%>%
  filter(Event == "NL")%>%
  ggplot(aes(Elevation, DOY, color = Treatment))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 250))+
  labs(title = "Timing of New Leaves", y = "Julian Day of Year")
nl

fle <- treatment%>%
  filter(Event == "FLE")%>%
  ggplot(aes(Elevation, DOY, color = Treatment))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 250))+
  labs(title = "Timing of Full Leaf Expasion", y = "Julian Day of Year")
fle

fof <- treatment%>%
  filter(Event == "FOF")%>%
  ggplot(aes(Elevation, DOY, color = Treatment))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 250))+
  labs(title = "Timing of First Open Flower", y = "Julian Day of Year")
fof

flcc <- treatment%>%
  filter(Event == "FLCC")%>%
  ggplot(aes(Elevation, DOY, color = Treatment))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(100, 250))+
  labs(title = "Timing of Full Leaf Color Change", y = "Julian Day of Year")
flcc

ggarrange(nl, fle, fof, flcc, common.legend = T, legend = "right")

####YEAR SCATTER####
year$Site <- factor(year$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))
nl <- year%>%
  filter(Event == "NL")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "New Leaves")
nl

fle <- year%>%
  filter(Event == "FLE")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "Full Leaf Expansion")
fle

fof <- year%>%
  filter(Event == "FOF")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "First Open Flower")
fof

flcc <- year%>%
  filter(Event == "FLCC")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "Full Leaf Color Change")
flcc

ggarrange(nl, fle, fof, flcc, common.legend = T, legend = "right")

####TRMT SCATTER####
treatment$Site <- factor(treatment$Site, c("LM", "UM", "LSA", "USA", "ALP"))
nl <- treatment%>%
  filter(Event == "NL")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "New Leaves")
nl

fle <- treatment%>%
  filter(Event == "FLE")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "Full Leaf Expansion")
fle

fof <- treatment%>%
  filter(Event == "FOF")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "First Open Flower")
fof

flcc <- treatment%>%
  filter(Event == "FLCC")%>%
  ggplot(aes(DOYsf, DOY, color = Site))+
  geom_point()+
  theme_bw()+
  labs(title = "Full Leaf Color Change")
flcc

ggarrange(nl, fle, fof, flcc, common.legend = T, legend = "right")
