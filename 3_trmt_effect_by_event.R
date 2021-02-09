rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating the timing of events for the effect the treatment
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

#calculate means by block
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment_stats <- na.omit(treatment)

#calculate means by treatment
treatment <- treatment%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = meanDOY)

#calculate treatment effect
treatment$effect <- treatment$Treatment - treatment$Control

####NL####
NL <- filter(treatment, Event == "NL")

NL <- na.omit(NL)

lm <- NL%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LM")

um <- NL%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "UM")

lsa <- NL%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LSA")

usa <- NL%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLE####
FLE <- filter(treatment, Event == "FLE")

FLE <- na.omit(FLE)

lm <- FLE%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-25,10))+
  labs(title = "LM")

um <- FLE%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-25,10))+
  labs(title = "UM")

lsa <- FLE%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-25,10))+
  labs(title = "LSA")

usa <- FLE%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-25,10))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FOF####
FOF <- filter(treatment, Event == "FOF")

FOF <- na.omit(FOF)

lm <- FOF%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,20))+
  labs(title = "LM")

um <- FOF%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,20))+
  labs(title = "UM")

lsa <- FOF%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,20))+
  labs(title = "LSA")

usa <- FOF%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,20))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLCC####
FLCC <- filter(treatment, Event == "FLCC")

FLCC <- na.omit(FLCC)

lm <- FLCC%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "LM")

um <- FLCC%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "UM")

lsa <- FLCC%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "LSA")

usa <- FLCC%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

