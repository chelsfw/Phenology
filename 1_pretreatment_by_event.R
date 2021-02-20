rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating the timing of events for the effect of the year
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
pretreatment <- filter(pheno, Year == 2017)
pretreatment <- filter(pretreatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
pretreatment <- pretreatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
pretreatment <- pretreatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

#calculate means by treatment
pretreatment <- pretreatment%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

pretreatment <- pretreatment%>%
  pivot_wider(names_from = Treatment, values_from = meanDOY)

#calculate treatment effect
pretreatment$effect <- pretreatment$Treatment - pretreatment$Control

####NL####
NL <- filter(pretreatment, Event == "NL")

NL <- na.omit(NL)

lm <- NL%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-18,10))+
  labs(title = "LM")

um <- NL%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-18,10))+
  labs(title = "UM")

lsa <- NL%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-18,10))+
  labs(title = "LSA")

usa <- NL%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-18,10))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLE####
FLE <- filter(pretreatment, Event == "FLE")

FLE <- na.omit(FLE)

lm <- FLE%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,15))+
  labs(title = "LM")

um <- FLE%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,15))+
  labs(title = "UM")

lsa <- FLE%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,15))+
  labs(title = "LSA")

usa <- FLE%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,15))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FOF####
FOF <- filter(pretreatment, Event == "FOF")

FOF <- na.omit(FOF)

lm <- FOF%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "LM")

um <- FOF%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "UM")

lsa <- FOF%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "LSA")

usa <- FOF%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,20))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLCC####
FLCC <- filter(pretreatment, Event == "FLCC")

FLCC <- na.omit(FLCC)

lm <- FLCC%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "LM")

um <- FLCC%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "UM")

lsa <- FLCC%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "LSA")

usa <- FLCC%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)
