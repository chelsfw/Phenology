rm(list = ls())
## Date: 2020-08-19
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in duration data####
setwd("/Users/chelseawilmer/Desktop/Github/Phenology")
deltas <- read.csv("/Users/chelseawilmer/Desktop/Github/Phenology/deltas.csv")

deltas <- deltas%>%
  select(-c(X))

deltas$Event <- factor(deltas$Event, levels = c("FLCC", "FOF", "FLE", "NL"))

####What's the change in duration for 2017pretrmt and 2018trmt? 2017ctrl and 2018ctrl?####
deltas$Event <- factor(deltas$Event, levels = c("NL", "FLE", "FOF", "FLCC"))
deltas%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, delta_duration, fill = Event))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  coord_flip()+
  facet_grid(~Treatment)+
  labs(y = "Change in duration of events between 2018 and 2017 (# of days)")

deltas%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, delta_duration, fill = Event))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  coord_flip()+
  facet_grid(~Treatment)+
  labs(y = "Change in duration of events between 2018 and 2017 (# of days)")

deltas%>%
  filter(Site == "LSA" & Event == "NL")%>%
  ggplot(aes(Species, delta_duration))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  coord_flip()+
  facet_grid(~Treatment)+
  labs(y = "Change in duration of events between 2018 and 2017 (# of days)")

deltas%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, delta_duration, fill = Event))+
  geom_bar(stat = "identity", position = "dodge", width = .8)+
  coord_flip()+
  facet_grid(~Treatment)+
  labs(y = "Change in duration of events between 2018 and 2017 (# of days)")
