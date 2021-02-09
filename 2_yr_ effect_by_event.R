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

#make year dataset
year <- filter(pheno, Treatment == "Control")

#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means
year.stats <- year%>%
  group_by(Year, Site, Block, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year <- year.stats%>%
  group_by(Year, Site, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

#reorder
year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c('2018', '2017'))

year <- na.omit(year)

####NL####
#filter by observations where NL was present and remove extra event columns (for now)
NL <- filter(year, Event == "NL")

lm <- NL%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-45,1))+
  labs(title = "LM")

um <- NL%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-45,1))+
  labs(title = "UM")

lsa <- NL%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-45,1))+
  labs(title = "LSA")

usa <- NL%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-45,1))+
  labs(title = "USA")

alp <- NL%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-45,1))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####FLE####
#filter by observations where NL was present and remove extra event columns (for now)
FLE <- filter(year, Event == "FLE")

lm <- FLE%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-60,3))+
  labs(title = "LM")

um <- FLE%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-60,3))+
  labs(title = "UM")

lsa <- FLE%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-60,3))+
  labs(title = "LSA")

usa <- FLE%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-60,3))+
  labs(title = "USA")

alp <- FLE%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-60,3))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####FOF####
#filter by observations where NL was present and remove extra event columns (for now)
FOF <- filter(year, Event == "FOF")

lm <- FOF%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-70,20))+
  labs(title = "LM")

um <- FOF%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-70,20))+
  labs(title = "UM")

lsa <- FOF%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-70,20))+
  labs(title = "LSA")

usa <- FOF%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-70,20))+
  labs(title = "USA")

alp <- FOF%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-70,20))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####FLCC####
#filter by observations where NL was present and remove extra event columns (for now)
FLCC <- filter(year, Event == "FLCC")

lm <- FLCC%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-40,55))+
  labs(title = "LM")

um <- FLCC%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-40,55))+
  labs(title = "UM")

lsa <- FLCC%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-40,55))+
  labs(title = "LSA")

usa <- FLCC%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-40,55))+
  labs(title = "USA")

alp <- FLCC%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, yr_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-40,55))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)
