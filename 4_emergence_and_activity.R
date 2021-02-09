rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating the duration of emergence and activity
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

####Activity####
activity <- filter(pheno, FLE >0 & FLCC >0)
activity <- select(activity, -c(NL, FOF))

#SF and activity period
activity$activity <- activity$FLCC-activity$FLE
activity <- select(activity, -c(FLE, FLCC))

#calculate means for activity period
activity <- activity%>%
  group_by(Year, Site, Block, Treatment, Species)%>%
  summarise(mean_activity = mean(activity, na.rm = T))

activity <- activity%>%
  group_by(Year, Site, Treatment, Species)%>%
  summarise(mean_activity = mean(mean_activity, na.rm = T))

# reorder data to calculate year effect
year <- activity%>%
  pivot_wider(names_from = Year, values_from = mean_activity)

year <- na.omit(year)

#calculate effect of the year on the activity duration
year$year_effect <- year$`2018` - year$`2017`

year <- filter(year, Treatment == "Control")

#what does this shit look like
lm <- year%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,90))+
  labs(title = "LM")

um <- year%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,90))+
  labs(title = "UM")

lsa <- year%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,90))+
  labs(title = "LSA")

usa <- year%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,90))+
  labs(title = "USA")

alp <- year%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,90))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

#treatment effect
treatment <- filter(activity, Year == 2018)

#reorder data to calculate effect of the year
treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = mean_activity)

treatment <- na.omit(treatment)

treatment$trmt_effect <- treatment$Treatment - treatment$Control

#what does THIS shit look like
lm <- treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LM")

um <- treatment%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "UM")

lsa <- treatment%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LSA")

usa <- treatment%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####Emergence####
emergence <- filter(pheno, FLE >0 & NL >0)
emergence <- select(emergence, -c(FOF, FLCC))

emergence$emergence <- emergence$FLE - emergence$NL

#calculate means for emergence
emergence <- emergence%>%
  group_by(Year, Site, Block, Treatment, Species)%>%
  summarise(mean_emergence = mean(emergence, na.rm = T))

emergence <- emergence%>%
  group_by(Year, Site, Treatment, Species)%>%
  summarise(mean_emergence = mean(mean_emergence, na.rm = T))

#calculate effect of year on emergence
year <- emergence%>%
  pivot_wider(names_from = Year, values_from = mean_emergence)

year <- na.omit(year)

year$year_effect <- year$`2018` - year$`2017`

year <- filter(year, Treatment == "Control")

#what does this shit look like
lm <- year%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LM")

um <- year%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "UM")

lsa <- year%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LSA")

usa <- year%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "USA")

alp <- year%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, year_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

#calculate effect of treatment on emergence
treatment <- filter(emergence, Year == 2018)

treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = mean_emergence)

treatment <- na.omit(treatment)

treatment$trmt_effect <- treatment$Treatment - treatment$Control

#what does this shit look like
lm <- treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LM")

um <- treatment%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "UM")

lsa <- treatment%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LSA")

usa <- treatment%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, trmt_effect))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)
