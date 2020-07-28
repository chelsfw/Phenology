rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno/Phenology.data.cleanup.R")
#write_csv(pheno, "/Users/chelseawilmer/Desktop/CSU/Thesis/phenology_complete.csv")
pheno <- select(pheno, -c(Duplicate.Plot, Functional.Type, FBB, FLB))

# calculate community phenophase (ex. max date of nl - min date of nl)

# reorder pheno data
pheno1 <- gather(data = pheno, key = "Event", value = "DOY", 9:12) %>% 
  group_by(Event) #key

pheno1 <- pheno1 %>%
  group_by(Year, Site, Treatment, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))

pheno1$community_duration <- pheno1$max - pheno1$min
pheno1 <- select(pheno1, -c(max, min))


pheno2 <- pheno1 %>% 
  mutate(i = row_number()) %>% 
  spread(Year, community_duration)

pheno2$delta_duration <- pheno2$`2018`-pheno2$`2017`

pheno2$Site <- factor(pheno2$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
pheno2$Event<- factor(pheno2$Event, levels = c("NL", "FLE", "FOF","FLCC"))
ggplot(pheno2, aes(Event, delta_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~.)+
  labs(y="Change in duration (2018 - 2017)")



pheno1$Site <- factor(pheno1$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
pheno1$Event<- factor(pheno1$Event, levels = c("NL", "FLE", "FOF","FLCC"))
ggplot(pheno1, aes(Event, community_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~Year)+
  labs(y = "# days for all species to reach phenophase")

#### Calculate key events as phenophase 2 - phenophase 1 (ex. fle - nl) ####
pheno$sf_nl <- pheno$NL-pheno$DOYsf # initiation of leaves
pheno$sf_fle <- pheno$FLE-pheno$DOYsf # onset of growth
pheno$sf_fof <- pheno$FOF-pheno$DOYsf # onset of fowering
pheno$sf_flcc <- pheno$FLCC-pheno$DOYsf 

pheno$nl_fle <- pheno$FLE-pheno$NL # initiation of growth
pheno$fle_fof <- pheno$FOF-pheno$FLE
pheno$fle_flcc <- pheno$FLCC-pheno$FLE # total activity 

# reorder to make duration dataset
duration <- gather(data = pheno, key = "Period", value = "Days", 13:19) %>% 
  group_by(Period) #key

# removing uncessary columns
duration <- select(duration, -c(DOYsf, NL, FLE, FOF, FLCC))

# calculate mean duration and summary stats
duration <- duration %>%
  group_by(Year, Site, Treatment, Species, Period) %>%
  summarise(mean_duration = mean(Days, na.rm = T),
            min = min(Days, na.rm = T),
            max = max(Days, na.rm = T),
            sd = sd(Days, na.rm = T))

event_means <- duration %>%
  group_by(Year, Site, Treatment, Period) %>%
  summarise(mean_duration = mean(mean_duration, na.rm = T))
event_means <- event_means %>% 
  mutate(i = row_number()) %>% 
  spread(Year, mean_duration)
event_means$delta_duration <- event_means$`2018`- event_means$`2017`


event_means$Site <- factor(event_means$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
event_means$Period <- factor(event_means$Period, levels = c("sf_nl", "sf_fle", "sf_fof","sf_flcc", "nl_fle", "fle_fof", "fle_flcc"))
ggplot(event_means, aes(Period, delta_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~.)+
  labs(y="Change in duration (2018 - 2017)")


duration <- duration %>% 
  mutate(i = row_number()) %>% 
  spread(Year, mean_duration)