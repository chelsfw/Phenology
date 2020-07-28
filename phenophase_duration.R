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

#### Community phenophase duration ####
# calculate community phenophases (ex. max date of nl - min date of nl)
# reorder data so that the calculated phenophases are in a column called "Event" and the value is the "DOY" that the event happened
pheno1 <- gather(data = pheno, key = "Event", value = "DOY", 9:12) %>% 
  group_by(Event) #key

# calculate max and min dates of each event
pheno1 <- pheno1 %>%
  group_by(Year, Site, Treatment, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))

# community duration is the difference between the first date that a phenophase happened and the last
pheno1$community_duration <- pheno1$max - pheno1$min

# clean up dataset so that you can spread the data for the difference in duration between 2017 and 2018. I don't know why, but when I don't do this step the spread gets messed up.
pheno1 <- select(pheno1, -c(max, min))

# spread the data 
pheno2 <- pheno1 %>% 
  mutate(i = row_number()) %>% 
  spread(Year, community_duration)

# subtract 2017 event durations from 2018 event durations to get the change in duration from 2017 to 2018. (+) values mean that the duration of the event was LONGER in 2018, (-) values mean the event durations were SHORTER for 2018. 0 values mean no change
pheno2$delta_duration <- pheno2$`2018`-pheno2$`2017`

# ordering sites and phenophases for graphing
pheno2$Site <- factor(pheno2$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
pheno2$Event<- factor(pheno2$Event, levels = c("NL", "FLE", "FOF","FLCC"))

# graphing the change in duration from 2017 to 2018
ggplot(pheno2, aes(Event, delta_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~.)+
  labs(y="Change in duration (2018 - 2017)")

# ordering sites and phenophases for graphing
pheno1$Site <- factor(pheno1$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
pheno1$Event<- factor(pheno1$Event, levels = c("NL", "FLE", "FOF","FLCC"))

# graphing the number of days that it took for the community to complete phenophases
# would like to look at how the community phenophases overlap
ggplot(pheno1, aes(Event, community_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~Year)+
  labs(y = "# days for all species to reach phenophase")

#### Phase-based durations ####
# Calculate key events as phenophase 2 - phenophase 1 (ex. fle - nl)

# re-run lines 1-12, then start running these lines
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

# removing uncessary/unsed columns
duration <- select(duration, -c(DOYsf, NL, FLE, FOF, FLCC))

# calculate mean duration and summary stats
duration <- duration %>%
  group_by(Year, Site, Treatment, Species, Period) %>%
  summarise(mean_duration = mean(Days, na.rm = T),
            min = min(Days, na.rm = T),
            max = max(Days, na.rm = T),
            sd = sd(Days, na.rm = T))

# average over species
duration <- duration %>%
  group_by(Year, Site, Treatment, Period) %>%
  summarise(mean_duration = mean(mean_duration, na.rm = T))

# spread data so that we can take the difference between 2017 and 2018
duration <- duration %>% 
  mutate(i = row_number()) %>% 
  spread(Year, mean_duration)

# calculate difference in duration between 2017 and 2018. (+) values mean that the duration of the event was LONGER in 2018, (-) values mean the event durations were SHORTER for 2018. 0 values mean no change
duration$delta_duration <- duration$`2018`- duration$`2017`

# ordering sites and phenophases for graphing
duration$Site <- factor(duration$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
duration$Period <- factor(duration$Period, levels = c("sf_nl", "sf_fle", "sf_fof","sf_flcc", "nl_fle", "fle_fof", "fle_flcc"))

# graphing the change in duration for the different phases based on the phase1-phase2 calculation
ggplot(duration, aes(Period, delta_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~.)+
  labs(y="Change in duration (2018 - 2017)")

#### Teasing out species and elevation ####
#rerun lines 1 through twelve, then run these lines

# reorder data so that the calculated phenophases are in a column called "Event" and the value is the "DOY" that the event happened
pheno3 <- gather(data = pheno, key = "Event", value = "DOY", 9:12) %>% 
  group_by(Event) #key

# calculate max and min values of the dates of phenophases for species
pheno3 <- pheno3 %>%
  group_by(Year, Site, Treatment, Species, Event) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))

# community duration is the difference between the first date that a phenophase happened and the last
pheno3$species_duration <- pheno3$max - pheno3$min

# remove -Inf's
pheno3 <- filter(pheno3, species_duration >=0)

# reorder so that 2017 and 2018 are columns
pheno3 <- select(pheno3, -c(max, min))
pheno3 <- pheno3 %>% 
  mutate(i = row_number()) %>% 
  spread(Year, species_duration)

pheno3$species_duration <- pheno3$`2018`- pheno3$`2017`
pheno3 <- filter(pheno3, species_duration >=0)

# order sites and phenophases for graphing
pheno3$Site <- factor(pheno3$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))
pheno3$Event<- factor(pheno3$Event, levels = c("NL","FLE","FOF","FLCC"))

# graph durations 
pheno3%>%
  filter(Site == "LM")%>%
  ggplot(aes(species_duration, Species, color = Treatment))+
  geom_point()+
  facet_grid(~Event)+
  labs(x="Change in duration (2018 - 2017)", title = "Lower Montane")

pheno3%>%
  filter(Site == "UM")%>%
  ggplot(aes(species_duration, Species, color = Treatment))+
  geom_point()+
  facet_grid(~Event)+
  labs(x="Change in duration (2018 - 2017)", title = "Upper Montane")

pheno3%>%
  filter(Site == "LSA")%>%
  ggplot(aes(species_duration, Species, color = Treatment))+
  geom_point()+
  facet_grid(~Event)+
  labs(x="Change in duration (2018 - 2017)", title = "Lower Subalpine")

pheno3%>%
  filter(Site == "USA")%>%
  ggplot(aes(species_duration, Species, color = Treatment))+
  geom_point()+
  facet_grid(~Event)+
  labs(x="Change in duration (2018 - 2017)", title = "Upper Subalpine")

pheno3%>%
  filter(Site == "ALP")%>%
  ggplot(aes(species_duration, Species, color = Treatment))+
  geom_point()+
  facet_grid(~Event)+
  labs(x="Change in duration (2018 - 2017)", title = "Alpine")