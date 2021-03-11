rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(tidyverse)

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
  group_by(Year, Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

#reorder to take difference in mean DOY between 2018 and 2017
year <- year%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c('2018', '2017'))

year <- na.omit(year)

#average over block for graphing
year_graph <- year%>%
  group_by(Site, Species, Event)%>%
  summarise(mean_days_change_yr = mean(yr_effect))

#remove boechera outlier
year_graph <- year_graph[-c(17),]

#set levels for site and phenophase
year_graph$Site <- factor(year_graph$Site, c("LM", "UM", "LSA", "USA", "ALP"))
year_graph$Event <- factor(year_graph$Event, c("NL", "FLE", "FOF", "FLCC"))

#boxplots for each site per phenophase
ggplot(year_graph, aes(Event, mean_days_change_yr, color = Site))+
  geom_boxplot()+
  scale_y_continuous(limits = c(-60, 60))+
  labs(title = "Year Effect on Number of Days Change in Timing", y = "# of days change")


####TRMT####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

#reorder dataset to take difference between treatment and control plots
treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = meanDOY)

treatment$trmt_effect <- treatment$Early - treatment$Control

treatment <- select(treatment, -c(Early, Control))

treatment <- na.omit(treatment)

#average over block for graphing
treatment_graph <- treatment%>%
  group_by(Site, Species, Event)%>%
  summarise(mean_days_change_trmt = mean(trmt_effect))

#set levels for site and phenophase
treatment_graph$Site <- factor(treatment_graph$Site, c("LM", "UM", "LSA", "USA", "ALP"))
treatment_graph$Event <- factor(treatment_graph$Event, c("NL", "FLE", "FOF", "FLCC"))

#boxplots for each site per phenophase
ggplot(treatment_graph, aes(Event, mean_days_change_trmt, color = Site))+
  geom_boxplot()+
  scale_y_continuous(limits = c(-60, 60))+
  labs(title = "Treatment Effect on Number of Days Change in Timing", y = "# of days change")

####Combined###
#combine year_graph and trmt_graph to be able to see graph as yr_effect next to trmt_effect
yr_and_trmt <- left_join(year_graph, treatment_graph, by = c("Site", "Species", "Event"))

yr_and_trmt <- yr_and_trmt%>%
  pivot_longer(cols = c("mean_days_change_yr", "mean_days_change_trmt"),
                names_to = "Effect", values_to = "Mean_Days_Change")

ggplot(yr_and_trmt, aes(Site, Mean_Days_Change, color = Effect))+
  geom_boxplot()+
  scale_y_continuous(limits = c(-60, 60))+
  facet_grid(Event~.)+
  labs(title = "Mean number of days difference in timing", y = "# of days change")

####YEAR STATS####
pp = unique(year$Event)
pp
site = unique(year$Site)
site

for (p in pp) {
  for (s in site) {
    print(p)
    print(s)
    t = filter(year, Event == p & Site == s)
    stats = t.test(t$yr_effect, mu = 0)
    print(stats)
  }
}

####TRMT STATS####
pp_trmt = unique(treatment$Event)
pp_trmt
site_trmt = unique(treatment$Site)
site_trmt

for (p in pp_trmt) {
  for (s in site_trmt) {
    print(p)
    print(s)
    t = filter(treatment, Event == p & Site == s)
    stats = t.test(t$trmt_effect, mu = 0)
    print(stats)
  }
}
