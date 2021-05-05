rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####Durations for YEAR####
year <- year%>%
  group_by(Year, Site, Block, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T),
            EOS= mean(EOS, na.rm = T))

#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
year$NL <- year$FLE - year$NL
year$FLE <- year$FOF - year$FLE
year$FOF <- year$FLCC - year$FOF
year$FLCC <- year$EOS - year$FLCC

#reorganize data
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "duration") 

year <- na.omit(year)
year <- select(year, -c(EOS))
year <- year%>%
  pivot_wider(names_from = Year, values_from = duration)

year <- na.omit(year)

year$yr_effect <- year$`2018` - year$`2017`

year <- select(year, -c("2017", "2018"))

#set levels for site and phenophase
year_graph <- year%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(yr_effect = mean(yr_effect, na.rm = T))

year_graph$Site <- factor(year_graph$Site, c("LM", "UM", "LSA", "USA", "ALP"))
year_graph$Event <- factor(year_graph$Event, c("NL", "FLE", "FOF", "FLCC"))

#boxplots for each site per phenophase
ggplot(year_graph, aes(Event, yr_effect, color = Site))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(-60, 60))+
  labs(title = "Year Effect on Number of Days Change in Duration", y = "# of days change")

####Durations for TRMT####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T),
            EOS= mean(EOS, na.rm = T))

#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
treatment$NL <- treatment$FLE - treatment$NL
treatment$FLE <- treatment$FOF - treatment$FLE
treatment$FOF <- treatment$FLCC - treatment$FOF
treatment$FLCC <- treatment$EOS - treatment$FLCC

treatment <- select(treatment, -c(EOS))
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "duration") 

#reorder dataset to get events back into columns
treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = duration)

treatment$trmt_effect <- treatment$Early - treatment$Control

treatment <- select(treatment, -c(Early, Control))

treatment <- na.omit(treatment)

#set levels for site and phenophase
treatment_graph <- treatment%>%
  group_by(Site, Species, Event)%>%
  summarise(trmt_effect = mean(trmt_effect, na.rm = T))

treatment_graph$Site <- factor(treatment_graph$Site, c("LM", "UM", "LSA", "USA", "ALP"))
treatment_graph$Event <- factor(treatment_graph$Event, c("NL", "FLE", "FOF", "FLCC"))

#boxplots for each site per phenophase
ggplot(treatment_graph, aes(Event, trmt_effect, color = Site))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(-60, 60))+
  labs(title = "Treatment Effect on Number of Days Change in Duration", y = "# of days change")

####YEAR STATS####
col_name_output <- c("Site", "Phenophase", "statistic", "p.value")
all_results_yr <- data.frame(matrix(vector(), 0, length(col_name_output), dimnames = list(c(), col_name_output)), stringsAsFactors = T)

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
    print(all_results_yr)
    x = c(s, p, round(stats$statistic, 3), round(stats$p.value, 3))
    names(x) = col_name_output
    all_results_yr = rbind(all_results_yr, x)
  }
}

colnames(all_results_yr) = col_name_output

####TRMT STATS####
col_name_output_trmt <- c("Site", "Phenophase", "statistic", "p.value")
all_results_trmt <- data.frame(matrix(vector(), 0, length(col_name_output), dimnames = list(c(), col_name_output)), stringsAsFactors = T)

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
    print(all_results_trmt)
    x = c(s, p, round(stats$statistic, 3), round(stats$p.value, 3))
    names(x) = col_name_output_trmt
    all_results_trmt = rbind(all_results_trmt, x)
  }
}

colnames(all_results_trmt) = col_name_output_trmt
