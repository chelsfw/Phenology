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
pheno <- dplyr::select(pheno, -c(FBB, FLB))
pheno$Elevation <- 0
pheno$Elevation <- ifelse(pheno$Site == "LM", 2774, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "UM", 2957, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "LSA", 3169, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "USA", 3475, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "ALP", 3597, pheno$Elevation)

#make some kind of summary figure for difference in snowmelt date between treatments at all sites
snowmelt <- pheno%>%
  group_by(Year, Site, Elevation, Block, Treatment)%>%
  summarise(DOYsf = mean(DOYsf, na.rm = T))

#average over block for graphing
snowmelt_graph <- snowmelt%>%
  group_by(Year, Site, Elevation, Treatment)%>%
  summarise(DOYsf = mean(DOYsf, na.rm = T))

snowmelt_graph$Site <- factor(snowmelt_graph$Site, c("LM", "UM", "LSA", "USA", "ALP"))
ggplot(snowmelt_graph, aes(DOYsf, Elevation, color = Treatment))+
  geom_point()+
  facet_grid(Year~.)+
  theme_bw()+
  labs(x = "Day of Year", y = "Elevation (m)")

#make some kind of sumnary figure for the growing season at each site
#average over subplots
growing_season <- pheno%>%
  group_by(Year, Site, Elevation, Block, Treatment)%>%
  summarise(NL = mean(NL, na.rm = T),
            FLCC = mean(FLCC, na.rm = T))

#average over block for graphing
growing_season <- pheno%>%
  group_by(Year, Site, Elevation, Treatment)%>%
  summarise(NL = mean(NL, na.rm = T),
            FLCC = mean(FLCC, na.rm = T))

#calculate start and end
growing_season <- pheno%>%
  group_by(Year, Site, Elevation, Treatment)%>%
  summarise(start = min(NL, na.rm = T),
            end = max(FLCC, na.rm = T))

#growing_season$duration <- growing_season$end - growing_season$start

#reorder
growing_season <- growing_season%>%
  pivot_longer(cols = c("start", "end"),
               names_to = "range", values_to = "DOY")

#set levels
growing_season$Site <- factor(growing_season$Site, c("LM", "UM", "LSA", "USA", "ALP"))

#graphing
ggplot(growing_season, aes(DOY, Site, color = Treatment))+
  geom_point()+
  geom_line()+
  facet_grid(Year~.)+
  labs(title = "Growing Season (minimum mean FLCC - minimum mean FLE)")

#make some kind of dataframe and figure that looks at the number of days from snowmelt to the start of each phenophase
days_since_snowmelt <- pheno%>%
  group_by(Year, Site, Elevation, Block, Plot, Duplicate.Plot, Subplot, Treatment, Species, DOYsf)%>%
  summarise(NL = mean(NL, na.rm = T),
            FLE = mean(FLE, na.rm = T),
            FOF = mean(FOF, na.rm = T),
            FLCC = mean(FLCC, na.rm = T))

days_since_snowmelt$nl <- days_since_snowmelt$NL - days_since_snowmelt$DOYsf
days_since_snowmelt$fle <- days_since_snowmelt$FLE - days_since_snowmelt$DOYsf
days_since_snowmelt$fof <- days_since_snowmelt$FOF - days_since_snowmelt$DOYsf
days_since_snowmelt$flcc <- days_since_snowmelt$FLCC - days_since_snowmelt$DOYsf

days_since_snowmelt <- dplyr::select(days_since_snowmelt, -c("DOYsf", "NL", "FLE", "FOF", "FLCC"))
days_since_snowmelt <- na.omit(days_since_snowmelt)

days_since_snowmelt <- days_since_snowmelt%>%
  pivot_longer(cols = c("nl", "fle", "fof", "flcc"),
               names_to = "Event", values_to = "Days")

days_since_snowmelt$Site <- factor(days_since_snowmelt$Site, c("LM", "UM", "LSA", "USA", "ALP"))

####Days since snowmelt for YEAR####
days_since_snowmelt_yr <- filter(days_since_snowmelt, Treatment == "Control")
days_since_snowmelt_yr <- days_since_snowmelt_yr%>%
  pivot_wider(names_from = Event, values_from = Days)

#average over subplots
days_since_snowmelt_yr <- days_since_snowmelt_yr%>%
  group_by(Year, Site, Elevation, Block, Treatment, Species)%>%
  summarise(NL = mean(nl, na.rm = T),
            FLE = mean(fle, na.rm = T),
            FOF = mean(fof, na.rm = T),
            FLCC = mean(flcc, na.rm = T))

days_since_snowmelt_yr <- days_since_snowmelt_yr%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Days")

days_since_snowmelt_yr <- days_since_snowmelt_yr%>%
  pivot_wider(names_from = Year, values_from = Days)

days_since_snowmelt_yr <- na.omit(days_since_snowmelt_yr)

####YEAR STATS####
col_name_output <- c("Site", "Phenophase", "statistic", "p.value")
all_results_yr <- data.frame(matrix(vector(), 0, length(col_name_output), dimnames = list(c(), col_name_output)), stringsAsFactors = T)

pp = unique(days_since_snowmelt_yr$Event)
pp
site = unique(days_since_snowmelt_yr$Site)
site

for (p in pp) {
  for (s in site) {
    print(p)
    print(s)
    t = filter(days_since_snowmelt_yr, Event == p & Site == s)
    stats = t.test(t$`2018`, t$`2017`, vars.equal = F)
    print(all_results_yr)
    x = c(s, p, round(stats$statistic, 3), round(stats$p.value, 3))
    names(x) = col_name_output
    all_results_yr = rbind(all_results_yr, x)
  }
}

colnames(all_results_yr) = col_name_output

####Days since snowmelt for YEAR####
days_since_snowmelt_trmt <- filter(days_since_snowmelt, Year == 2018)
days_since_snowmelt_trmt <- filter(days_since_snowmelt_trmt, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

days_since_snowmelt_trmt <- days_since_snowmelt_trmt%>%
  pivot_wider(names_from = Event, values_from = Days)

days_since_snowmelt_trmt <- days_since_snowmelt_trmt%>%
  group_by(Year, Site, Elevation, Block, Treatment, Species)%>%
  summarise(NL = mean(nl, na.rm = T),
            FLE = mean(fle, na.rm = T),
            FOF = mean(fof, na.rm = T),
            FLCC = mean(flcc, na.rm = T))

days_since_snowmelt_trmt <- days_since_snowmelt_trmt%>%
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "Days")

days_since_snowmelt_trmt <- days_since_snowmelt_trmt%>%
  pivot_wider(names_from = Treatment, values_from = Days)

days_since_snowmelt_trmt <- na.omit(days_since_snowmelt_trmt)

####TRMT STATS####
col_name_output_trmt <- c("Site", "Phenophase", "statistic", "p.value")
all_results_trmt <- data.frame(matrix(vector(), 0, length(col_name_output), dimnames = list(c(), col_name_output)), stringsAsFactors = T)

pp = unique(days_since_snowmelt_trmt$Event)
pp
site = unique(days_since_snowmelt_trmt$Site)
site

for (p in pp) {
  for (s in site) {
    print(p)
    print(s)
    t = filter(days_since_snowmelt_trmt, Event == p & Site == s)
    stats = t.test(t$Early, t$Control, vars.equal = F)
    print(all_results_trmt)
    x = c(s, p, round(stats$statistic, 3), round(stats$p.value, 3))
    names(x) = col_name_output_trmt
    all_results_trmt = rbind(all_results_trmt, x)
  }
}

colnames(all_results_trmt) = col_name_output_trmt
