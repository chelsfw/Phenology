rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Creating files for duration of pheno events and change in duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Watershed Function SFA/pheno/Phenology.data.cleanup.R")
# path for Dana
source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(FBB, FLB, X))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) #playing around with difference between & and | to filter. if '&' it cuts the data down to only observations that have values for ALL phenophases that year.

pheno$Site <- factor(pheno$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))

#reorder dataset so that NL, FLE, FOF, and FLCC are in colum 'Event' and the values are in column 'DOY'
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#omit NA values after pivot longer to avoid -Inf and Inf values later down the line
pheno <- na.omit(pheno)

# set the levels of the sites. 
pheno$Event<- factor(pheno$Event, levels = c("NL", "FLE", "FOF","FLCC"))

####Find shared species####
pheno_species_shared <- pheno %>% 
  distinct(Year, Site, Treatment, Species)

pheno_species_shared$presence = 1

pheno_species_shared <- pheno_species_shared%>%
  group_by(Site, Species)%>%
  summarise(count = sum(presence))

#filter species down to those that have a presence of four for being shared between 2017 trmt and ctrl (2) and 2018 trmt and ctrl (2) = (4)
pheno_species_shared <- pheno_species_shared%>%
  filter(count == 4)

#make a filter (list) for species shared across years and treatments at each site
shared_LM <-  filter(pheno_species_shared, Site == "LM")
shared_LM <- unique(shared_LM$Species)

shared_UM <-  filter(pheno_species_shared, Site == "UM")
shared_UM <- unique(shared_UM$Species)

shared_LSA <-  filter(pheno_species_shared, Site == "LSA")
shared_LSA <- unique(shared_LSA$Species)

shared_USA <-  filter(pheno_species_shared, Site == "USA")
shared_USA <- unique(shared_USA$Species)

#shared_ALP <-  filter(pheno_species_shared, Site == "ALP")
#shared_ALP <- unique(shared_ALP$Species)

####Filter pheno by shared species for each site using the site specific lists####
pheno_lm <- filter(pheno, Site == "LM")
pheno_lm <- subset(pheno_lm, Species %in% shared_LM)

pheno_um <- filter(pheno, Site == "UM")
pheno_um <- subset(pheno_um, Species %in% shared_UM)

pheno_lsa <- filter(pheno, Site == "LSA")
pheno_lsa <- subset(pheno_lsa, Species %in% shared_LSA)

pheno_usa <- filter(pheno, Site == "USA")
pheno_usa <- subset(pheno_usa, Species %in% shared_USA)

#pheno_alp <- filter(pheno, Site == "ALP")
#pheno_alp <- subset(pheno, Species %in% shared_ALP)

####RBind site specific pheno data frames####
pheno <- rbind(pheno_lm, pheno_um, pheno_lsa, pheno_usa)

#Remove extra dataframes from workspace
rm(pheno_lm, pheno_um, pheno_lsa, pheno_usa, pheno_species_shared)

####Calculate durations and deltas####
##create durations dataset
pheno_durations <- pheno %>% 
  group_by(Year, Site, Treatment, Species, Event) %>% #added Block to get sd, n, se
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T),
            mean = mean(DOY, na.rm = T), #added 8/20 8:50am
            #sd = sd(DOY, na.rm = T), #added 8/20 8:50am
            n = n()) #added 8/20 8:50am
            #se = sd/sqrt(n)) #added 8/20 8:50am

#calculate durations
pheno_durations$species_duration <- pheno_durations$max - pheno_durations$min

pheno_treatment_effects <- pheno_durations %>%
  select(-c(max, min, n, species_duration))%>%
  pivot_wider(names_from = Treatment, values_from = mean)

pheno_treatment_effects$Effect <- pheno_treatment_effects$Treatment - pheno_treatment_effects$Control

##reorder dataframe to create deltas dataset
pheno_deltas <- pheno_durations %>%
  select(-c(max, min, n, mean))%>%
  pivot_wider(names_from = Year, values_from = species_duration)

#calculate deltas
pheno_deltas$delta_duration <- pheno_deltas$`2018`-pheno_deltas$`2017`

pheno_deltas <- pheno_deltas%>%
  select(-c(`2017`, `2018`))

####Write durations and deltas datasets to .csv's####
write.csv(pheno_deltas, "/Users/chelseawilmer/Desktop/Github/Phenology/deltas.csv")
write.csv(pheno_durations, "/Users/chelseawilmer/Desktop/Github/Phenology/durations.csv")
write.csv(pheno_treatment_effects, 'treatment_effects.csv')



