rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Creating files for duration of pheno events and change in duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter observations to those that have a vlaue for NL, FLE, FOF, and FLCC
#playing around with difference between & and | to filter. if '&' it cuts the data down to only observations that have values for ALL phenophases that year. 
pheno <- filter(pheno, NL >0 | FLE >0 | FOF>0 | FLCC >0) 
#pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) 

pheno$Site <- factor(pheno$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))

#reorder dataset so that NL, FLE, FOF, and FLCC are in column 'Event' and the values are in column 'DOY'
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#omit NA values after pivot longer to avoid -Inf and Inf values later down the line
#pheno <- na.omit(pheno)

# set the levels of the sites. 
pheno$Event<- factor(pheno$Event, levels = c("NL", "FLE", "FOF","FLCC"))

####Find shared species####
#makes a plot specific list of species
pheno_species_shared <- pheno %>% 
  distinct(Year, Site, Treatment, Species)

#create presence column and fill it with 1s
pheno_species_shared$presence = 1

#group down to site and species and sum the presence column. a presence of 4 for species means it was present in both treatments in 2017 (2) and both treatments in 2018 (2) 
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

#Write .csv for pheno <- filter(pheno, NL >0 | FLE >0 | FOF>0 | FLCC >0). You have to comment out pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0) to run
write.csv(pheno, "/Users/chelseawilmer/Desktop/Github/Phenology/Phenology_at_least_one_phenophase_observed.csv")

#Write .csv for pheno <- filter(pheno, NL >0 & FLE >0 & FOF>0 & FLCC >0). You have to comment out pheno <- filter(pheno, NL >0 | FLE >0 | FOF>0 | FLCC >0) to run
write.csv(pheno, "/Users/chelseawilmer/Desktop/Github/Phenology/Phenology_all_phenophases_observed.csv")

