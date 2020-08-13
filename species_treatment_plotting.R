rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

#### Bring in phenology data####
source("Phenology.data.cleanup.R")

#### Remove unused data and convert sites to factors ####
pheno <- select(pheno, -c(Functional.Type, FBB, FLB))
pheno$Site <- factor(pheno$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))


## Determine if species has data recorded for it 
## by summing across observation rows
pheno %>% 
  select(NL, FLE, FOF, FLCC) %>% 
  rowSums(na.rm=TRUE) -> pheno$presence 

# Convert non-zero values to 1
pheno_presence <- pheno %>% mutate(presence = replace(presence, presence > 0, 1))

# remove DOY observation information
pheno_presence<-select(pheno_presence, -c(DOYsf, NL, FLE, FOF, FLCC))

#Count the number of species observed for year/site/treatment
pheno_species <- pheno_presence %>% 
  group_by(Treatment, Year, Site, Species) %>%
  summarise(count = sum(presence, na.rm = T))

# filter for year (would be nice to have this in same graph somehow, but not sure how best to do that...)
pheno_species_2017 <- pheno_species %>% filter(Year == 2017)
pheno_species_2018 <- pheno_species %>% filter(Year == 2018)

##########
# generating presence/absence plot by species
##########
tile_width = 0.75
tile_height = 0.75

ggplot()+
  geom_tile(data = pheno_species_2017, aes(x = Species, y = Treatment, fill = count, width=tile_width, height=tile_height))+
  geom_tile(data = pheno_species_2018, fill=NA, size=1, aes(x = Species, y = Treatment, color = count, width=tile_width, height=tile_height))+
  facet_grid(Site ~ ., labeller = label_wrap_gen(width = 10))+
  scale_fill_gradient(low="white", high="forestgreen")+
  scale_color_gradient(low="white", high="black")+
  labs(title = "Species occurance", fill = "2017", colour = "2018")+
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5),
        panel.border = element_rect(colour="black",size=1, fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=8))
ggsave("fig_species.png", width = 13, height = 6)


#########
##### Ideas for species filtering
##########

# What if you replace the count in pheno_species with 1s and then 
# Group by species and summarize by counting again. 
# Then you could filter species for any instances where you have a count of 4

# Just a thought! 


