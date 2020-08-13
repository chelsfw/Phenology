rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
pheno <- read.csv("PhenologyData_complete.csv")

#### Remove unused data and convert sites to factors ####
pheno<-select(pheno, -c(FBB, FLB, SG, NL2, FLE2, FLD, SG, DOYsf))
pheno$Site<- factor(pheno$Site, levels = c("Alpine", "Upper Sub-alpine", "Lower Sub-alpine","Upper Montane", "Lower Montane"))

## Determine if species has data recorded for it 
## by summing across observation rows
pheno %>% 
  select(NL, FLE, FOF, FLCC) %>% 
  rowSums(na.rm=TRUE) -> pheno$presence 

# Convert non-zero values to 1
pheno <- pheno %>% mutate(presence = replace(presence, presence > 0, 1))

# remove DOY observation information
pheno<-select(pheno, -c(NL, FLE, FOF, FLCC))

#Count the number of species observed for year/site/treatment
pheno_species <- pheno %>% 
  group_by(Treatment, Year, Site, Species) %>%
  summarise(count = sum(presence, na.rm = T))

# filter for year (would be nice to have this in same graph somehow, but not sure how best to do that...)
pheno_species_2017 <- pheno_species %>% filter(Year == 2017)
pheno_species_2018 <- pheno_species %>% filter(Year == 2018)

# Plotting as tiles the counts of species in treatment/site combinations
ggplot(pheno_species_2017, aes(x = Species, y = Treatment, fill = count, width=0.7, height=0.7))+
  geom_tile()+
  facet_grid(Site ~ ., labeller = label_wrap_gen(width = 10))+
  scale_fill_gradient(low="white", high="forestgreen")+
  labs(title = "2017")+
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 270, hjust = 0),
        panel.border = element_rect(colour="black",size=1, fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=8))
ggsave("fig_species_2017.png", width = 12, height = 6)



ggplot(pheno_species_2018, aes(x = Species, y = Treatment, fill = count, width=0.7, height=0.7))+
  geom_tile()+
  facet_grid(Site ~ ., labeller = label_wrap_gen(width = 10))+
  scale_fill_gradient(low="white", high="forestgreen")+
  labs(title = "2018")+
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 270, hjust = 0),
        panel.border = element_rect(colour="black",size=1, fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=8))
ggsave("fig_species_2018.png", width = 12, height = 6)


