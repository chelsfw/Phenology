rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in phenology data####
#pheno <- read.csv("PhenologyData_complete.csv")
source("Phenology.data.cleanup.R")

#write_csv(pheno, "/Users/chelseawilmer/Desktop/CSU/Thesis/phenology_complete.csv")
pheno <- select(pheno, -c(Functional.Type, FBB, FLB))
pheno$Site <- factor(pheno$Site, levels = c("ALP", "USA", "LSA","UM", "LM"))

#### Community phenophase duration ####
# calculate community phenophases (ex. max date of nl - min date of nl)
# reorder data so that the calculated phenophases are in a column called "Event" and the value is the "DOY" that the event happened

# I grouped these actions together here 
pheno <- pheno %>% 
  # pivot longer lets you designate columns by name so that if the order changes it's ok
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
                        names_to = "Event", values_to = "DOY") 

# setting event names to be factors
pheno$Event<- factor(pheno$Event, levels = c("NL", "FLE", "FOF","FLCC"))

###########
# Plot level delta calculations 
###########
pheno_plot <- pheno %>% 
  group_by(Block, Plot, Treatment, Event, Year, Site) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))
pheno_plot$plot_duration <- pheno_plot$max - pheno_plot$min

# spread equivalent here 
pheno_plot_delta <- pheno_plot %>% 
  select(-c(max, min)) %>% 
  pivot_wider(names_from = Year, values_from = plot_duration)

#Calculating year deltas
pheno_plot_delta$delta_duration <- pheno_plot_delta$`2018`-pheno_plot_delta$`2017`

###########
# Community level delta calculations 
###########
pheno_community <- pheno %>% 
  group_by(Treatment, Event, Year, Site) %>%
  summarise(max = max(DOY, na.rm = T),
            min = min(DOY, na.rm = T))

# calculating duration for each community
pheno_community$community_duration <- pheno_community$max - pheno_community$min

# spread equivalent here 
pheno_community_delta <- pheno_community %>% 
  select(-c(max, min)) %>% 
  pivot_wider(names_from = Year, values_from = community_duration)

pheno_community_delta$delta_duration <- pheno_community_delta$`2018`-pheno_community_delta$`2017`


###########
# Figure generation for deltas
###########


#Plotting this as box plots instead of community level points 
ggplot(pheno_plot_delta, aes(Event, delta_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Site~.)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Site ~., nrow=2, ncol=3) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12), 
        # put the legend into the extra facet
        legend.position = c(1, 0),
        legend.justification = c(1, 0))+
  labs(y="Change in duration (2018 - 2017)")
ggsave("fig_plot_delta.png", width = 6.5, height = 3.5)



ggplot(pheno_community_delta, aes(Event, delta_duration, color = Treatment))+
  geom_point()+
  facet_grid(Site~.)+
  labs(y="Change in duration (2018 - 2017)")
ggsave("fig_community_delta.png", width = 6.5, height = 3.5)





###########
# This is where I got to so far... 
# I now understand aspects of this coding style better and 
# should be somewhat better equipped to help with specific questions going forward 
###########
