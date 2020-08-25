rm(list = ls())
## Date: 2020-08-19
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Exploratory analysis for duration of pheno events
## Packages:
library(tidyverse)

####Bring in duration data####
setwd("/Users/chelseawilmer/Desktop/Github/Phenology")
durations <- read.csv("/Users/chelseawilmer/Desktop/Github/Phenology/durations.csv")

durations <- durations%>%
  select(-c(X))

durations$Event <- factor(durations$Event, levels = c("FLCC", "FOF", "FLE", "NL"))

####Site specific durations of events for each species####
durations%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, mean_duration, fill = Event))+
    geom_bar(stat = "identity", position = "stack")+ facet_grid(Year~Treatment)+ labs(x = "Species", y = "Mean Duration of Event (days)", title = "Lower Montane")+ theme(axis.text.x = element_text(angle = 90))

durations%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, mean_duration, fill = Event))+
  geom_bar(stat = "identity", position = "stack")+ facet_grid(Year~Treatment)+ labs(x = "Species", y = "Mean Duration of Event (days)", title = "Upper Montane")+ theme(axis.text.x = element_text(angle = 90))

durations%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, mean_duration, fill = Event))+
  geom_bar(stat = "identity", position = "stack")+ facet_grid(Year~Treatment)+ labs(x = "Species", y = "Mean Duration of Event (days)", title = "Lower Subalpine")+ theme(axis.text.x = element_text(angle = 90))  

durations%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, mean_duration, fill = Event))+
  geom_bar(stat = "identity", position = "stack")+ facet_grid(Year~Treatment)+ labs(x = "Species", y = "Mean Duration of Event (days)", title = "Upper Subalpine")+ theme(axis.text.x = element_text(angle = 90))

####Average duration of event at each site####
site_durations <- durations%>%
  group_by(Year, Site, Treatment, Event)%>%
  summarise(mean_duration = mean(mean_duration))

site_durations$Site <- factor(site_durations$Site, levels = c("USA", "LSA", "UM","LM"))
site_durations$Event <- factor(site_durations$Event, levels = c("NL", "FLE", "FOF","FLCC"))
ggplot(site_durations, aes(Event, mean_duration, fill = Treatment))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(Site~Year)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "Mean Duration of Event (days)")

####Using Dana's cool boxplot code####
#for site
durations$Site <- factor(durations$Site, levels = c("LM", "UM", "LSA","USA"))
durations$Event <- factor(durations$Event, levels = c("NL", "FLE", "FOF","FLCC"))
ggplot(durations, aes(Event, mean_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Site~Year)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Site~Year, nrow=2, ncol=4) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12))+
  labs(y="Duration of event (days)")

ggsave("fig_plot_delta.png", width = 6.5, height = 3.5)

#for species
durations%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, mean_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Year~.)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Year~., nrow=2, ncol=3) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12), 
        # put the legend into the extra facet
        axis.text.x = element_text(angle = 90))+
  labs(y="Duration of event (days)")

durations%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, mean_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Year~.)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Year~., nrow=2, ncol=3) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12), 
        # put the legend into the extra facet
        axis.text.x = element_text(angle = 90))+
  labs(y="Duration of event (days)")
        
durations%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, mean_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Year~.)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Year~., nrow=2, ncol=3) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12), 
        # put the legend into the extra facet
        axis.text.x = element_text(angle = 90))+
  labs(y="Duration of event (days)")

durations%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, mean_duration, color = Treatment))+
  geom_abline(intercept = 0, slope = 0, colour = 'gray')+
  geom_boxplot()+
  facet_grid(Year~.)+
  # wrapping into different columns/rows of subsets
  facet_wrap(Year ~., nrow=2, ncol=3) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        #this is where I change the gray boxes to white and change the size of text
        strip.background =element_rect(color = 'white', fill='white'), 
        strip.text = element_text(size=12), 
        # put the legend into the extra facet
        axis.text.x = element_text(angle = 90))+
  labs(y="Duration of event (days)")
