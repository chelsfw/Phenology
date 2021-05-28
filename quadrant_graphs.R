rm(list = ls())
####Quadrant Species Phenology Graph####
#Making a graph of early/short, early/long, late/short, late/long species responses to the year and treatment effect of early snowmelt

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")
library(ggpubr)

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####YEAR####
year_duration <- year

year_duration$NL <- year_duration$FLE - year_duration$NL
year_duration$FLE <- year_duration$FOF - year_duration$FLE
year_duration$FOF <- year_duration$FLCC - year_duration$FOF
year_duration$FLCC <- year_duration$EOS - year_duration$FLCC

year_duration <- year_duration %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "Duration")

#reorder TIMING dataset to be longer (events are in an 'event' column instead of columns of their own)
year_timing <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 
year_timing <- select(year_timing, -c(Treatment))

#average over subplots
year_timing <- year_timing%>%
  group_by(Year, Site, Block, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year_timing <- year_timing%>%
  group_by(Year, Site, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

year_timing <- year_timing%>%
  group_by(Year, Site, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

#reorder to take difference in mean DOY between 2018 and 2017
year_timing <- year_timing%>%
  pivot_wider(names_from = Year, values_from = meanDOY)

year_timing$yr_effect_timing <- year_timing$`2018` - year_timing$`2017`

year_timing <- select(year_timing, -c('2018', '2017'))

#reorder DURATION dataset to be longer (events are in an 'event' column instead of columns of their own)

year_duration <- select(year_duration, -c(Treatment))

#average over subplots
year_duration <- year_duration%>%
  group_by(Year, Site, Block, Species, Event)%>%
  summarise(meanDuration = mean(Duration, na.rm = T))

year_duration <- year_duration%>%
  group_by(Year, Site, Species, Event)%>%
  summarise(meanDuration = mean(meanDuration, na.rm = T))

year_duration <- year_duration%>%
  group_by(Year, Site, Event)%>%
  summarise(meanDuration = mean(meanDuration, na.rm = T))

#reorder to take difference in mean DOY between 2018 and 2017
year_duration <- year_duration%>%
  pivot_wider(names_from = Year, values_from = meanDuration)

year_duration$yr_effect_duration <- year_duration$`2018` - year_duration$`2017`

year_duration <- select(year_duration, -c('2018', '2017'))

year <- left_join(year_timing, year_duration, by = c("Site", "Event"))

#summarise over block for graphing
year <- year%>%
  group_by(Site, Species, Event)%>%
  summarise(yr_timing = mean(yr_effect_timing, na.rm = T),
            yr_duration = mean(yr_effect_duration, na.rm = T))

year <- na.omit(year)

####Year quadrant graph####
####BY EVENT####
#lower montane
lm <- year%>%
  filter(Site == "LM")%>%
  ggplot(aes(yr_effect_duration, yr_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  theme_bw()+
  labs(title = "Lower Montane", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
lm

um <- year%>%
  filter(Site == "UM")%>%
  ggplot(aes(yr_effect_duration, yr_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  theme_bw()+
  labs(title = "Upper Montane", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
um

lsa <- year%>%
  filter(Site == "LSA")%>%
  ggplot(aes(yr_effect_duration, yr_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  theme_bw()+
  labs(title = "Lower Subalpine", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
lsa

usa <- year%>%
  filter(Site == "USA")%>%
  ggplot(aes(yr_effect_duration, yr_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  theme_bw()+
  labs(title = "Upper Subalpine", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
usa

alp <- year%>%
  filter(Site == "ALP")%>%
  ggplot(aes(yr_effect_duration, yr_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  theme_bw()+
  labs(title = "Alpine", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
alp

ggarrange(lm, um, lsa, usa, alp, common.legend = T, legend = "right")







####BY SPECIES####
nl <- year_lm%>%
  filter(Event == "NL")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Lower Montane")
nl

fle <- year_lm%>%
  filter(Event == "FLE")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Lower Montane")
fle

fof <- year_lm%>%
  filter(Event == "FOF")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Lower Montane")
fof

#Upper Montane
um <- year_nl%>%
  filter(Site == "UM")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Upper Montane")
um

lsa <- year_nl%>%
  filter(Site == "LSA")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Lower Subalpine")
lsa

usa <- year_nl%>%
  filter(Site == "USA")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Upper Subalpine")
usa

alp <- year_nl%>%
  filter(Site == "ALP")%>%
  ggplot(aes(yr_duration, yr_timing, color = Species))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-40, 40))+
  scale_x_continuous(limits = c(-40, 40))+
  labs(title = "New Leaves at Alpine")
alp

####TREATMENT####
treatment_duration <- treatment

treatment_duration$NL <- treatment_duration$FLE - treatment_duration$NL
treatment_duration$FLE <- treatment_duration$FOF - treatment_duration$FLE
treatment_duration$FOF <- treatment_duration$FLCC - treatment_duration$FOF
treatment_duration$FLCC <- treatment_duration$EOS - treatment_duration$FLCC

treatment_duration <- treatment_duration %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "Duration")

#reorder TIMING dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment_timing <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#average over subplots
treatment_timing <- treatment_timing%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment_timing <- treatment_timing%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

treatment_timing <- treatment_timing%>%
  group_by(Site, Treatment, Event)%>%
  summarise(meanDOY = mean(meanDOY, na.rm = T))

#reorder to take difference in mean DOY between 2018 and 2017
treatment_timing <- treatment_timing%>%
  pivot_wider(names_from = Treatment, values_from = meanDOY)

treatment_timing$trmt_effect_timing <- treatment_timing$Early - treatment_timing$Control

treatment_timing <- select(treatment_timing, -c("Early", "Control"))

#reorder DURATION dataset to be longer (events are in an 'event' column instead of columns of their own)
#average over subplots
treatment_duration <- treatment_duration%>%
  group_by(Site, Block, Treatment, Species, Event)%>%
  summarise(meanDuration = mean(Duration, na.rm = T))

treatment_duration <- treatment_duration%>%
  group_by(Site, Treatment, Species, Event)%>%
  summarise(meanDuration = mean(meanDuration, na.rm = T))

treatment_duration <- treatment_duration%>%
  group_by(Site, Treatment, Event)%>%
  summarise(meanDuration = mean(meanDuration, na.rm = T))

#reorder to take difference in mean DOY between 2018 and 2017
treatment_duration <- treatment_duration%>%
  pivot_wider(names_from = Treatment, values_from = meanDuration)

treatment_duration$trmt_effect_duration <- treatment_duration$Early - treatment_duration$Control

treatment_duration <- select(treatment_duration, -c("Early", "Control"))

treatment <- left_join(treatment_timing, treatment_duration, by = c("Site", "Event"))

#Lower Montane
lm <- treatment%>%
  filter(Site == "LM")%>%
  ggplot(aes(trmt_effect_duration, trmt_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-20, 20))+
  scale_x_continuous(limits = c(-20, 20))+
  theme_bw()+
  labs(title = "Lower Montane", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
lm

um <- treatment%>%
  filter(Site == "UM")%>%
  ggplot(aes(trmt_effect_duration, trmt_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-20, 20))+
  scale_x_continuous(limits = c(-20, 20))+
  theme_bw()+
  labs(title = "Upper Montane", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
um

lsa <- treatment%>%
  filter(Site == "LSA")%>%
  ggplot(aes(trmt_effect_duration, trmt_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-20, 20))+
  scale_x_continuous(limits = c(-20, 20))+
  theme_bw()+
  labs(title = "Lower Subalpine", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
lsa

usa <- treatment%>%
  filter(Site == "USA")%>%
  ggplot(aes(trmt_effect_duration, trmt_effect_timing, color = Event))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  scale_y_continuous(limits = c(-20, 20))+
  scale_x_continuous(limits = c(-20, 20))+
  theme_bw()+
  labs(title = "Upper Subalpine", x = "Duration (short (-)/long (+))", y = "Timing (early (-)/late (+)")
usa

ggarrange(lm, um, lsa, usa, common.legend = T, legend = "right")

