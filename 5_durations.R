rm(list = ls())
## Date: 2020-04-10
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: Calculating durations
## Packages:
library(tidyverse)
library(ggpubr)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB))

#filter observations for year
year <- filter(pheno, Treatment == "Control")

#filter observations for treatment
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####Durations for year####
year <- year%>%
  group_by(Year, Site, Block, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))

year <- year%>%
  group_by(Year, Site, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))
  

#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
year$NL <- year$FLE - year$NL
year$FLE <- year$FOF - year$FLE
year$FOF <- year$FLCC - year$FOF
year$FLCC <- 277 - year$FLCC

#reorganize data
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "duration") 

year <- na.omit(year)

year <- year%>%
  pivot_wider(names_from = Year, values_from = duration)

year <- na.omit(year)

year$duration <- year$`2018` - year$`2017`

year <- select(year, -c("2017", "2018"))

####NL####
NL <- filter(year, Event == "NL")

#what does this shit look like
lm <- NL%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LM")

um <- NL%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "UM")

lsa <- NL%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "LSA")

usa <- NL%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "USA")

alp <- NL%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,30))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa, alp)

####FLE####
FLE <- filter(year, Event == "FLE")

#what does this shit look like
lm <- FLE%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,55))+
  labs(title = "LM")

um <- FLE%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,55))+
  labs(title = "UM")

lsa <- FLE%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,55))+
  labs(title = "LSA")

usa <- FLE%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,55))+
  labs(title = "USA")

alp <- FLE%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,55))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####FOF####
FOF <- filter(year, Event == "FOF")

#what does this shit look like
lm <- FOF%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,70))+
  labs(title = "LM")

um <- FOF%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,70))+
  labs(title = "UM")

lsa <- FOF%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,70))+
  labs(title = "LSA")

usa <- FOF%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,70))+
  labs(title = "USA")

alp <- FOF%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,70))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####FLCC####
FLCC <- filter(year, Event == "FLCC")

#what does this shit look like
lm <- FLCC%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-55,40))+
  labs(title = "LM")

um <- FLCC%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-55,40))+
  labs(title = "UM")

lsa <- FLCC%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-55,40))+
  labs(title = "LSA")

usa <- FLCC%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "USA")

alp <- FLCC%>%
  filter(Site == "ALP")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-50,40))+
  labs(title = "ALP")

ggarrange(lm, um, lsa, usa, alp)

####Durations for treatment####
treatment <- treatment%>%
  group_by(Site, Block, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))

treatment <- treatment%>%
  group_by(Site, Treatment, Species)%>%
  summarise(NL= mean(NL, na.rm = T),
            FLE= mean(FLE, na.rm = T),
            FOF= mean(FOF, na.rm = T),
            FLCC= mean(FLCC, na.rm = T))


#Calculate durations as event2 - event1, event3 - event2 etc
#calculate NL-DOYsf
treatment$NL <- treatment$FLE - treatment$NL
treatment$FLE <- treatment$FOF - treatment$FLE
treatment$FOF <- treatment$FLCC - treatment$FOF
treatment$FLCC <- 277 - treatment$FLCC

#reorganize data
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF", "FLCC"),
               names_to = "Event", values_to = "duration") 

treatment <- na.omit(treatment)

treatment <- treatment%>%
  pivot_wider(names_from = Treatment, values_from = duration)

treatment <- na.omit(treatment)

treatment$duration <- treatment$Early - treatment$Control

treatment <- select(treatment, -c("Early", "Control"))

treatment <- na.omit(treatment)

####NL####
NL <- filter(treatment, Event == "NL")

#what does this shit look like
lm <- NL%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LM")

um <- NL%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "UM")

lsa <- NL%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "LSA")

usa <- NL%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,10))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLE####
FLE <- filter(treatment, Event == "FLE")

#what does this shit look like
lm <- FLE%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,35))+
  labs(title = "LM")

um <- FLE%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,35))+
  labs(title = "UM")

lsa <- FLE%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,35))+
  labs(title = "LSA")

usa <- FLE%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-20,35))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FOF####
FOF <- filter(treatment, Event == "FOF")

#what does this shit look like
lm <- FOF%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-15,25))+
  labs(title = "LM")

um <- FOF%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-15,25))+
  labs(title = "UM")

lsa <- FOF%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-15,25))+
  labs(title = "LSA")

usa <- FOF%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-15,25))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)

####FLCC####
FLCC <- filter(treatment, Event == "FLCC")

#what does this shit look like
lm <- FLCC%>%
  filter(Site == "LM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "LM")

um <- FLCC%>%
  filter(Site == "UM")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "UM")

lsa <- FLCC%>%
  filter(Site == "LSA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "LSA")

usa <- FLCC%>%
  filter(Site == "USA")%>%
  ggplot(aes(Species, duration))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(-30,30))+
  labs(title = "USA")

ggarrange(lm, um, lsa, usa)
