rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(dplyr)
library(car)
library(emmeans)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- dplyr::select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))
pheno$Elevation <- 0
pheno$Elevation <- ifelse(pheno$Site == "LM", 2774, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "UM", 2957, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "LSA", 3169, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "USA", 3475, pheno$Elevation)
pheno$Elevation <- ifelse(pheno$Site == "ALP", 3597, pheno$Elevation)

#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####YEAR####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
year <- year %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

year <- year%>%
  group_by(Year, Elevation, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

year <- na.omit(year)

year_nl <- filter(year, Event == "NL")
year_fle <- filter(year, Event == "FLE")
year_fof <- filter(year, Event == "FOF")
year_flcc <- filter(year, Event == "FLCC")


####TRMT####
#reorder dataset to be longer (events are in an 'event' column instead of columns of their own)
treatment <- treatment %>% 
  pivot_longer(cols = c("NL", "FLE", "FOF","FLCC"),
               names_to = "Event", values_to = "DOY") 

#calculate means by block
treatment <- treatment%>%
  group_by(Elevation, Block, Treatment, Species, Event)%>%
  summarise(meanDOY = mean(DOY, na.rm = T))

treatment <- na.omit(treatment)

treatment_nl <- filter(treatment, Event == "NL")
treatment_fle <- filter(treatment, Event == "FLE")
treatment_fof <- filter(treatment, Event == "FOF")
treatment_flcc <- filter(treatment, Event == "FLCC")

####STATS YEAR####
#ancova alternate parameterization
year_nl$Year <- as.factor(year_nl$Year)
p <- qplot(Elevation, meanDOY, color = Year, data = year_nl, main = "NL") 
p+geom_smooth(method = "lm", se = F, fullrange = T)

sumstats <- summarise(group_by(year_nl, Elevation),
                      n = n(),
                      mean = mean(meanDOY),
                      sd = sd(meanDOY),
                      SE = sd/sqrt(n))

model_yr_nl <- lm(meanDOY ~ -1 + Year + Elevation + Year:Elevation, data = year_nl)
extractAIC(model_yr_nl)
summary(model_yr_nl)

#ancova not alternate parameterization
model_yr_nl_2 <- lm(meanDOY~ Year*Elevation, data = year_nl)
Anova(model_yr_nl_2, type = 3)
extractAIC(model_yr_nl_2)
summary(model_yr_nl_2)

#ancova no interaction
model_yr_nl_3 <- lm(meanDOY ~ Year + Elevation, data = year_nl)
Anova(model_yr_nl_3, type = 3)
par(mfrow = c(2,2)) 
plot(model_yr_nl_3)

emout <- emmeans(model_yr_nl_3, pairwise ~ Year) 
emout
CLD(emout$emmeans)

#anova
Model_yr_nl_4 <- lm(meanDOY ~ Year + Elevation, data = year_nl) 
summary(Model_yr_nl_4)

