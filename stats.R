rm(list = ls())
## Date: 2021-02-18
## User: Chelsea Wilmer
## SFA/CSU Thesis
## Script: percent species early graphs for elevations
## Packages:
library(lme4)
library(lmerTest)
library(emmeans)

####Bring in phenology data####
source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
# path for Dana
#source("Phenology.data.cleanup.R")

#remove columns that we don't need
pheno <- select(pheno, -c(Duplicate.Plot, FBB, FLB, Plot))

str(pheno)

#make variables factos
pheno$Year <- as.factor(pheno$Year)
pheno$Site <- as.factor(pheno$Site)
pheno$PlotPair <- as.factor(pheno$PlotPair)
pheno$Plot_ID <- as.factor(pheno$Plot_ID)
pheno$Subplot_ID <- as.factor(pheno$Subplot_ID)
pheno$Treatment <- as.factor(pheno$Treatment)
pheno$Species <- as.factor(pheno$Species)
pheno$Functional.Type <- as.factor(pheno$Functional.Type)
pheno$EOS <- as.integer(pheno$EOS)
pheno$Elevation <- as.integer(pheno$Elevation)

pheno <- filter(pheno, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####Full Model####
model <- lmer(NL ~ Site*Treatment*Year +  Species + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = pheno)
anova(model, ddf = "Kenward-Roger")

####Make year and Treatment effect data sets####
#filter pheno dataset for the control plots to make effect of the year dataset
year <- filter(pheno, Treatment == "Control")
str(year)

#filter pheno dataset for 2018 to make the effect of the treatment dataset
treatment <- filter(pheno, Year == 2018)
treatment <- filter(treatment, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")

####Year####
#NL
model1 <- lmer(NL ~ Site*Year + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = year)
anova(model1, ddf = "Kenward-Roger")
emmeans(model1, pairwise ~ Year|Site)
emmeans(model1, pairwise ~ Site|Year)

plot(model1)

#FLE
model2 <- lmer(FLE ~ Site*Year + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = year)
anova(model2, ddf = "Kenward-Roger")
emmeans(model2, pairwise ~ Year|Site)
emmeans(model2, pairwise ~ Site|Year)

#FOF
model3 <- lmer(FOF ~ Site*Year + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = year)
anova(model3, ddf = "Kenward-Roger")
emmeans(model3, pairwise ~ Year|Site)
emmeans(model3, pairwise ~ Site|Year)

#FLCC
model4 <- lmer(FLCC ~ Site*Year + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = year)
anova(model4, ddf = "Kenward-Roger")
emmeans(model4, pairwise ~ Year|Site)
emmeans(model4, pairwise ~ Site|Year)

####Treatment####
#NL
model5 <- lmer(NL ~ Site*Treatment + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model5, ddf = "Kenward-Roger")
emmeans(model5, pairwise ~ Treatment|Site)
emmeans(model5, pairwise ~ Site|Treatment)

plot(model5)

#FLE
model6 <- lmer(FLE ~ Site*Treatment + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model6, ddf = "Kenward-Roger")
emmeans(model6, pairwise ~ Treatment|Site)
emmeans(model6, pairwise ~ Site|Treatment)

#FOF
model7 <- lmer(FOF ~ Site*Treatment + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model7, ddf = "Kenward-Roger")
emmeans(model7, pairwise ~ Treatment|Site)
emmeans(model7, pairwise ~ Site|Treatment)

#FLCC
model8 <- lmer(FLCC ~ Site*Treatment + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model8, ddf = "Kenward-Roger")
emmeans(model8, pairwise ~ Treatment|Site)
emmeans(model8, pairwise ~ Site|Treatment)

#test
model8 <- lmer(FLCC ~ Site*DOYsf + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model8, ddf = "Kenward-Roger")
emmeans(model8, pairwise ~ Treatment|Site)
emmeans(model8, pairwise ~ Site|Treatment)

model7 <- lmer(FOF ~ Site*DOYsf + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model7, ddf = "Kenward-Roger")
emmeans(model7, pairwise ~ Treatment|Site)
emmeans(model7, pairwise ~ Site|Treatment)

model6 <- lmer(FLE ~ Site*DOYsf + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model6, ddf = "Kenward-Roger")
emmeans(model6, pairwise ~ Treatment|Site)
emmeans(model6, pairwise ~ Site|Treatment)

model5 <- lmer(NL ~ Site*DOYsf + (1|PlotPair) + (1|Plot_ID) + (1|Subplot_ID), data = treatment)
anova(model5, ddf = "Kenward-Roger")
emmeans(model5, pairwise ~ Site|DOYsf)