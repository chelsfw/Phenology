rm(list = ls())
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(MuMIn)

source("/Users/chelseawilmer/Desktop/Github/Phenology/Phenology.data.cleanup.R")
str(pheno)

#make variables factors
pheno$Year <- as.factor(pheno$Year)
pheno$Site <- as.factor(pheno$Site)
pheno$PlotPair <- as.factor(pheno$PlotPair)
pheno$Plot_ID <- as.factor(pheno$Plot_ID)
pheno$Subplot_ID <- as.factor(pheno$Subplot_ID)
pheno$Treatment <- as.factor(pheno$Treatment)
pheno$Species <- as.factor(pheno$Species)
pheno$Functional.Type <- as.factor(pheno$Functional.Type)
pheno$Elevation <- as.integer(pheno$Elevation)

pheno$NL <- pheno$FLE - pheno$NL
pheno$FLE <- pheno$FOF - pheno$FLE
pheno$FOF <- pheno$FLCC - pheno$FOF
pheno$FLCC <- pheno$EOS - pheno$FLCC

pheno <- filter(pheno, Site == "LM" | Site == "UM" | Site == "LSA" | Site == "USA")
