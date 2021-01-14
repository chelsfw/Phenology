
## Date: 2019-01-22 
## User: Chelsea Wilmer
## US DOE Phenology Project
## Script: The purpose of this script is to bring in phenology data, explore, and clean up. and have fun.
## Packages:
library(tidyverse)

## 1. Bring in data ####
# Set your working directory
setwd("/Users/chelseawilmer/Desktop/Github/Phenology")

# Load data
pheno <- read.csv("PhenologyData_complete.csv", stringsAsFactors = FALSE, strip.white = TRUE)

# Look at dataset
str(pheno) #check column names, check data types

# Clean up column names
# Use rename to rename columns
# Rename all columns at once
pheno <- rename(pheno, Veg.Type = Veg.type, Functional.Type = functional.type) # new name equals = old name
# Use select to remove unwanted columns
pheno <- select(pheno, -c(SG, NL2, FLE2, FLD)) # drop variables that are not currently going to be used for analysis. May want to include these later

## 2. Explore data ####
# Look at columns for typos and potential outliers
unique(pheno$Year)
unique(pheno$Site)
unique(pheno$Block.Plot) # this column has extra plots that were established in 2018 because the original plots had not melted out; could be treated as duplicate plots or removed for analysis, or replace original plots; for now, we will indicate these as duplicate plots with a Y/N column 'Duplicate Plot'. Where "B2 2018" and "C1 2018" are YES and all other types are NO

# Check range of phenology observations for each phenophase
range(pheno$FBB, na.rm = TRUE)# because this phenophase isn't going to be used immediately we're going to ommit it. 
range(pheno$NL, na.rm = T)
range(pheno$FLE, na.rm = T)
range(pheno$FOF, na.rm = T)
range(pheno$FLCC, na.rm = T) #DAYS OUT OF RANGE --> go back to data to check/correct

#### 3. Clean up data ####

## Rename sites 
pheno$Site <- ifelse(test = pheno$Site == "Alpine", yes = "ALP", no = pheno$Site)
pheno$Site <- ifelse(test = pheno$Site == "Upper Sub-alpine", yes = "USA", no = pheno$Site)
pheno$Site <- ifelse(test = pheno$Site == "Lower Sub-alpine", yes = "LSA", no = pheno$Site)
pheno$Site <- ifelse(test = pheno$Site == "Upper Montane", yes = "UM", no = pheno$Site)
pheno$Site <- ifelse(test = pheno$Site == "Lower Montane", yes = "LM", no = pheno$Site)

## $Duplicate.Plot
pheno <- rename(pheno, Duplicate.Plot = Block.Plot)
# use logical ifelse statement to assign new values 
pheno$Duplicate.Plot <- ifelse(test = pheno$Duplicate.Plot == "B2 2018" | pheno$Duplicate.Plot == "C1 2018", yes = "Y", no = "N")

## $Treatment
# use logical statement to fill in blank values in the treatment column. I imported values without that info. 
# no = pheno$Treatment allowed us to preserve previous data in the cells we didn't want to change
pheno$Treatment <- ifelse(test = pheno$Treatment == "", yes = "Treatment", no = pheno$Treatment)

## $Functional.Type
# Replace names 'Gram' and 'Grass' to 'Graminoid'
pheno$Functional.Type <- ifelse(test = pheno$Functional.Type == "Gram" | pheno$Functional.Type == "Grass", yes = "Graminoid", no = pheno$Functional.Type)

## $Species
# Order species data by alphabet to check for typos
species.list <- unique(pheno$Species) #pull out unique species as character vector
species.list <- sort(species.list) # sort alphabetically
# Correct errors in species list
pheno$Species <- ifelse(test = pheno$Species == "Fuzzy Forb  1", "Fuzzy Forb 1", pheno$Species)
pheno$Species <- ifelse(test = pheno$Species == "Hymenoxis", "Hymenoxys hoopesii", pheno$Species)
pheno$Species <- ifelse(test = pheno$Species == "Linum Lewisii", "Linum lewisii", pheno$Species)
pheno$Species <- ifelse(test = pheno$Species == "potentilla", "Potentilla sp. 2", pheno$Species)
pheno$Species <- ifelse(test = pheno$Species == "Valeriana sp.", "Valeriana edulis", pheno$Species)

#### NEXT STEP ####
# maybe split into separate data frames for each of the sites
# make grappppphhhhhsss

#write_csv(pheno, "/Users/chelseawilmer/Desktop/Watershed Function SFA/Phenology_Complete.csv")
