rm(list = ls())
#Date: 08-11-2021
#User: CWilmer
#SFA/CSU Thesis
#Purpose: scatterplots of timing vs. duration

####Bring in phenology data####
#load packages
library(tidyverse)

#load source code
results <- read.csv("/Users/chelseawilmer/Desktop/Github/Phenology/results.csv")

results$Phase <- factor(results$Phase, levels = c("DOYsf", "NL", "FLE", "FOF", "FLCC"))
results$Site <- factor(results$Site, levels = c("LM", "UM", "LSA", "USA", "ALP"))

results$Variable <- factor(results$Variable, levels = c("Timing", "Duration"))
results$Set <- factor(results$Set, levels = c("Year", "Treatment"))

ggplot(results, aes(Phase, Effect, fill = Site))+
  geom_col(position = "dodge")+
  facet_wrap(Variable~Set, scales = "free_y")+
  labs(y = "Days earlier(-)/later(+) (Timing) or shorter(-)/longer(+) (Duration)")
