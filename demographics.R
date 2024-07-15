#####################################


# Demographics for 3 datasets of the project 
# COST action "The neural architecture of consciousness"

# Audrey Mazancieux 2024


#####################################


## Packages ----------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)
library(broom)


## Import data -----------------------------------------------------------

# demographics
aarhus_site <- read.csv("./demographics/cost_ahs_demographics_2.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
krakow_site <- read.csv("./demographics/cost_mri_bio.csv", header=FALSE, sep="\t", dec=".", fill  = TRUE)
  
# preprocessed data
dataset1 <- read.csv("./results/dataset1/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset2 <- read.csv("./results/dataset2/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset3 <- read.csv("./results/dataset3/clean_individual_Mratio.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)


## Get participant per dataset -----------------------------------------------

# dataset 1
aarhus_site %<>% 
  mutate(Pp = ID)
pp1 <- data.frame(Pp = unique(dataset1$Pp))
pp_data1 <- merge(pp1, aarhus_site, by='Pp')

summary(pp_data1)
pp_data1 %>% 
  group_by(gen) %>% 
  count()

# dataset 2
krakow_site %<>% 
  select(Pp = V1,
         gender = V2, 
         age = V3)
pp2 <- data.frame(Pp = unique(dataset2$Pp))
pp_data2 <- merge(pp2, krakow_site, by='Pp')

summary(pp_data2)
pp_data2 %>% 
  group_by(gender) %>% 
  count()

# dataset 3
aarhus_krakow <- aarhus_site %>% 
  select(Pp, 
         gender = gen,
         age = age) %>% 
  add_row(krakow_site)

pp3 <- data.frame(Pp = unique(dataset3$Pp))
pp_data3 <- merge(aarhus_krakow, pp3, by='Pp')

summary(pp_data3)
pp_data3 %>% 
  group_by(gender) %>% 
  count()
