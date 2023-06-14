#####################################


# Analysis script of the domain-general metacognition 4 tasks
# split by task difficulty
# COST action "The neural architecture of consciousness"
# Kravok site of data collection 

# Audrey Mazancieux 2023


#####################################

## Packages and graphic theme ----------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)
library(cowplot)

library(Rcpp) 
library(RcppZiggurat)
library(DEoptim) 
library(reshape)

apatheme=theme_bw()+ #theme
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size = 11))


## Import data ----------------------------------------------------------------

DF_auditory <- read.csv("DF_auditory.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
DF_visual <- read.csv("DF_visual.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
DF_somato <- read.csv("DF_somato.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

DF_all_tasks <- rbind(DF_visual, DF_auditory, DF_somato)


## First-order performance: % Correct per difficulty level -------------------------------------------------------------

Perf_all <- DF_all_tasks %>%
  dcast(Pp ~ Modality, value.var = "Acc", mean)

Perf_dif1 <- DF_all_tasks %>%
  filter(Diff == 1) %>% 
  dcast(Pp ~ Modality, value.var = "Acc", mean)

Perf_dif2 <- DF_all_tasks %>% 
  filter(Diff == 2) %>% 
  dcast(Pp ~ Modality, value.var = "Acc", mean)

Perf_dif1 <- DF2 %>%
  mutate(count = 1) %>% 
  filter(Diff == 1) %>% 
  dcast(Pp ~ Modality, value.var = "count", sum)

Perf_dif2 <- DF2 %>% 
  mutate(count = 1) %>% 
  filter(Diff == 2) %>% 
  dcast(Pp ~ Modality, value.var = "count", sum)


## Data preparation for Meta-d' model  --------------------------------------------

# only exclude the same participants as model with the 2 levels of difficulty 
# to avoid excluding too many participants 

To_excl <- Perf_all %>% 
  filter(Auditory == "NaN" | Visual == "NaN" | Tactile == "NaN" | Pain == "NaN")

DF2 <- DF_all_tasks
for (i in 1:nrow(To_excl)){
  DF2 %<>%
    filter(Pp != To_excl[i,1])
}


Excl <- Perf_all %>% 
  filter(Auditory > 0.95 | Visual > 0.95 | Tactile > 0.95 | Pain > 0.95)

Excl2 <- Perf_all %>% 
  filter(Auditory < 0.55 | Visual < 0.55 | Tactile < 0.55 | Pain < 0.55)

for (i in 1:nrow(Excl)){
  DF2 %<>%
    filter(Pp != Excl[i,1])
}

for (i in 1:nrow(Excl2)){
  DF2 %<>%
    filter(Pp != Excl2[i,1])
}

n <- n_distinct(DF2$Pp)


# Prepare data for the 2 models split by difficulty

DF2 %<>%
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2,
           Modality == "Tactile" ~ 3,
           Modality == "Pain" ~ 4))

ntask <- n_distinct(DF2$Modality)

nR_S1_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S1_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())


for (t in 1:(ntask)) {
  
  S1 <- DF2 %>% 
    filter(Diff == 1) %>% 
    filter(Correct == 1,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == 1 ~ "R1",
      Resp == 2 ~ "R2")) %>% 
    dcast(Pp ~ Confidence + Resp, value.var = "Count", sum) %>% 
    select(`6_R1`,
           `5_R1`,
           `4_R1`,
           `3_R1`,
           `2_R1`,
           `1_R1`,
           `1_R2`,
           `2_R2`,
           `3_R2`,
           `4_R2`,
           `5_R2`,
           `6_R2`)
  
  nR_S1_D1[[t]] %<>%  rbind(S1)
  
  S2 <- DF2 %>% 
    filter(Diff == 1) %>% 
    filter(Correct == 2,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == 1 ~ "R1",
      Resp == 2 ~ "R2")) %>% 
    dcast(Pp ~ Confidence + Resp, value.var = "Count", sum) %>% 
    select(`6_R1`,
           `5_R1`,
           `4_R1`,
           `3_R1`,
           `2_R1`,
           `1_R1`,
           `1_R2`,
           `2_R2`,
           `3_R2`,
           `4_R2`,
           `5_R2`,
           `6_R2`)
  
  nR_S2_D1[[t]] %<>%  rbind(S2)
  
}

for (t in 1:(ntask)) {
  
  S1 <- DF2 %>% 
    filter(Diff == 2) %>% 
    filter(Correct == 1,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == 1 ~ "R1",
      Resp == 2 ~ "R2")) %>% 
    dcast(Pp ~ Confidence + Resp, value.var = "Count", sum) %>% 
    select(`6_R1`,
           `5_R1`,
           `4_R1`,
           `3_R1`,
           `2_R1`,
           `1_R1`,
           `1_R2`,
           `2_R2`,
           `3_R2`,
           `4_R2`,
           `5_R2`,
           `6_R2`)
  
  nR_S1_D2[[t]] %<>%  rbind(S1)
  
  S2 <- DF2 %>% 
    filter(Diff == 2) %>% 
    filter(Correct == 2,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == 1 ~ "R1",
      Resp == 2 ~ "R2")) %>% 
    dcast(Pp ~ Confidence + Resp, value.var = "Count", sum) %>% 
    select(`6_R1`,
           `5_R1`,
           `4_R1`,
           `3_R1`,
           `2_R1`,
           `1_R1`,
           `1_R2`,
           `2_R2`,
           `3_R2`,
           `4_R2`,
           `5_R2`,
           `6_R2`)
  
  nR_S2_D2[[t]] %<>%  rbind(S2)
  
}

## HMeta model - Difficulty 1 ----------------------------------------------------------

for (i in 1:length(nR_S1_D1)) {
  
  nR_S1_D1[[i]] %<>% 
    t %>% data.frame
  
  nR_S2_D1[[i]] %<>% 
    t %>% data.frame
}

source("Function_metad_groupcorr.R")
H_output_D1 <- metad_groupcorr(nR_S1 = nR_S1_D1, nR_S2 = nR_S2_D1)

# Values 
Value <- summary(H_output_D1)
stat_group <- data.frame(mean = Value$statistics[,"Mean"])
stat_group %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(H_output, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(H_output, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit_D1 <- stat_group %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

write.csv(Fit, "./results/dataset2/Hierarchial_Mratio_D1.csv")

# Traceplot
traceplot(H_output_D1)

# MCMC sample
mcmc.sample <- ggs(H_output_D1)
write.csv(mcmc.sample, "./results/dataset2/Hierarchial_mcmc_sample_D1.csv")


