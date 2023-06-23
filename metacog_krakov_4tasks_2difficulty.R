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
Value <- gelman.diag(H_output_D1, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(H_output_D1, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit_D1 <- stat_group %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

write.csv(Fit_D1, "./results/dataset2/Hierarchial_Mratio_D1.csv")

# Traceplot
traceplot(H_output_D1)

# MCMC sample
mcmc.sample <- ggs(H_output_D1)
write.csv(mcmc.sample, "./results/dataset2/Hierarchial_mcmc_sample_D1.csv")



## HMeta model - Difficulty 2 ----------------------------------------------------------

for (i in 1:length(nR_S1_D2)) {
  
  nR_S1_D2[[i]] %<>% 
    t %>% data.frame
  
  nR_S2_D2[[i]] %<>% 
    t %>% data.frame
}

source("Function_metad_groupcorr.R")
H_output_D2 <- metad_groupcorr(nR_S1 = nR_S1_D2, nR_S2 = nR_S2_D2)

# Values 
Value <- summary(H_output_D2)
stat_group <- data.frame(mean = Value$statistics[,"Mean"])
stat_group %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(H_output_D2, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(H_output_D2, prob = 0.95))
HDI %<>%
  rownames_to_column(var = "name")

# All values in the same dataframe
Fit_D2 <- stat_group %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

write.csv(Fit_D2, "./results/dataset2/Hierarchial_Mratio_D2.csv")

# Traceplot
traceplot(H_output_D1)

# MCMC sample
mcmc.sample <- ggs(H_output_D1)
write.csv(mcmc.sample, "./results/dataset2/Hierarchial_mcmc_sample_D1.csv")




## Individual meta-d' - Difficulty 1 -------------------------------------

# Prepare data

DF_indiv_D1 <- DF_all_tasks %>% 
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2,
           Modality == "Tactile" ~ 3,
           Modality == "Pain" ~ 4)) %>% 
  filter(Diff == 1)


ntask <- n_distinct(DF_indiv_D1$Modality)

nR_S1_indiv_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2_indiv_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

for (t in 1:(ntask)) {
  
  S1 <- DF_indiv_D1 %>% 
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
  
  nR_S1_indiv_D1[[t]] %<>%  rbind(S1)
  
  S2 <- DF_indiv_D1 %>% 
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
  
  nR_S2_indiv_D1[[t]] %<>%  rbind(S2)
  
}

# Calculate one meta-d' per pp per task

source("Function_metad_indiv.R")

d_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

c_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

stat_D1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nsubj <- nrow(nR_S1_indiv[[1]])


# Calculate d' and meta-d' per task and per participant

for (t in 1:(ntask)) {
  
  for (n in 1:(nsubj)) {
    
    S1 <- c(t(nR_S1_indiv_D1[[t]][n,]))
    S2 <- c(t(nR_S2_indiv_D1[[t]][n,]))
    
    output <- metad_indiv(nR_S1 = S1, nR_S2 = S2)
    
    d_D1[[t]] %<>% rbind(d1) 
    c_D1[[t]] %<>% rbind(c1) 
    
    Value <- summary(output)
    
    stat1 <- data.frame(mean = Value[["statistics"]][, "Mean"])
    stat_D1[[t]] %<>% rbind(t(stat1)) 
    
  }
}

# Arrange output

Auditory_D1 <- Perf %>% 
  select(Pp = Pp,
         Perf = Auditory) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D1[[1]][,11],
         c = c_D1[[1]][,1],
         d = d_D1[[1]][,1],
         Modality = 'Auditory')

Visual_D1 <- Perf %>% 
  select(Pp = Pp,
         Perf = Visual) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D1[[2]][,11],
         c = c_D1[[2]][,1],
         d = d_D1[[2]][,1],
         Modality = 'Visual')

Tactile_D1 <- Perf %>% 
  select(Pp = Pp,
         Perf = Tactile) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D1[[3]][,11],
         d = d_D1[[3]][,1],
         c = c_D1[[3]][,1],
         Modality = 'Tactile')

Pain_D1 <- Perf %>% 
  select(Pp = Pp,
         Perf = Pain) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D1[[4]][,11],
         d = d_D1[[4]][,1],
         c = c_D1[[4]][,1],
         Modality = 'Pain')

all_data_D1 <- Auditory_D1 %>% 
  rbind(Visual_D1, 
        Tactile_D1, 
        Pain_D1) %>% 
  mutate(Mratio = metad/d)


write.csv2(all_data_D1, "./results/dataset2/Individual_Mratio_D1.csv")




## Individual meta-d' - Difficulty 2 ---------------------------------------

# Prepare data

DF_indiv_D2 <- DF_all_tasks %>% 
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2,
           Modality == "Tactile" ~ 3,
           Modality == "Pain" ~ 4)) %>% 
  filter(Diff == 2)


ntask <- n_distinct(DF_indiv_D2$Modality)

nR_S1_indiv_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2_indiv_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

for (t in 1:(ntask)) {
  
  S1 <- DF_indiv_D2 %>% 
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
  
  nR_S1_indiv_D2[[t]] %<>%  rbind(S1)
  
  S2 <- DF_indiv_D2 %>% 
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
  
  nR_S2_indiv_D2[[t]] %<>%  rbind(S2)
  
}

# Calculate one meta-d' per pp per task

source("Function_metad_indiv.R")

d_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

c_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

stat_D2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nsubj <- nrow(nR_S1_indiv[[1]])


# Calculate d' and meta-d' per task and per participant

for (t in 1:(ntask)) {
  
  for (n in 1:(nsubj)) {
    
    S1 <- c(t(nR_S1_indiv_D2[[t]][n,]))
    S2 <- c(t(nR_S2_indiv_D2[[t]][n,]))
    
    output <- metad_indiv(nR_S1 = S1, nR_S2 = S2)
    
    d_D2[[t]] %<>% rbind(d1) 
    c_D2[[t]] %<>% rbind(c1) 
    
    Value <- summary(output)
    
    stat1 <- data.frame(mean = Value[["statistics"]][, "Mean"])
    stat_D2[[t]] %<>% rbind(t(stat1)) 
    
  }
}

# Arrange output

Auditory_D2 <- Perf %>% 
  select(Pp = Pp,
         Perf = Auditory) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D2[[1]][,11],
         c = c_D2[[1]][,1],
         d = d_D2[[1]][,1],
         Modality = 'Auditory')

Visual_D2 <- Perf %>% 
  select(Pp = Pp,
         Perf = Visual) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D2[[2]][,11],
         c = c_D2[[2]][,1],
         d = d_D2[[2]][,1],
         Modality = 'Visual')

Tactile_D2 <- Perf %>% 
  select(Pp = Pp,
         Perf = Tactile) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D2[[3]][,11],
         d = d_D2[[3]][,1],
         c = c_D2[[3]][,1],
         Modality = 'Tactile')

Pain_D2 <- Perf %>% 
  select(Pp = Pp,
         Perf = Pain) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat_D2[[4]][,11],
         d = d_D2[[4]][,1],
         c = c_D2[[4]][,1],
         Modality = 'Pain')

all_data_D2 <- Auditory_D2 %>% 
  rbind(Visual_D2, 
        Tactile_D2, 
        Pain_D2) %>% 
  mutate(Mratio = metad/d)


write.csv2(all_data_D2, "./results/dataset2/Individual_Mratio_D2.csv")



## Individual stats and plots -------------------------------------------

all_data_D2 <- read.csv("./results/dataset2/Individual_Mratio_D2.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
all_data_D1 <- read.csv("./results/dataset2/Individual_Mratio_D1.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

all_data_D1 %<>%
  mutate(Difficulty = "Difficulty 1")

all_data_D2 %<>%
  mutate(Difficulty = "Difficulty 2")

all_diff <- all_data_D1 %>% 
  rbind(all_data_D2)

nsubj <- n_distinct(all_diff$Pp)

all_diff %>%
  group_by(Modality, Difficulty) %>%
  summarise(VD = mean(d),
            sd = sd(d),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = dataset1,
             aes(x = Task, y = d, color = Task),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.2, 
             show.legend = FALSE) +
  geom_boxplot(data = dataset1,
               aes(x = Task, y = d, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(2), 
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  xlab("Task") +
  ylab("d' value")