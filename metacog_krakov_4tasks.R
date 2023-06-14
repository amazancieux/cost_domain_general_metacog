#####################################


# Analysis script of the domain-general metacognition 4 tasks:
# visual, auditory, tactile, and pain perception
# COST action "The neural architecture of consciousness"
# Kravok site of data collection 

# Audrey Mazancieux 2022


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


## First-order performance: % Correct -------------------------------------------------------------

Perf <- DF_all_tasks %>% 
  dcast(Pp ~ Modality, value.var = "Acc", mean)


## Raw confidence  -------------------------------------------------------

Raw_conf <- DF_all_tasks %>% 
  dcast(Pp ~ Modality, value.var = "Confidence", mean)

write.csv(Raw_conf, "./results/dataset2/raw_confidence2.csv")


## Data preparation for Meta-d' model  --------------------------------------------

# exclusion of pp that did not performed the 4 tasks

To_excl <- Perf %>% 
  filter(Auditory == "NaN" | Visual == "NaN" | Tactile == "NaN" | Pain == "NaN")

DF2 <- DF_all_tasks
for (i in 1:nrow(To_excl)){
  DF2 %<>%
    filter(Pp != To_excl[i,1])
}

# exclude pp with performance above 95% and bellow 55% for HHM

Excl <- Perf %>% 
  filter(Auditory > 0.95 | Visual > 0.95 | Tactile > 0.95 | Pain > 0.95)

Excl2 <- Perf %>% 
  filter(Auditory < 0.55 | Visual < 0.55 | Tactile < 0.55 | Pain < 0.55)

for (i in 1:nrow(Excl)){
  DF2 %<>%
    filter(Pp != Excl[i,1])
}

for (i in 1:nrow(Excl2)){
  DF2 %<>%
    filter(Pp != Excl2[i,1])
}

# Prepare data

DF2 %<>%
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2,
           Modality == "Tactile" ~ 3,
           Modality == "Pain" ~ 4))

ntask <- n_distinct(DF2$Modality)

nR_S1 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2 <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())


for (t in 1:(ntask)) {
  
  S1 <- DF2 %>% 
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
  
  nR_S1[[t]] %<>%  rbind(S1)
  
  S2 <- DF2 %>% 
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
  
  nR_S2[[t]] %<>%  rbind(S2)
  
}

# save included participants ID
sub_id <- data.frame(Pp = sort(unique(DF2$Pp)))
write.csv2(sub_id, "./results/dataset2/participant_id_H-metad.csv")


## HMeta model ----------------------------------------------------------

for (i in 1:length(nR_S1)) {
  
  nR_S1[[i]] %<>% 
    t %>% data.frame
  
  nR_S2[[i]] %<>% 
    t %>% data.frame
}

source("Function_metad_groupcorr.R")
H_output <- metad_groupcorr(nR_S1 = nR_S1, nR_S2 = nR_S2)

# Values 
Value <- summary(H_output)
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
Fit <- stat_group %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

write.csv(Fit, "./results/dataset2/Hierarchial_Mratio.csv")

# Traceplot
traceplot(H_output)

# MCMC sample
mcmc.sample <- ggs(H_output)
write.csv(mcmc.sample, "./results/dataset2/Hierarchial_mcmc_sample.csv")



## Plot posterior distributions -------------------------------------------------------------------

# Import data
mcmc.sample <- read.csv("./results/dataset2/Hierarchial_mcmc_sample.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
Fit <- read.csv("./results/dataset2/Hierarchial_Mratio.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# Plot mu_logMratio posterior distribution

Auditory <- mcmc.sample %>%
    filter(Parameter == "mu_logMratio[1]") %>% 
    ggplot(aes(exp(value))) +
    geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
    geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[1]"]),linetype="dashed", size = 1) +
    geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[1]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[1]"]), yend = 200), colour = "white", size = 2.5) +
    apatheme +
    xlim(c(0.70, 1.40)) +
    ylim(c(0, 6500)) +
    ylab("Sample count") +
    xlab(expression(paste(mu, " Mratio")))

Visual <- mcmc.sample %>%
    filter(Parameter == "mu_logMratio[2]") %>% 
    ggplot(aes(exp(value))) +
    geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
    geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[2]"]),linetype="dashed", size = 1) +
    geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[2]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[2]"]), yend = 200), colour = "white", size = 2.5) +
    apatheme +
    xlim(c(0.70, 1.40)) +
    ylim(c(0, 6500)) +
    ylab("Sample count") +
    xlab(expression(paste(mu, " Mratio")))

Tactile <- mcmc.sample %>%
    filter(Parameter == "mu_logMratio[3]") %>% 
    ggplot(aes(exp(value))) +
    geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
    geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[3]"]),linetype="dashed", size = 1) +
    geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[3]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[3]"]), yend = 200), colour = "white", size = 2.5) +
    apatheme +
    xlim(c(0.70, 1.40)) +
    ylim(c(0, 6500)) +
    ylab("Sample count") +
    xlab(expression(paste(mu, " Mratio")))

Pain <- mcmc.sample %>%
    filter(Parameter == "mu_logMratio[4]") %>% 
    ggplot(aes(exp(value))) +
    geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
    geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[4]"]),linetype="dashed", size = 1) +
    geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[4]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[4]"]), yend = 200), colour = "white", size = 2.5) +
    apatheme +
    xlim(c(0.70, 1.40)) +
    ylim(c(0, 6500)) +
    ylab("Sample count") +
    xlab(expression(paste(mu, " Mratio")))

png(file="./plots/dataset2/MRATIO_4MODALITIES.png", width=10, height=8, units="in", res=300)
plot_grid(Auditory, Visual, Tactile, Pain, labels = c("Auditory", "Visual", "Tactile", "Pain"), nrow = 2, ncol = 2) 
dev.off()


# Plot rho posterior distribution

mcmc.rho <- mcmc.sample %>% 
  filter(Parameter == "rho[1]"| Parameter == "rho[2]"| Parameter == "rho[3]"| Parameter == "rho[4]"| Parameter == "rho[5]"| Parameter == "rho[6]")

Rho1 <- mcmc.sample %>%
  filter(Parameter == "rho[1]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[1]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[1]"], y = 80, xend = HDI$upper[HDI$name == "rho[1]"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for auditory/visual correlation")))

Rho2 <- mcmc.sample %>%
  filter(Parameter == "rho[2]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[2]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[2]"], y = 80, xend = HDI$upper[HDI$name == "rho[2]"], yend = 80),colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for auditory/tactile correlation")))

Rho3 <- mcmc.sample %>%
  filter(Parameter == "rho[3]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[3]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[3]"], y = 80, xend = HDI$upper[HDI$name == "rho[3]"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for auditory/pain correlation")))

Rho4 <- mcmc.sample %>%
  filter(Parameter == "rho[4]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[4]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[4]"], y = 80, xend = HDI$upper[HDI$name == "rho[4]"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for visual/tactile correlation")))

Rho5 <- mcmc.sample %>%
  filter(Parameter == "rho[5]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[5]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[5]"], y = 80, xend = HDI$upper[HDI$name == "rho[5]"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for visual/pain correlation")))

Rho6 <- mcmc.sample %>%
  filter(Parameter == "rho[6]") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho[6]"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho[6]"], y = 80, xend = HDI$upper[HDI$name == "rho[6]"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylim(c(0, 4500)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for tactile/pain correlation")))

png(file="./plots/dataset2/MRATIO_6COR.png", width=10, height=8, units="in", res=300)
plot_grid(Rho1, Rho2, Rho3, NULL, Rho4, Rho5, NULL, NULL, Rho6, labels = c("A", "B", "C", "", "D", "E", "", "", "F"), nrow = 3, ncol = 3)
dev.off()


## Individual meta_d function -----------------------------------------------------

# Prepare data

DF3 <- DF_all_tasks %>% 
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2,
           Modality == "Tactile" ~ 3,
           Modality == "Pain" ~ 4))


ntask <- n_distinct(DF3$Modality)

nR_S1_indiv <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nR_S2_indiv <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

for (t in 1:(ntask)) {
  
  S1 <- DF3 %>% 
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
  
  nR_S1_indiv[[t]] %<>%  rbind(S1)
  
  S2 <- DF3 %>% 
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
  
  nR_S2_indiv[[t]] %<>%  rbind(S2)
  
}

# Calculate one meta-d' per pp per task

source("Function_metad_indiv.R")

d <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

c <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

stat <- list(
  data.frame(),
  data.frame(),
  data.frame(),
  data.frame())

nsubj <- nrow(nR_S1_indiv[[1]])


# Calculate d' and meta-d' per task and per participant

for (t in 1:(ntask)) {
  
  for (n in 1:(nsubj)) {
    
    S1 <- c(t(nR_S1_indiv[[t]][n,]))
    S2 <- c(t(nR_S2_indiv[[t]][n,]))
    
    output <- metad_indiv(nR_S1 = S1, nR_S2 = S2)
    
    d[[t]] %<>% rbind(d1) 
    c[[t]] %<>% rbind(c1) 
    
    Value <- summary(output)
    
    stat1 <- data.frame(mean = Value[["statistics"]][, "Mean"])
    stat[[t]] %<>% rbind(t(stat1)) 
    
  }
}

# Arrange output

Auditory <- Perf %>% 
  select(Pp = Pp,
         Perf = Auditory) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat[[1]][,11],
         d = d[[1]][,1],
         Modality = 'Auditory')

Visual <- Perf %>% 
  select(Pp = Pp,
         Perf = Visual) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat[[2]][,11],
         d = d[[2]][,1],
         Modality = 'Visual')

Tactile <- Perf %>% 
  select(Pp = Pp,
         Perf = Tactile) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat[[3]][,11],
         d = d[[3]][,1],
         Modality = 'Tactile')

Pain <- Perf %>% 
  select(Pp = Pp,
         Perf = Pain) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat[[4]][,11],
         d = d[[4]][,1],
         Modality = 'Pain')


write.csv(Auditory, "./results/dataset2/auditory_individual_metad.csv")
write.csv(Visual, "./results/dataset2/visual_individual_metad.csv")
write.csv(Tactile, "./results/dataset2/tactile_individual_metad.csv")
write.csv(Pain, "./results/dataset2/pain_individual_metad.csv")


## Calculate ratio and plot data ------------------------------------------------------------

all_data <- Auditory %>% 
  rbind(Visual, 
        Tactile, 
        Pain) %>% 
  mutate(Mratio = metad/d)

# Performance exclusion criteria

all_data %<>%
  filter(Perf >= 0.55 & Perf <= 0.95)

# Exlude participants than add issue with number of trials on the tactile task
all_data <- subset(all_data, all_data$Pp != 65251 | all_data$Modality != "Tactile")
all_data <- subset(all_data, all_data$Pp != 87127 | all_data$Modality != "Tactile")


write.csv2(all_data, "./results/dataset2/Individual_Mratio_forplots.csv")



