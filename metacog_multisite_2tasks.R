#####################################


# Analysis script of multisite metacognition dataset: 
# visual and auditory modalities
# COST action "The neural architecture of consciousness"
# Aarhus and Kravok site of data collection 

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

pp_names_K <- list.files("./rawdata/Krakow_multisite")
pp_names_A <- list.files("/rawdata/Aarhus_multisite")

files_K <-
  list.files("./rawdata/Krakow_multisite", recursive = TRUE) %>% 
  as_data_frame() %>% 
  filter(str_detect(value, regex(".dat$")))

files_A <-
  list.files("./rawdata/Aarhus_multisite", recursive = TRUE) %>% 
  as_data_frame() %>% 
  filter(str_detect(value, regex(".dat$")))



data_K <- data.frame()

for (i in files_K$value){
  
  pp_data <- read.csv(file.path("./rawdata/Krakow_multisite", i), header = TRUE, sep="\t", dec=".", fill  = TRUE, strip.white=TRUE) %>% 
    mutate(Pp = i %>% 
             str_extract(regex("\\d+")))
  
  data_K %<>% rbind(pp_data) 
}

data_A <- data.frame()

for (i in files_A$value){
  
  pp_data <- read.csv(file.path("./rawdata/Aarhus_multisite", i), header = TRUE, sep="\t", dec=".", fill  = TRUE, strip.white=TRUE) %>% 
    mutate(Pp = i %>% 
             str_extract(regex("\\d+")))
  
  data_A %<>% rbind(pp_data) 
}


## Combine dataframe -----------------------------------------------------------

DF_all_sites <- rbind(data_A, data_K) %>% 
  filter(BlockType != 11 & BlockType != 12) %>% 
  mutate(Modality = case_when(
    BlockType == 1 | BlockType == 21 ~ "Visual", 
    BlockType == 2 | BlockType == 22 ~ "Auditory")) %>% 
  arrange(Pp)


## First-order performance: % Correct -------------------------------------------------------------

Perf <- DF_all_sites %>% 
  filter(!is.nan(correct)) %>% 
  dcast(Pp ~ Modality, value.var = "correct", mean)


## Raw confidence  -------------------------------------------------------

Raw_conf <- DF_all_sites %>% 
  dcast(Pp ~ Modality, value.var = "answ_2", mean)

write.csv(Raw_conf, "./results/dataset3/raw_confidence3.csv")


## Data preparation for Meta-d' model  --------------------------------------------

DF2 <- DF_all_sites

# exclude pp with performance above 95% and bellow 55% for HHM

Excl <- Perf %>% 
  filter(Auditory > 0.95 | Visual > 0.95)

Excl2 <- Perf %>% 
  filter(Auditory < 0.55 | Visual < 0.55)

# participants than did not do both tasks

Excl3 <- Perf %>% 
  filter(is.nan(Auditory) | is.nan(Visual))


for (i in 1:nrow(Excl2)){
  DF2 %<>%
    filter(Pp != Excl2[i,1])
}

for (i in 1:nrow(Excl3)){
  DF2 %<>%
    filter(Pp != Excl3[i,1])
}

# Get location of the correct response
DF2 %<>% 
  mutate(cor_loc = ifelse(answ_1 == 1 & correct == 1, 1,
                          ifelse(answ_1 == 2 & correct == 1, 2,
                                 ifelse(answ_1 == 1 & correct == 0, 2,
                                        ifelse(answ_1 == 2 & correct == 0, 1, "NO")))))
    
# Prepare data

tasks <- unique(DF2$Modality)

nR_S1 <- list()
nR_S2 <- list()

DF2 %<>% mutate(Count = 1) 

for (t in tasks) {
  
  S1 <- DF2 %>% 
    filter(cor_loc == 1,
           Modality == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(Pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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
    filter(cor_loc == 2,
           Modality == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(Pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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
write.csv2(sub_id, "./results/dataset3/participant_id_H-metad.csv")


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

write.csv(Fit, "./results/dataset3/Hierarchial_Mratio.csv")

# Traceplot
traceplot(H_output)

# MCMC sample
mcmc.sample <- ggs(H_output)
write.csv(mcmc.sample, "./results/dataset3/Hierarchial_mcmc_sample.csv")


## Plot posterior distributions -------------------------------------------------------------------

# Import data
mcmc.sample <- read.csv("./results/dataset3/Hierarchial_mcmc_sample.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
Fit <- read.csv("./results/dataset3/Hierarchial_Mratio.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# Plot mu_logMratio posterior distribution

Auditory <- mcmc.sample %>%
  filter(Parameter == "mu_logMratio[1]") %>% 
  ggplot(aes(exp(value))) +
  geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[1]"]),linetype="dashed", size = 1) +
  geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[1]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[1]"]), yend = 200), colour = "white", size = 2.5) +
  apatheme +
  # xlim(c(0.70, 1.40)) +
  # ylim(c(0, 6500)) +
  ylab("Sample count") +
  xlab(expression(paste(mu, " Mratio")))

Visual <- mcmc.sample %>%
  filter(Parameter == "mu_logMratio[2]") %>% 
  ggplot(aes(exp(value))) +
  geom_histogram(binwidth = 0.01, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = exp(stat_group$mean[stat_group$name == "mu_logMratio[2]"]),linetype="dashed", size = 1) +
  geom_segment(aes(x = exp(HDI$lower[HDI$name == "mu_logMratio[2]"]), y = 200, xend = exp(HDI$upper[HDI$name == "mu_logMratio[2]"]), yend = 200), colour = "white", size = 2.5) +
  apatheme +
  # xlim(c(0.70, 1.40)) +
  # ylim(c(0, 6500)) +
  ylab("Sample count") +
  xlab(expression(paste(mu, " Mratio")))

png(file="./plots/dataset3/MRATIO_2MODALITIES.png", width=10, height=5, units="in", res=300)
plot_grid(Auditory, Visual, labels = c("Auditory", "Visual"), nrow = 1, ncol = 2) 
dev.off()

# Plot rho posterior distribution

mcmc.sample %>%
  filter(Parameter == "rho") %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.03, fill = "blue", colour = "grey", alpha = 0.5) +
  geom_vline(xintercept = stat_group$mean[stat_group$name == "rho"],linetype="dashed", size = 1) +
  geom_segment(aes(x = HDI$lower[HDI$name == "rho"], y = 80, xend = HDI$upper[HDI$name == "rho"], yend = 80), colour = "white", size = 1.5) +
  apatheme +
  xlim(c(-1, 1)) +
  ylab("Sample count") +
  xlab(expression(paste(rho, " value for auditory/visual correlation")))

png(file="./plots/dataset3/MRATIO_COR.png", width=8, height=8, units="in", res=300)



## Individual meta_d function -----------------------------------------------------

# Prepare data

DF3 <- DF_all_sites %>% 
  mutate(Count = 1,
         Task_num = case_when(
           Modality == "Auditory" ~ 1,
           Modality == "Visual" ~ 2))

# get location of the correct response
DF3 %<>% 
  mutate(cor_loc = ifelse(answ_1 == 1 & correct == 1, 1,
                          ifelse(answ_1 == 2 & correct == 1, 2,
                                 ifelse(answ_1 == 1 & correct == 0, 2,
                                        ifelse(answ_1 == 2 & correct == 0, 1, "NO")))))


ntask <- n_distinct(DF3$Modality)

nR_S1_indiv <- list(
  data.frame(),
  data.frame())

nR_S2_indiv <- list(
  data.frame(),
  data.frame())

for (t in 1:(ntask)) {
  
  S1 <- DF3 %>% 
    filter(cor_loc == 1,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(Pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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
    filter(cor_loc == 2,
           Task_num == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(Pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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
  data.frame())

c <- list(
  data.frame(),
  data.frame())

stat <- list(
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
         c = c[[1]][,1],
         Modality = 'Auditory')

Visual <- Perf %>% 
  select(Pp = Pp,
         Perf = Visual) %>% 
  filter(Perf != 'NaN') %>% 
  mutate(metad = stat[[2]][,11],
         d = d[[2]][,1],
         c = c[[2]][,1],
         Modality = 'Visual')


write.csv(Auditory, "./results/dataset3/auditory_multi_individual_metad.csv")
write.csv(Visual, "./results/dataset3/visual_multi_individual_metad.csv")


# Performance exclusion criteria

all_data <- Auditory %>% 
  rbind(Visual) %>% 
  mutate(Mratio = metad/d)

all_data %<>%
  filter(Perf > 0.55 & Perf < 0.95)

write.csv2(all_data, "./results/dataset3/clean_individual_Mratio.csv")


