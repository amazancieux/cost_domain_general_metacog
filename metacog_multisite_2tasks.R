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


apatheme=theme_bw()+ #theme
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size = 11))


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

DF_all_sites <- rbind(data_A, data_K)



## First-order performance: % Correct -------------------------------------------------------------

Perf <- data_K %>% 
  dcast(Pp ~ BlockType, value.var = "correct", mean)


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

for (i in 1:nrow(To_excl)){
  DF2 %<>%
    filter(Pp != To_excl[i,1])
}

for (i in 1:nrow(To_excl)){
  DF2 %<>%
    filter(Pp != To_excl[i,1])
}

# Prepare data

ntask <- n_distinct(data$BlockType)

nR_S1 <- list(
  data.frame(),
  data.frame())

nR_S2 <- list(
  data.frame(),
  data.frame())

data %<>% mutate(Count = 1) 

for (t in 1:(ntask)) {
  
  S1 <- data %>% 
    filter(Direction == 1,
           BlockType == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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
  
  S2 <- data %>% 
    filter(Direction == 2,
           BlockType == t) %>% 
    mutate(Resp = case_when(
      answ_1 == 1 ~ "R1",
      answ_1 == 2 ~ "R2")) %>% 
    dcast(pp ~ answ_2 + Resp, value.var = "Count", sum) %>% 
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

# Apatheme for plot
apatheme=theme_bw()+ #theme
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size = 15),
        axis.title = element_text(size = 12))

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
  filter(Perf > 0.55 & Perf < 0.95)

# First-order performance 

Plot_d <- all_data %>%
  group_by(Modality) %>%
  summarise(VD = mean(d),
            sd = sd(d),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = all_data,
             aes(x = Modality, y = d, color = Modality),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = all_data,
               aes(x = Modality, y = d, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.2))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  apatheme +
  xlab("Modality") +
  ylab("d' value")

# Second-order performance 

Plot_metad <- all_data %>%
  group_by(Modality) %>%
  summarise(VD = mean(metad),
            sd = sd(metad),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = all_data,
             aes(x = Modality, y = metad, color = Modality),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = all_data,
               aes(x = Modality, y = metad, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.2))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  apatheme +
  xlab("Task") +
  ylab("meta-d' value")


# Metacognitive efficiency

Plot_Mratio <- all_data %>%
  group_by(Modality) %>%
  summarise(VD = mean(Mratio),
            sd = sd(Mratio),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = all_data,
             aes(x = Modality, y = Mratio, color = Modality),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = all_data,
               aes(x = Modality, y = Mratio, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.1))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  apatheme +
  xlab("Task") +
  ylab("Mratio value")


png(file="./plots/dataset2/INDIV_4MODALITIES.png", width=10, height=8, units="in", res=300)
plot_grid(Plot_d, Plot_metad, Plot_Mratio, labels = c("A", "B", "C"), nrow = 2, ncol = 2)
dev.off()



## Cross-task correlations

# auditory and visual 
corr_AV <- all_data %>% 
  filter(Modality == 'Auditory' | Modality == 'Visual') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_AV <- na.omit(corr_AV)

cor1 <- tidy(cor.test(corr_AV$Auditory, corr_AV$Visual))

Plot_cor1 <- corr_AV %>% 
  ggplot(aes(x = Auditory, y = Visual)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the auditory modality") +
  ylab("Mratio for the visual modality") +
  ggtitle(paste("Correlation is", round(cor1$estimate, 2),"[", round(cor1$conf.low, 3),";",round(cor1$conf.high, 3), "]"))


# auditory and tactile
corr_AT <- all_data %>% 
  filter(Modality == 'Auditory' | Modality == 'Tactile') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_AT <- na.omit(corr_AT)

cor2 <- tidy(cor.test(corr_AT$Auditory, corr_AT$Tactile))

Plot_cor2 <- corr_AT %>% 
  ggplot(aes(x = Auditory, y = Tactile)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the auditory modality") +
  ylab("Mratio for the tactile modality") +
  ggtitle(paste("Correlation is", round(cor2$estimate, 2),"[", round(cor2$conf.low, 3),";",round(cor2$conf.high, 3), "]"))


# auditory and pain
corr_AP <- all_data %>% 
  filter(Modality == 'Auditory' | Modality == 'Pain') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_AP <- na.omit(corr_AP)

cor3 <- tidy(cor.test(corr_AP$Auditory, corr_AP$Pain))

Plot_cor3 <- corr_AP %>% 
  ggplot(aes(x = Auditory, y = Pain)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the auditory modality") +
  ylab("Mratio for the pain modality") +
  ggtitle(paste("Correlation is", round(cor3$estimate, 2),"[", round(cor3$conf.low, 3),";",round(cor3$conf.high, 3), "]"))


# visual and tactile
corr_VT <- all_data %>% 
  filter(Modality == 'Visual' | Modality == 'Tactile') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_VT <- na.omit(corr_VT)

cor4 <- tidy(cor.test(corr_VT$Visual, corr_VT$Tactile))

Plot_cor4 <- corr_VT %>% 
  ggplot(aes(x = Visual, y = Tactile)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the visual modality") +
  ylab("Mratio for the tactile modality") +
  ggtitle(paste("Correlation is", round(cor4$estimate, 2),"[", round(cor4$conf.low, 3),";",round(cor4$conf.high, 3), "]"))


# visual and pain
corr_VP <- all_data %>% 
  filter(Modality == 'Visual' | Modality == 'Pain') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_VP <- na.omit(corr_VP)

cor5 <- tidy(cor.test(corr_VP$Visual, corr_VP$Pain))

Plot_cor5 <- corr_VP %>% 
  ggplot(aes(x = Visual, y = Pain)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the visual modality") +
  ylab("Mratio for the pain modality") +
  ggtitle(paste("Correlation is", round(cor5$estimate, 2),"[", round(cor5$conf.low, 3),";",round(cor5$conf.high, 3), "]"))


# tactivle and pain
corr_TP <- all_data %>% 
  filter(Modality == 'Tactile' | Modality == 'Pain') %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio') 

corr_TP <- na.omit(corr_TP)

cor6 <- tidy(cor.test(corr_TP$Tactile, corr_TP$Pain))

Plot_cor6 <- corr_TP %>% 
  ggplot(aes(x = Tactile, y = Pain)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the visual modality") +
  ylab("Mratio for the pain modality") +
  ggtitle(paste("Correlation is", round(cor6$estimate, 2),"[", round(cor6$conf.low, 3),";",round(cor6$conf.high, 3), "]"))



png(file="./plots/dataset2/INDIV_COR6.png", width=11, height=8, units="in", res=300)  
plot_grid(Plot_cor1, Plot_cor2, Plot_cor3,NULL,Plot_cor4, Plot_cor5,NULL,NULL,Plot_cor6, nrow = 3, ncol = 3)
dev.off()






