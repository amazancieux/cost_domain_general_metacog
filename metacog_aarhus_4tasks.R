#####################################


# Analysis script of the domain-general metacognition 4 tasks:
# episodic memory, semantic memory, visual perception, exectuive function
# COST action "The neural architecture of consciousness"
# Aarhus site of data collection 

# Audrey Mazancieux 2021


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
#library(factoextra)


apatheme=theme_bw()+ #theme
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size = 11))


## Import data ----------------------------------------------------------------

DF <- read.csv("aarhus_data_metacognition.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)


## Variables selection -----------------------------------------------------------
DF %<>%
  select(Pp = Subject,
         Left = Left,
         Right = Right,
         CR = CR,
         Resp_EF = respEF.RESP,
         Resp_EM = respEM.RESP,
         Resp_SM = respSM.RESP,
         Resp_VP = respVP.RESP,
         RT_EF = respEF.RT,
         RT_EM = respEM.RT,
         RT_SM = respSM.RT,
         RT_VP = respVP.RT,
         Confidence = confidence.RESP,
         Confidence_RT = confidence.RT,
         Task = Procedure.Trial.)

DF2 <- DF %>% 
  mutate(Resp_EF = as.character(Resp_EF),
        Resp_EM = as.character(Resp_EM),
        Resp_SM = as.character(Resp_SM),
        Resp_VP = as.character(Resp_VP),
        Resp = ifelse(Resp_EF != "", Resp_EF, ifelse(Resp_EM != "", Resp_EM, ifelse(Resp_SM != "", Resp_SM, ifelse(Resp_VP != "", Resp_VP, "NO")))),
        Resp_RT = ifelse(RT_EF != "NA", RT_EF, ifelse(RT_EM != "NA", RT_EM, ifelse(RT_SM != "NA", RT_SM, ifelse(RT_VP != "NA", RT_VP, "NO"))))) %>% 
  filter(Task != "encod" & Task != "encod1") %>%
  mutate(Task = case_when(
    Task == "EM2" | Task == "EM" ~ "EM",
    Task == "SM" ~ "SM",
    Task == "EF" ~ "EF",
    Task == "VP" ~ "VP")) %>% 
  dplyr::select(Pp,
                CR,
                Task,
                Resp,
                Resp_RT,
                Confidence,
                Confidence_RT,
                Left,
                Right) 

DF3 <- DF2


## First-order performance: % Correct -------------------------------------------------------------

Perf <- DF2 %>% 
  mutate(Acc = case_when(
    Resp == "{LEFTARROW}" & CR == "s"~ 1,
    Resp == "{RIGHTARROW}" & CR == "s"~ 0,
    Resp == "{LEFTARROW}" & CR == "l"~ 0,
    Resp == "{RIGHTARROW}" & CR == "l"~ 1)) %>% 
  dcast(Pp ~ Task, value.var = "Acc", mean)


## Exclusion criteria ----------------------------------------------------


# Exclude pp with performance above 95% and bellow 55% for HHM

Excl <- Perf %>% 
  filter(EF > 0.95 | EM > 0.95 | SM > 0.95 | VP > 0.95)

Excl2 <- Perf %>% 
  filter(EF < 0.55 | EM < 0.55 | SM < 0.55 | VP < 0.55)


for (i in 1:nrow(Excl)){
  DF2 %<>%
    filter(Pp != Excl[i,1])
}

for (i in 1:nrow(Excl2)){
  DF2 %<>%
    filter(Pp != Excl2[i,1])
}


## Data preparation for Meta-d' model  --------------------------------------------

DF2 %<>%
  mutate(Count = 1,
         Task_num = case_when(
           Task == "EM" ~ 1,
           Task == "VP" ~ 2,
           Task == "SM" ~ 3,
           Task == "EF" ~ 4))

ntask <- n_distinct(DF2$Task)

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
    filter(CR == "s",
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == "{LEFTARROW}" ~ "R1",
      Resp == "{RIGHTARROW}" ~ "R2")) %>% 
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
    filter(CR == "l",
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == "{LEFTARROW}" ~ "R1",
      Resp == "{RIGHTARROW}" ~ "R2")) %>% 
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

write.csv(Fit, "/results/dataset1/Hierarchial_Mratio.csv")

# Traceplot
traceplot(H_output)

# MCMC sample
mcmc.sample <- ggs(H_output)
write.csv(mcmc.sample, "/results/dataset1/Hierarchial_mcmc_sample.csv")



## Plot posterior distributions -------------------------------------------------------------------

# Apatheme for plot
apatheme=theme_bw()+ #theme
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text = element_text(size = 15),
        axis.title = element_text(size = 12))

# Plot mu_logMratio posterior distribution

EM <- mcmc.sample %>%
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

VP <- mcmc.sample %>%
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

SM <- mcmc.sample %>%
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

EF <- mcmc.sample %>%
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

png(file="./plots/dataset1/MRATIO_4TASK.png", width=10, height=8, units="in", res=300)
plot_grid(EM, VP, SM, EF, labels = c("Episodic memory", "Visual perception", "Semantic memory", "Executive functioning"), nrow = 2, ncol = 2) 
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
  xlab(expression(paste(rho, " value for EM/VP correlation")))

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
  xlab(expression(paste(rho, " value for EM/SM correlation")))

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
  xlab(expression(paste(rho, " value for EM/EF correlation")))

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
  xlab(expression(paste(rho, " value for VP/SM correlation")))

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
  xlab(expression(paste(rho, " value for VP/EF correlation")))

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
  xlab(expression(paste(rho, " value for SM/EF correlation")))

png(file="./plots/dataset1/MRATIO_6COR.png", width=10, height=8, units="in", res=300)
plot_grid(Rho1, Rho2, Rho3, NULL, Rho4, Rho5, NULL, NULL, Rho6, labels = c("A", "B", "C", "", "D", "E", "", "", "F"), nrow = 3, ncol = 3)
dev.off()


## Plot individual Mratio 

# Create individual Mratio data frame
Mratio_indiv <- stat %>% 
  filter(grepl("^M", name)) %>% 
  mutate(task = ifelse(grepl("1]", name), "EM",
                       ifelse(grepl("2]", name), "VP",
                              ifelse(grepl("3]", name), "SM","EF"))),
         pp = name %>%
           str_extract(regex("\\d+(?=,)")) %>% 
           as.integer()) %>% 
  dcast(pp ~ task, value.var = "mean")

# Create plot
Cor1 <- Mratio_indiv %>% 
  ggplot(aes(x = VP, y = EM)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

Cor2 <- Mratio_indiv %>% 
  filter(task == "EM" | task == "SM") %>% 
  ggplot(aes(x = stat$mean[grepl(",3]", stat$name)], y = stat$mean[grepl(",1]", stat$name)])) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

Cor3 <- Mratio_indiv %>% 
  filter(task == "EM" | task == "EF") %>% 
  ggplot(aes(x = stat$mean[grepl(",4]", stat$name)], y = stat$mean[grepl(",1]", stat$name)])) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

Cor4 <- Mratio_indiv %>% 
  filter(task == "VP" | task == "SM") %>% 
  ggplot(aes(x = stat$mean[grepl(",3]", stat$name)], y = stat$mean[grepl(",2]", stat$name)])) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

Cor5 <- Mratio_indiv %>% 
  filter(task == "VP" | task == "EF") %>% 
  ggplot(aes(x = stat$mean[grepl(",4]", stat$name)], y = stat$mean[grepl(",2]", stat$name)])) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

Cor6 <- Mratio_indiv %>% 
  filter(task == "SM" | task == "EF") %>% 
  ggplot(aes(x = stat$mean[grepl(",4]", stat$name)], y = stat$mean[grepl(",3]", stat$name)])) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", size = 1) +
  apatheme +
  ylim(c(0.2, 2)) +
  xlim(c(0.2, 2)) +
  ylab(expression(paste(mu, " Mratio"))) +
  xlab(expression(paste(mu, " Mratio")))

plot_grid(Cor1, Cor2, Cor3, NULL, Cor4, Cor5, NULL, NULL, Cor6, 
          labels = c("A", "B", "C", "", "D", "E", "", "", "F"), nrow = 3, ncol = 3)



## Compute HDI of difference for all tasks ---------------------------------

Diff <- mcmc.sample %>%
  filter(Parameter == "mu_logMratio[1]" 
         | Parameter == "mu_logMratio[2]" 
         | Parameter == "mu_logMratio[3]"
         | Parameter == "mu_logMratio[4]") %>% 
  dcast(Iteration ~ Parameter, value.var = "value", mean) %>% 
  mutate(EMVP = `mu_logMratio[1]`-`mu_logMratio[2]`,
         EMSM = `mu_logMratio[1]`-`mu_logMratio[3]`,
         EMEF = `mu_logMratio[1]`-`mu_logMratio[4]`,
         SMVP = `mu_logMratio[3]`-`mu_logMratio[2]`,
         EFVP = `mu_logMratio[4]`-`mu_logMratio[2]`,
         SMEF = `mu_logMratio[3]`-`mu_logMratio[4]`)

HPDinterval(as.mcmc(Diff), prob = 0.95)

mean(Diff$EMVP)
mean(Diff$EMSM)
mean(Diff$EMEF)
mean(Diff$SMVP)
mean(Diff$EFVP)
mean(Diff$SMEF)



## Individual meta_d function -----------------------------------------------------

# Prepare data

DF3 %<>%
  mutate(Count = 1,
         Task_num = case_when(
           Task == "EM" ~ 1,
           Task == "VP" ~ 2,
           Task == "SM" ~ 3,
           Task == "EF" ~ 4))

ntask <- n_distinct(DF2$Task)

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
    filter(CR == "s",
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == "{LEFTARROW}" ~ "R1",
      Resp == "{RIGHTARROW}" ~ "R2")) %>% 
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
    filter(CR == "l",
           Task_num == t) %>% 
    mutate(Resp = case_when(
      Resp == "{LEFTARROW}" ~ "R1",
      Resp == "{RIGHTARROW}" ~ "R2")) %>% 
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

write.csv(stat, "results/dataset1/output_fit_individual_metad.csv")


## Calculate ratio and plot data ------------------------------------------------------------

# First-order performance 

Pp <- unique(DF3$Pp)
d_prime <- data.frame(d) 
names(d_prime) <- c("EM", "VP", "SM", "EF") 
d_prime %<>% 
  mutate(Pp = Pp) %>% 
  gather(Task, d, -Pp)

# Second-order performance 

metad <- data.frame(EM = stat[[1]][,11]) %>% 
  cbind(VP = stat[[2]][,11]) %>% 
  cbind(SM = stat[[3]][,11]) %>% 
  cbind(EF = stat[[4]][,11]) %>% 
  gather(Task, metad) %>% 
  cbind(Pp = Pp)

# Plot_metad <- metad %>% 
#   group_by(Task) %>% 
#   summarise(VD = mean(score),
#             sd = sd(score),
#             se = sd/sqrt(nsubj),
#             CI = se * qt(.975, n() - 1)) %>% 
#   ggplot(aes(x = Task, y = VD, color = Task)) +
#   geom_point(data = metad, 
#              aes(x = Task, y = score, color = Task), 
#              position = position_jitterdodge(0.3), 
#              size = 1, shape = 1) +
#   geom_boxplot(data = metad,
#                aes(x = Task, y = score, fill = Task),
#                outlier.shape = NA,
#                alpha = .5, width = .2,
#                position = position_dodge(0.7)) +
#   geom_point(size = 3, position = position_dodge(0.2)) +
#   geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.2))+
#   scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
#   scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
#   apatheme +
#   xlab("Task") +
#   ylab("meta-d' value")


# Metacognitive efficiency

Mratio <- merge(d_prime, metad, by=c('Pp','Task'))
Mratio %<>% 
  mutate(Mratio = metad/d)

write.csv(Mratio, "Individual_Mratio.csv")

# Plot_Mratio <- Mratio %>% 
#   group_by(Task) %>% 
#   summarise(VD = mean(Mratio),
#             sd = sd(Mratio),
#             se = sd/sqrt(nsubj),
#             CI = se * qt(.975, n() - 1)) %>% 
#   ggplot(aes(x = Task, y = VD, color = Task)) +
#   geom_point(data = Mratio, 
#              aes(x = Task, y = Mratio, color = Task), 
#              position = position_jitterdodge(0.3), 
#              size = 1, shape = 1) +
#   geom_boxplot(data = Mratio,
#                aes(x = Task, y = Mratio, fill = Task),
#                outlier.shape = NA,
#                alpha = .5, width = .2,
#                position = position_dodge(0.7)) +
#   geom_point(size = 3, position = position_dodge(0.1)) +
#   geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.1))+
#   scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
#   scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
#   apatheme +
#   xlab("Task") +
#   ylab("Mratio value")
# 
# 
# png(file="./INDIV_4TASK.png", width=10, height=8, units="in", res=300)  
# plot_grid(Plot_d, Plot_metad, Plot_Mratio, labels = c("A", "B", "C"), nrow = 2, ncol = 2)
# dev.off()


# Exclude pp with first-order perf > 0.95

Excl_EM <- Perf %>% 
  filter(EM > 0.95 | EM < 0.55)

Excl_SM <- Perf %>% 
  filter(SM > 0.95 | SM < 0.55)

Excl_VP <- Perf %>% 
  filter(VP > 0.95 | VP < 0.55)

Excl_EF <- Perf %>% 
  filter(EF > 0.95 | EF < 0.55)


# Plot data

Plot_d <- d_prime %>%
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = d_prime,
             aes(x = Task, y = score, color = Task),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = d_prime,
               aes(x = Task, y = score, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7)) +
  geom_point(size = 3, position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.80, position = position_dodge(0.2))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  apatheme +
  xlab("Task") +
  ylab("d' value")




## Cross-task correlations

# EM and VP
corr_EMVP <- Mratio

for (i in 1:nrow(Excl_EM)){
  corr_EMVP %<>%
    filter(Pp != Excl_EM[i,1])
}

cor1 <- tidy(cor.test(corr_EMVP$Mratio[corr_EMVP$Task == "EM"], corr_EMVP$Mratio[corr_EMVP$Task == "VP"]))

Plot_cor1 <- corr_EMVP %>% 
  filter(Task == "EM" | Task == "VP") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = EM, y = VP)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the EM task") +
  ylab("Mratio for the VP task") +
  ggtitle(paste("Correlation is", round(cor1$estimate, 2),"[", round(cor1$conf.low, 3),";",round(cor1$conf.high, 3), "]"))

# EM and SM
corr_EMSM <- Mratio

for (i in 1:nrow(Excl_EM)){
  corr_EMSM %<>%
    filter(Pp != Excl_EM[i,1])
}

for (i in 1:nrow(Excl_SM)){
  corr_EMSM %<>%
    filter(Pp != Excl_SM[i,1])
}

cor2 <- tidy(cor.test(corr_EMSM$Mratio[corr_EMSM$Task == "EM"], corr_EMSM$Mratio[corr_EMSM$Task == "SM"]))

Plot_cor2 <- corr_EMSM %>% 
  filter(Task == "EM" | Task == "SM") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = EM, y = SM)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the EM task") +
  ylab("Mratio for the SM task") +
  ggtitle(paste("Correlation is", round(cor2$estimate, 2),"[", round(cor2$conf.low, 3),";",round(cor2$conf.high, 3), "]"))

# EM and EF
corr_EMEF <- Mratio

for (i in 1:nrow(Excl_EM)){
  corr_EMEF %<>%
    filter(Pp != Excl_EM[i,1])
}

for (i in 1:nrow(Excl_EF)){
  corr_EMEF %<>%
    filter(Pp != Excl_EF[i,1])
}

cor3 <- tidy(cor.test(corr_EMEF$Mratio[corr_EMEF$Task == "EM"], corr_EMEF$Mratio[corr_EMEF$Task == "EF"]))

Plot_cor3 <- corr_EMEF %>% 
  filter(Task == "EM" | Task == "EF") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = EM, y = EF)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the EM task") +
  ylab("Mratio for the EF task") +
  ggtitle(paste("Correlation is", round(cor3$estimate, 2),"[", round(cor3$conf.low, 3),";",round(cor3$conf.high, 3), "]"))

# VP and SM
corr_VPSM <- Mratio

for (i in 1:nrow(Excl_SM)){
  corr_VPSM %<>%
    filter(Pp != Excl_SM[i,1])
}

cor4 <- tidy(cor.test(corr_VPSM$Mratio[corr_VPSM$Task == "VP"], corr_VPSM$Mratio[corr_VPSM$Task == "SM"]))

Plot_cor4 <- corr_VPSM %>% 
  filter(Task == "VP" | Task == "SM") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = VP, y = SM)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the VP task") +
  ylab("Mratio for the SM task") +
  ggtitle(paste("Correlation is", round(cor4$estimate, 2),"[",round(cor4$conf.low, 3),";",round(cor4$conf.high, 3), "]"))

# VP and EF
corr_VPEF <- Mratio

for (i in 1:nrow(Excl_EF)){
  corr_VPEF %<>%
    filter(Pp != Excl_EF[i,1])
}

cor5 <- tidy(cor.test(corr_VPEF$Mratio[corr_VPEF$Task == "VP"], corr_VPEF$Mratio[corr_VPEF$Task == "EF"]))

Plot_cor5 <- corr_VPEF %>% 
  filter(Task == "VP" | Task == "EF") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = VP, y = EF)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the VP task") +
  ylab("Mratio for the EF task") +
  ggtitle(paste("Correlation is", round(cor5$estimate, 2),"[",round(cor5$conf.low, 3),";",round(cor5$conf.high, 3), "]"))


# SM and EF
corr_SMEF <- Mratio

for (i in 1:nrow(Excl_EF)){
  corr_SMEF %<>%
    filter(Pp != Excl_EF[i,1])
}

for (i in 1:nrow(Excl_SM)){
  corr_SMEF %<>%
    filter(Pp != Excl_SM[i,1])
}

cor6 <- tidy(cor.test(corr_SMEF$Mratio[corr_SMEF$Task == "SM"], corr_SMEF$Mratio[corr_SMEF$Task == "EF"]))

Plot_cor6 <- corr_SMEF %>% 
  filter(Task == "SM" | Task == "EF") %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  ggplot(aes(x = SM, y = EF)) +
  geom_point(shape = 1, colour = "#003366") +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("Mratio for the SM task") +
  ylab("Mratio for the EF task") +
  ggtitle(paste("Correlation is", round(cor6$estimate, 2),"[",round(cor6$conf.low, 3),";", round(cor6$conf.high, 3), "]"))


png(file="./INDIV_COR6.png", width=11, height=8, units="in", res=300)  
plot_grid(Plot_cor1, Plot_cor2, Plot_cor3,NULL,Plot_cor4, Plot_cor5,NULL,NULL,Plot_cor6, nrow = 3, ncol = 3)
dev.off()



## DGI ---------------------------------------------------------------------

DGI <- Mratio %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  mutate(DGI = ((abs(EF-EM))+(abs(EF-VP))+(abs(EF-SM))+(abs(EM-VP))+(abs(EM-SM))+(abs(SM-VP)))/6) 


## PCA  --------------------------------------------------------------

# Exclude paritipants according to performance

DF_PCA <- Mratio
for (i in 1:nrow(Excl)){
  DF_PCA %<>%
    filter(Pp != Excl[i,1])s
}

for (i in 1:nrow(Excl2)){
  DF_PCA %<>%
    filter(Pp != Excl2[i,1])
}

# Run PCA

DF_PCA %<>% 
  select(Pp, Task, Mratio) %>% 
  spread(key = Task, value = Mratio) %>% 
  select(-Pp)


PCA_Mratio <- princomp(DF_PCA, cor = TRUE, scores = TRUE)
summary(PCA_Mratio)


