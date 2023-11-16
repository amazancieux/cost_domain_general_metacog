#####################################


# Statistics and plots for first-order performance and
# metacognitive bias, and individuals M-ratio as well as
# CFA and SEM for the 3 datasets
# COST action "The neural architecture of consciousness"

# Audrey Mazancieux 2023


#####################################


## Packages ----------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)
library(broom)
library(cowplot)
library(lattice)
library(lavaan)
library(lavaanPlot)
library(effectsize)
library(AICcmodavg)


## Import preprocessed data -----------------------------------------------------------

## individual meta-d'
dataset1 <- read.csv("./results/dataset1/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset2 <- read.csv("./results/dataset2/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset3 <- read.csv("./results/dataset3/clean_individual_Mratio.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)

# Raw confidence
conf1 <- read.csv("./results/dataset1/raw_confidence1.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
conf2 <- read.csv("./results/dataset2/raw_confidence2.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
conf3 <- read.csv("./results/dataset3/raw_confidence3.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# Proportion correct type-1 
perf1 <- read.csv("./results/dataset1/proportion_correct1.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
perf2 <- read.csv("./results/dataset2/proportion_correct2.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
perf3 <- read.csv("./results/dataset3/proportion_correct3.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)

# reaction time
rt1 <- read.csv("./results/dataset1/reaction_time1.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
rt2 <- read.csv("./results/dataset2/reaction_time2.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
rt3 <- read.csv("./results/dataset3/reaction_time3.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)


# after screening data - outlier exclusion on Mratio for dataset1 > 3
dataset1 %<>% 
  filter(Mratio < 3)

# after screening data - outlier exclusion on Mratio for dataset2 > 2.8
dataset2 %<>% 
  filter(Mratio < 2.8)


## First-order performance all datasets (stats) ------------------------------------

## Dataset 1

first_order1 <- dataset1 %>% 
  dcast(Pp ~ Task, value.var = "d") %>% 
  mutate(D1 = EM - VP,
         D2 = EM - SM,
         D3 = EM - EF,
         D4 = VP - SM,
         D5 = VP - EF,
         D6 = SM - EF) 

D1 <- lm(D1 ~ 1, first_order1)
D2 <- lm(D2 ~ 1, first_order1)
D3 <- lm(D3 ~ 1, first_order1)
D4 <- lm(D4 ~ 1, first_order1)
D5 <- lm(D5 ~ 1, first_order1)
D6 <- lm(D6 ~ 1, first_order1)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(first_order1$EM, na.rm = TRUE)
mean(first_order1$VP, na.rm = TRUE)
sd(first_order1$EM, na.rm = TRUE)
sd(first_order1$VP, na.rm = TRUE)

summary(D2)
mean(first_order1$SM, na.rm = TRUE)
sd(first_order1$SM, na.rm = TRUE)

summary(D3)
mean(first_order1$EF, na.rm = TRUE)
sd(first_order1$EF, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)

# correlations
cor.test(first_order1$EM, first_order1$VP)
cor.test(first_order1$EM, first_order1$SM)
cor.test(first_order1$EM, first_order1$EF)
cor.test(first_order1$VP, first_order1$SM)
cor.test(first_order1$VP, first_order1$EF)
cor.test(first_order1$SM, first_order1$EF)


## Dataset 2

first_order2 <- dataset2 %>% 
  dcast(Pp ~ Modality, value.var = "d") %>% 
  mutate(D1 = Auditory - Visual,
         D2 = Auditory - Tactile,
         D3 = Auditory - Pain,
         D4 = Visual - Tactile,
         D5 = Visual - Pain,
         D6 = Tactile - Pain) 

D1 <- lm(D1 ~ 1, first_order2)
D2 <- lm(D2 ~ 1, first_order2)
D3 <- lm(D3 ~ 1, first_order2)
D4 <- lm(D4 ~ 1, first_order2)
D5 <- lm(D5 ~ 1, first_order2)
D6 <- lm(D6 ~ 1, first_order2)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(first_order2$Auditory, na.rm = TRUE)
mean(first_order2$Visual, na.rm = TRUE)
sd(first_order2$Auditory, na.rm = TRUE)
sd(first_order2$Visual, na.rm = TRUE)

summary(D2)
mean(first_order2$Tactile, na.rm = TRUE)
sd(first_order2$Tactile, na.rm = TRUE)

summary(D3)
mean(first_order2$Pain, na.rm = TRUE)
sd(first_order2$Pain, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)

# correlations
cor.test(first_order2$Auditory, first_order2$Visual)
cor.test(first_order2$Auditory, first_order2$Tactile)
cor.test(first_order2$Auditory, first_order2$Pain)
cor.test(first_order2$Visual, first_order2$Tactile)
cor.test(first_order2$Visual, first_order2$Pain)
cor.test(first_order2$Tactile, first_order2$Pain)


## Dataset 3

first_order3 <- dataset3 %>% 
  dcast(Pp ~ Modality, value.var = "d") %>% 
  mutate(D1 = Auditory - Visual) %>% 
  na.omit(first_order3)

D1 <- lm(D1 ~ 1, first_order3)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
cor.test(first_order3$Auditory, first_order3$Visual)
mean(first_order3$Auditory)
mean(first_order3$Visual)
sd(first_order3$Auditory)
sd(first_order3$Visual)



## Metacognitive bias all dataset (stats) --------------------------------------

## Dataset 1

conf1 %<>%
  select(-X) %>% 
  gather(Task, Conf,
         -Pp)

# exclude same participants as for d'

Excl_EM <- first_order1 %>% 
  filter(is.na(EM))
conf_EM <- conf1 %>% 
  filter(Task == 'EM')

for (i in 1:nrow(Excl_EM)){
  conf_EM %<>%
    filter(Pp != Excl_EM[i,1])
}

Excl_VP <- first_order1 %>% 
  filter(is.na(VP))
conf_VP <- conf1 %>% 
  filter(Task == 'VP')

Excl_SM <- first_order1 %>% 
  filter(is.na(SM))
conf_SM <- conf1 %>% 
  filter(Task == 'SM')

for (i in 1:nrow(Excl_SM)){
  conf_SM %<>%
    filter(Pp != Excl_SM[i,1])
}

Excl_EF <- first_order1 %>% 
  filter(is.na(EF))
conf_EF <- conf1 %>% 
  filter(Task == 'EF')

for (i in 1:nrow(Excl_EF)){
  conf_EF %<>%
    filter(Pp != Excl_EF[i,1])
}

conf1_clean <- conf_EM %>% 
  rbind(conf_VP,
        conf_SM,
        conf_EF) 

perf1 %<>%
  select(-X) %>% 
  gather(Task, Perf,
         -Pp)

bias1 <- merge(conf1_clean, perf1, by=c("Pp", "Task"))

# statistics

bias1 %<>%  
  mutate(bias = (Conf/100) - Perf) %>% 
  dcast(Pp ~ Task, value.var = "bias") %>% 
  mutate(D1 = EM - VP,
         D2 = EM - SM,
         D3 = EM - EF,
         D4 = VP - SM,
         D5 = VP - EF,
         D6 = SM - EF) 

D1 <- lm(D1 ~ 1, bias1)
D2 <- lm(D2 ~ 1, bias1)
D3 <- lm(D3 ~ 1, bias1)
D4 <- lm(D4 ~ 1, bias1)
D5 <- lm(D5 ~ 1, bias1)
D6 <- lm(D6 ~ 1, bias1)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(bias1$EM, na.rm = TRUE)
mean(bias1$VP, na.rm = TRUE)
sd(bias1$EM, na.rm = TRUE)
sd(bias1$VP, na.rm = TRUE)

summary(D2)
mean(bias1$SM, na.rm = TRUE)
sd(bias1$SM, na.rm = TRUE)

summary(D3)
mean(bias1$EF, na.rm = TRUE)
sd(bias1$EF, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)

# correlations
cor.test(bias1$EM, bias1$VP)
cor.test(bias1$EM, bias1$SM)
cor.test(bias1$EM, bias1$EF)
cor.test(bias1$VP, bias1$SM)
cor.test(bias1$VP, bias1$EF)
cor.test(bias1$SM, bias1$EF)


## Dataset 2

conf2 %<>%
  select(-X) %>% 
  gather(Task, Conf,
         -Pp)

# exclude same participants as for d'

Excl_Auditory <- first_order2 %>% 
  filter(is.na(Auditory))
conf_Auditory <- conf2 %>% 
  filter(Task == 'Auditory')

for (i in 1:nrow(Excl_Auditory)){
  conf_Auditory %<>%
    filter(Pp != Excl_Auditory[i,1])
}

Excl_Visual <- first_order2 %>% 
  filter(is.na(Visual))
conf_Visual <- conf2 %>% 
  filter(Task == 'Visual')
for (i in 1:nrow(Excl_Visual)){
  conf_Visual %<>%
    filter(Pp != Excl_Visual[i,1])
}

Excl_Tactile <- first_order2 %>% 
  filter(is.na(Tactile))
conf_Tactile <- conf2 %>% 
  filter(Task == 'Tactile')

for (i in 1:nrow(Excl_Tactile)){
  conf_Tactile %<>%
    filter(Pp != Excl_Tactile[i,1])
}

Excl_Pain <- first_order2 %>% 
  filter(is.na(Pain))
conf_Pain <- conf2 %>% 
  filter(Task == 'Pain')

for (i in 1:nrow(Excl_Pain)){
  conf_Pain %<>%
    filter(Pp != Excl_Pain[i,1])
}

conf2_clean <- conf_Auditory %>% 
  rbind(conf_Visual,
        conf_Tactile,
        conf_Pain) 


# statistics

perf2 %<>%
  select(-X) %>% 
  gather(Task, Perf,
         -Pp)

bias2 <- merge(conf2_clean, perf2, by=c("Pp", "Task"))

bias2 %<>%  
  mutate(bias = (Conf/100) - Perf) %>% 
  dcast(Pp ~ Task, value.var = "bias") %>% 
  mutate(D1 = Auditory - Visual,
         D2 = Auditory - Tactile,
         D3 = Auditory - Pain,
         D4 = Visual - Tactile,
         D5 = Visual - Pain,
         D6 = Tactile - Pain) 

D1 <- lm(D1 ~ 1, bias2)
D2 <- lm(D2 ~ 1, bias2)
D3 <- lm(D3 ~ 1, bias2)
D4 <- lm(D4 ~ 1, bias2)
D5 <- lm(D5 ~ 1, bias2)
D6 <- lm(D6 ~ 1, bias2)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(bias2$Auditory, na.rm = TRUE)
mean(bias2$Visual, na.rm = TRUE)
sd(bias2$Auditory, na.rm = TRUE)
sd(bias2$Visual, na.rm = TRUE)

summary(D2)
mean(bias2$Tactile, na.rm = TRUE)
sd(bias2$Tactile, na.rm = TRUE)

summary(D3)
mean(bias2$Pain, na.rm = TRUE)
sd(bias2$Pain, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)

# correlations
cor.test(bias2$Auditory, bias2$Visual)
cor.test(bias2$Auditory, bias2$Tactile)
cor.test(bias2$Auditory, bias2$Pain)
cor.test(bias2$Visual, bias2$Tactile)
cor.test(bias2$Visual, bias2$Pain)
cor.test(bias2$Tactile, bias2$Pain)


## Dataset 3

# exclude same participants as for d'

conf3_clean <- merge(first_order3, conf3, by = 'Pp')
conf3_clean %<>%
  select(Pp, 
         Auditory = Auditory.y,
         Visual = Visual.y) %>% 
  gather(Task, Conf,
         -Pp)

perf3 %<>%
  select(-X) %>% 
  gather(Task, Perf,
         -Pp)
bias3 <- merge(conf3_clean, perf3, by=c("Pp", "Task"))

bias3 %<>%  
  mutate(bias = (Conf/100) - Perf) %>% 
  dcast(Pp ~ Task, value.var = "bias") %>% 
  mutate(D1 = Auditory - Visual)


# statistics

D1 <- lm(D1 ~ 1, bias3)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
cor.test(bias3$Auditory, bias3$Visual)
mean(bias3$Auditory, na.rm = TRUE)
mean(bias3$Visual, na.rm = TRUE)
sd(bias3$Auditory, na.rm = TRUE)
sd(bias3$Visual, na.rm = TRUE)



## Individual Mratio all datasets (stast) ------------------------------------

## Dataset 1

Mratio1 <- dataset1 %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  mutate(D1 = EM - VP,
         D2 = EM - SM,
         D3 = EM - EF,
         D4 = VP - SM,
         D5 = VP - EF,
         D6 = SM - EF) 

D1 <- lm(D1 ~ 1, Mratio1)
D2 <- lm(D2 ~ 1, Mratio1)
D3 <- lm(D3 ~ 1, Mratio1)
D4 <- lm(D4 ~ 1, Mratio1)
D5 <- lm(D5 ~ 1, Mratio1)
D6 <- lm(D6 ~ 1, Mratio1)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(Mratio1$EM, na.rm = TRUE)
mean(Mratio1$VP, na.rm = TRUE)
sd(Mratio1$EM, na.rm = TRUE)
sd(Mratio1$VP, na.rm = TRUE)

summary(D2)
mean(Mratio1$SM, na.rm = TRUE)
sd(Mratio1$SM, na.rm = TRUE)

summary(D3)
mean(Mratio1$EF, na.rm = TRUE)
sd(Mratio1$EF, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)

# correlations
cor.test(Mratio1$EM, Mratio1$VP)
cor.test(Mratio1$EM, Mratio1$SM)
cor.test(Mratio1$EM, Mratio1$EF)
cor.test(Mratio1$VP, Mratio1$SM)
cor.test(Mratio1$VP, Mratio1$EF)
cor.test(Mratio1$SM, Mratio1$EF)

## Dataset 2

Mratio2 <- dataset2 %>% 
  dcast(Pp ~ Modality, value.var = "Mratio") %>% 
  mutate(D1 = Auditory - Visual,
         D2 = Auditory - Tactile,
         D3 = Auditory - Pain,
         D4 = Visual - Tactile,
         D5 = Visual - Pain,
         D6 = Tactile - Pain) 

D1 <- lm(D1 ~ 1, Mratio2)
D2 <- lm(D2 ~ 1, Mratio2)
D3 <- lm(D3 ~ 1, Mratio2)
D4 <- lm(D4 ~ 1, Mratio2)
D5 <- lm(D5 ~ 1, Mratio2)
D6 <- lm(D6 ~ 1, Mratio2)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
mean(Mratio2$Auditory, na.rm = TRUE)
mean(Mratio2$Visual, na.rm = TRUE)
sd(Mratio2$Auditory, na.rm = TRUE)
sd(Mratio2$Visual, na.rm = TRUE)

summary(D2)
mean(Mratio2$Tactile, na.rm = TRUE)
sd(Mratio2$Tactile, na.rm = TRUE)

summary(D3)
mean(Mratio2$Pain, na.rm = TRUE)
sd(Mratio2$Pain, na.rm = TRUE)

summary(D4)
summary(D5)
summary(D6)


## Dataset 3

Mratio3 <- dataset3 %>%
  dcast(Pp ~ Modality, value.var = "Mratio") %>%
  mutate(D1 = Auditory - Visual) %>%
  na.omit(first_order3)

D1 <- lm(D1 ~ 1, Mratio3)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
cor.test(Mratio3$Auditory, Mratio3$Visual)
mean(Mratio3$Auditory)
mean(Mratio3$Visual)
sd(Mratio3$Auditory)
sd(Mratio3$Visual)



## Plots dataset 1 ---------------------------------------------

nsubj1 <-length(unique(dataset1$Pp))

## d', meta-d', M-ratio

Plot_d_1 <- dataset1 %>%
  select(Pp, 
         Task,
         score = d) %>% 
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj1),
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

bias1 %<>%
  select(Pp, 
         EM, VP, SM, EF) %>% 
  gather(Task, score, -Pp) 

Plot_bias_1 <- bias1 %>%
  group_by(Task) %>%
  summarise(VD = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            se = sd/sqrt(nsubj1),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = bias1,
             aes(x = Task, y = score, color = Task),
             position = position_jitterdodge(0.8),
             size = 1, alpha = 0.2,
             show.legend = FALSE) +
  geom_boxplot(data = bias1,
               aes(x = Task, y = score, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  xlab("Task") +
  ylab("Metacognitive bias")

Plot_mratio_1 <- dataset1 %>%
  select(Pp, 
         Task,
         score = Mratio) %>% 
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj1),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = dataset1,
             aes(x = Task, y = Mratio, color = Task),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.2,
             show.legend = FALSE) +
  geom_boxplot(data = dataset1,
               aes(x = Task, y = Mratio, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  scale_fill_manual(values = c("#0F056B", "#003366", "#2C75FF", "#9683EC")) +
  xlab("Task") +
  ylab("M-ratio value")

png(file="./plots/dataset1/dataset1.png", width=10, height=3, units="in", res=300)
plot_grid(Plot_d_1, Plot_bias_1, Plot_mratio_1, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
dev.off()


## Cross-task correlations

cor_Mratio <- dataset1 %>% 
  dcast(Pp ~ Task, value.var = 'Mratio')

# EM and VP
cor1 <- tidy(cor.test(cor_Mratio$EM, cor_Mratio$VP))

Plot_cor1 <- cor_Mratio %>% 
  ggplot(aes(x = EM, y = VP)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the VP task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor1$estimate, 2),"p = ", round(cor1$p.value, 3), "*"))

# EM and SM
cor2 <- tidy(cor.test(cor_Mratio$EM, cor_Mratio$SM))

Plot_cor2 <- cor_Mratio %>% 
  ggplot(aes(x = EM, y = SM)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the SM task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor2$estimate, 2),"p < 0.001 *"))

# EM and EF
cor3 <- tidy(cor.test(cor_Mratio$EM, cor_Mratio$EF))

Plot_cor3 <- cor_Mratio %>% 
  ggplot(aes(x = EM, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor3$estimate, 2),"p = ", round(cor3$p.value, 3), "*"))

# VP and SM
cor4 <- tidy(cor.test(cor_Mratio$VP, cor_Mratio$SM))

Plot_cor4 <- cor_Mratio %>% 
  ggplot(aes(x = VP, y = SM)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the VP task") +
  ylab("M-ratio for the SM task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor4$estimate, 2),"p = ", round(cor4$p.value, 3)))

# VP and EF
cor5 <- tidy(cor.test(cor_Mratio$VP, cor_Mratio$EF))

Plot_cor5 <- cor_Mratio %>% 
  ggplot(aes(x = VP, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the VP task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor5$estimate, 2),"p = ", round(cor5$p.value, 3), "*"))

# SM and EF
cor6 <- tidy(cor.test(cor_Mratio$SM, cor_Mratio$EF))

Plot_cor6 <- cor_Mratio %>% 
  ggplot(aes(x = SM, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the SM task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2.2) +
  ylim(0, 2.2) +
  ggtitle(paste("r = ", round(cor6$estimate, 2),"p = ", round(cor6$p.value, 3), "*"))


png(file="./plots/dataset1/Mratio_corr1.png", width=11, height=8, units="in", res=300)  
plot_grid(Plot_cor1, Plot_cor2, Plot_cor3,NULL,Plot_cor4, Plot_cor5,NULL,NULL,Plot_cor6, nrow = 3, ncol = 3)
dev.off()



## Plots dataset 2 ---------------------------------------------

nsubj2 <-length(unique(dataset2$Pp))


Plot_d_2 <- dataset2 %>%
  select(Pp, 
         Modality,
         score = d) %>% 
  group_by(Modality) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj2),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset2,
             aes(x = Modality, y = d, color = Modality),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.2, 
             show.legend = FALSE) +
  geom_boxplot(data = dataset2,
               aes(x = Modality, y = d, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(2), 
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0))+
  scale_colour_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  scale_fill_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  xlab("Modality") +
  ylab("d' value")


bias2 %<>%
  select(Pp, 
         Auditory, Visual, Tactile, Pain) %>% 
  gather(Task, score, -Pp) 

Plot_bias_2 <- bias2 %>%
  group_by(Task) %>%
  summarise(VD = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE),
            se = sd/sqrt(nsubj2),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = bias2,
             aes(x = Task, y = score, color = Task),
             position = position_jitterdodge(0.8),
             size = 1, alpha = 0.2,
             show.legend = FALSE) +
  geom_boxplot(data = bias2,
               aes(x = Task, y = score, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  scale_fill_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  xlab("Modality") +
  ylab("Metacognitive bias")

Plot_mratio_2 <- dataset2 %>%
  select(Pp, 
         Modality,
         score = Mratio) %>% 
  group_by(Modality) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj2),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset2,
             aes(x = Modality, y = Mratio, color = Modality),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.2,
             show.legend = FALSE) +
  geom_boxplot(data = dataset2,
               aes(x = Modality, y = Mratio, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  scale_fill_manual(values = c("#8B0000", "#FF0000", "#FF1493", "#FA8072")) +
  xlab("Modality") +
  ylab("M-ratio value")

png(file="./plots/dataset2/dataset2.png", width=10, height=3, units="in", res=300)
plot_grid(Plot_d_2, Plot_bias_2, Plot_mratio_2, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
dev.off()


## Cross-task correlations

cor_Mratio2 <- dataset2 %>% 
  dcast(Pp ~ Modality, value.var = 'Mratio')

# auditory and visual 
cor1 <- tidy(cor.test(cor_Mratio2$Auditory, cor_Mratio2$Visual))

Plot_cor1 <- cor_Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Visual)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the visual modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor1$estimate, 2),"p < 0.001 *"))


# auditory and tactile
cor2 <- tidy(cor.test(cor_Mratio2$Auditory, cor_Mratio2$Tactile))

Plot_cor2 <- cor_Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Tactile)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the tactile modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor2$estimate, 2),"p = ", round(cor2$p.value, 3)))


# auditory and pain
cor3 <- tidy(cor.test(cor_Mratio2$Auditory, cor_Mratio2$Pain))

Plot_cor3 <- cor_Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the pain modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor3$estimate, 2),"p = ", round(cor3$p.value, 3)))


# visual and tactile
cor4 <- tidy(cor.test(cor_Mratio2$Visual, cor_Mratio2$Tactile))

Plot_cor4 <- cor_Mratio2 %>% 
  ggplot(aes(x = Visual, y = Tactile)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the visual modality") +
  ylab("M-ratio for the tactile modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor4$estimate, 2),"p = ", round(cor4$p.value, 3), "*"))


# visual and pain
cor5 <- tidy(cor.test(cor_Mratio2$Visual, cor_Mratio2$Pain))

Plot_cor5 <- cor_Mratio2 %>% 
  ggplot(aes(x = Visual, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the visual modality") +
  ylab("M-ratio for the pain modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor5$estimate, 2),"p = ", round(cor5$p.value, 3)))


# tactivle and pain
cor6 <- tidy(cor.test(cor_Mratio2$Tactile, cor_Mratio2$Pain))

Plot_cor6 <- cor_Mratio2 %>% 
  ggplot(aes(x = Tactile, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the tactile modality") +
  ylab("M-ratio for the pain modality") +
  xlim(-0.5, 2.4) +
  ylim(-0.5, 2.4) +
  ggtitle(paste("r = ", round(cor6$estimate, 2),"p = ", round(cor6$p.value, 3), "*"))


png(file="./plots/dataset2/Mratio_corr2.png", width=11, height=8, units="in", res=300)  
plot_grid(Plot_cor1, Plot_cor2, Plot_cor3,NULL,Plot_cor4, Plot_cor5,NULL,NULL,Plot_cor6, nrow = 3, ncol = 3)
dev.off()




## Plots dataset 3 ---------------------------------------------------------

nsubj3 <-length(unique(dataset3$Pp))


Plot_d_3 <- dataset3 %>%
  select(Pp, 
         Modality,
         score = d) %>% 
  group_by(Modality) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj3),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset3,
             aes(x = Modality, y = d, color = Modality),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.2, 
             show.legend = FALSE) +
  geom_boxplot(data = dataset3,
               aes(x = Modality, y = d, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(2), 
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0))+
  scale_colour_manual(values = c("#FF8700", "#FFC100")) +
  scale_fill_manual(values = c("#FF8700", "#FFC100")) +
  xlab("Modality") +
  ylab("d' value")


bias3 %<>%
  select(Pp, 
         Auditory, Visual) %>% 
  gather(Task, score, -Pp) 

Plot_bias_3 <- bias3 %>%
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj3),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = bias3,
             aes(x = Task, y = score, color = Task),
             position = position_jitterdodge(0.8),
             size = 1, alpha = 0.2,
             show.legend = FALSE) +
  geom_boxplot(data = bias3,
               aes(x = Task, y = score, fill = Task),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#FF8700", "#FFC100")) +
  scale_fill_manual(values = c("#FF8700", "#FFC100")) +
  xlab("Modality") +
  ylab("Metacognitive bias")

Plot_mratio_3 <- dataset3 %>%
  select(Pp, 
         Modality,
         score = Mratio) %>% 
  group_by(Modality) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj3),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset3,
             aes(x = Modality, y = Mratio, color = Modality),
             position = position_jitterdodge(2),
             size = 1, alpha = 0.3,
             show.legend = FALSE) +
  geom_boxplot(data = dataset3,
               aes(x = Modality, y = Mratio, fill = Modality),
               outlier.shape = NA,
               alpha = .5, width = .2,
               position = position_dodge(0.7),
               show.legend = FALSE) +
  geom_point(size = 1, color = 'black', position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0, size = 0.5, color = 'black', position = position_dodge(0.2))+
  scale_colour_manual(values = c("#FF8700", "#FFC100")) +
  scale_fill_manual(values = c("#FF8700", "#FFC100")) +
  xlab("Modality") +
  ylab("M-ratio value")


corr_AV <- dataset3 %>% 
  dcast(Pp ~ Modality, value.var = "Mratio")
cor1 <- tidy(cor.test(corr_AV$Auditory, corr_AV$Visual))

Plot_cor <- corr_AV %>% 
  ggplot(aes(x = Auditory, y = Visual)) +
  geom_point(shape = 1, colour = "#FF7400") +
  geom_smooth(method = "lm", se = FALSE, colour = "#FF7400", size = 0.75) +
  xlab("Mratio for the auditory") +
  ylab("Mratio for the visual") +
  ggtitle(paste("r = ", round(cor1$estimate, 2),"p < 0.001 "))


png(file="./plots/dataset3/dataset3.png", width=8, height=6, units="in", res=300)
plot_grid(Plot_d_3, Plot_bias_3, Plot_mratio_3, Plot_cor, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
dev.off()


# All datasets
png(file="./plots/all_datasets.png", width=10, height=10, units="in", res=300)
plot_grid(Plot_d_1, Plot_d_2, Plot_d_3, Plot_bias_1, Plot_bias_2, Plot_bias_3, Plot_mratio_1, Plot_mratio_2, Plot_mratio_3, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), nrow = 3, ncol = 3)
dev.off()



## Correlation across datasets -----------------------------

# Dataset 1 and 3

Mratio_1_3 <- merge(Mratio1, Mratio3, by='Pp')

cor.test(Mratio_1_3$EM, Mratio_1_3$Auditory)
cor.test(Mratio_1_3$EM, Mratio_1_3$Visual)
cor.test(Mratio_1_3$VP, Mratio_1_3$Auditory)
cor.test(Mratio_1_3$VP, Mratio_1_3$Visual)
cor.test(Mratio_1_3$SM, Mratio_1_3$Auditory)
cor.test(Mratio_1_3$SM, Mratio_1_3$Visual)
cor.test(Mratio_1_3$EF, Mratio_1_3$Auditory)
cor.test(Mratio_1_3$EF, Mratio_1_3$Visual)

conf_clean_1_3 <- merge(conf1_clean_short, conf3_clean_short, by='Pp')

cor.test(conf_clean_1_3$EM, conf_clean_1_3$Auditory)
cor.test(conf_clean_1_3$EM, conf_clean_1_3$Visual)
cor.test(conf_clean_1_3$VP, conf_clean_1_3$Auditory)
cor.test(conf_clean_1_3$VP, conf_clean_1_3$Visual)
cor.test(conf_clean_1_3$SM, conf_clean_1_3$Auditory)
cor.test(conf_clean_1_3$SM, conf_clean_1_3$Visual)
cor.test(conf_clean_1_3$EF, conf_clean_1_3$Auditory)
cor.test(conf_clean_1_3$EF, conf_clean_1_3$Visual)

# Dataset 2 and 3

Mratio_2_3 <- merge(Mratio2, Mratio3, by='Pp')

cor.test(Mratio_2_3$Auditory.x, Mratio_2_3$Auditory.y)
cor.test(Mratio_2_3$Auditory.x, Mratio_2_3$Visual.y)
cor.test(Mratio_2_3$Visual.x, Mratio_2_3$Auditory.y)
cor.test(Mratio_2_3$Visual.x, Mratio_2_3$Visual.y)
cor.test(Mratio_2_3$Tactile, Mratio_2_3$Auditory.y)
cor.test(Mratio_2_3$Tactile, Mratio_2_3$Visual.y)
cor.test(Mratio_2_3$Pain, Mratio_2_3$Auditory.y)
cor.test(Mratio_2_3$Pain, Mratio_2_3$Visual.y)

conf_clean_2_3 <- merge(conf2_clean_short, conf3_clean_short, by='Pp')

cor.test(conf_clean_2_3$Auditory.x, conf_clean_2_3$Auditory.y)
cor.test(conf_clean_2_3$Auditory.x, conf_clean_2_3$Visual.y)
cor.test(conf_clean_2_3$Visual.x, conf_clean_2_3$Auditory.y)
cor.test(conf_clean_2_3$Visual.x, conf_clean_2_3$Visual.y)
cor.test(conf_clean_2_3$Tactile, conf_clean_2_3$Auditory.y)
cor.test(conf_clean_2_3$Tactile, conf_clean_2_3$Visual.y)
cor.test(conf_clean_2_3$Pain, conf_clean_2_3$Auditory.y)
cor.test(conf_clean_2_3$Pain, conf_clean_2_3$Visual.y)



## Comparision individual estimates H and non-H --------------------------

## Dataset 1

# load H-mratio
H_dataset1 <- read.csv("./results/dataset1/Hierarchial_Mratio.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
# load included participants id
H_Pp1 <- read.csv("./results/dataset1/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)

# get individual fits per task
H_Mratio1 <- H_dataset1 %>% 
  filter(str_detect(name, "Mratio")) %>% 
  filter(!str_detect(name, "mu")) %>% 
  filter(!str_detect(name, "sigma")) %>% 
  mutate(Pp_num = str_match(name, "[0-9]+,"), 
         Task_num = str_match(name, ",[0-9]"),
         Task = case_when(
           Task_num == ",1" ~ "EM",
           Task_num == ",2" ~ "VP",
           Task_num == ",3" ~ "SM",
           Task_num == ",4" ~ "EF"),
         Num = as.numeric(str_match(name, "[0-9]+"))) %>% 
  dcast(Num ~ Task, value.var = "mean") %>% 
  cbind(Pp = H_Pp1$Pp)


# correlations across estimates within tasks
Mratio_corr1 <- merge(Mratio1, H_Mratio1, by='Pp')

cor.test(Mratio_corr1$EM.x, Mratio_corr1$EM.y)
cor.test(Mratio_corr1$VP.x, Mratio_corr1$VP.y)
cor.test(Mratio_corr1$SM.x, Mratio_corr1$SM.y)
cor.test(Mratio_corr1$EF.x, Mratio_corr1$EF.y)

# calculate absolute distance between estimates
dist_estimates <- Mratio_corr1 %>% 
  mutate(dist_EM = abs(EM.x - EM.y),
         dist_VP = abs(VP.x - VP.y),
         dist_SM = abs(SM.x - SM.y),
         dist_EF = abs(EF.x - EF.y)) %>% 
  select(Pp, dist_EM,
         dist_VP, dist_SM, dist_EF)

# correlations distance and raw confidence
conf1_clean_short <- conf1_clean %>% 
  dcast(Pp ~ Task, value.var = 'score')
dist_estimates <- merge(dist_estimates, conf1_clean_short, by = "Pp")

cor.test(dist_estimates$EM, dist_estimates$dist_EM)
cor.test(dist_estimates$VP, dist_estimates$dist_VP)
cor.test(dist_estimates$SM, dist_estimates$dist_SM)
cor.test(dist_estimates$EF, dist_estimates$dist_EF)

# correlation distance and type-1 c
dist_estimates <- merge(dist_estimates, c1, by = "Pp")

cor.test(dist_estimates$EM, dist_estimates$dist_EM)
cor.test(dist_estimates$VP, dist_estimates$dist_VP)
cor.test(dist_estimates$SM, dist_estimates$dist_SM)
cor.test(dist_estimates$EF, dist_estimates$dist_EF)

# DGI

DGI_1 <- Mratio1 %>% 
  mutate(DGI = ((abs(EF-EM))+(abs(EF-VP))+(abs(EF-SM))+(abs(EM-VP))+(abs(EM-SM))+(abs(SM-VP)))/6) 

DGI_1 %<>%
  select(Pp, DGI)

DGI_1 <- merge(DGI_1, dist_estimates, by = "Pp")

cor.test(DGI_1$DGI, DGI_1$dist_EM)
cor.test(DGI_1$DGI, DGI_1$dist_VP)
cor.test(DGI_1$DGI, DGI_1$dist_SM)
cor.test(DGI_1$DGI, DGI_1$dist_EF)

## Dataset 2

# load H-mratio
H_dataset2 <- read.csv("./results/dataset2/Hierarchial_Mratio.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
# load included participants id
H_Pp2 <- read.csv("./results/dataset2/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)

# get individual fits per task
H_Mratio2 <- H_dataset2 %>% 
  filter(str_detect(name, "Mratio")) %>% 
  filter(!str_detect(name, "mu")) %>% 
  filter(!str_detect(name, "sigma")) %>% 
  mutate(Pp_num = str_match(name, "[0-9]+,"), 
         Task_num = str_match(name, ",[0-9]"),
         Modality = case_when(
           Task_num == ",1" ~ "Auditory",
           Task_num == ",2" ~ "Visual",
           Task_num == ",3" ~ "Tactile",
           Task_num == ",4" ~ "Pain"),
         Num = as.numeric(str_match(name, "[0-9]+"))) %>% 
  dcast(Num ~ Modality, value.var = "mean") %>% 
  cbind(Pp = H_Pp2$Pp)


# correlations across estimates within tasks
Mratio_corr2 <- merge(Mratio2, H_Mratio2, by='Pp')

cor.test(Mratio_corr2$Auditory.x, Mratio_corr2$Auditory.y)
cor.test(Mratio_corr2$Visual.x, Mratio_corr2$Visual.y)
cor.test(Mratio_corr2$Tactile.x, Mratio_corr2$Tactile.y)
cor.test(Mratio_corr2$Pain.x, Mratio_corr2$Pain.y)

# calculate distance between estimates
dist_estimates2 <- Mratio_corr2 %>% 
  mutate(dist_Auditory = Auditory.x - Auditory.y,
         dist_Visual = Visual.x - Visual.y,
         dist_Tactile = Tactile.x - Tactile.y,
         dist_Pain = Pain.x - Pain.y) %>% 
  select(Pp, dist_Auditory,
         dist_Visual, dist_Tactile, dist_Pain)

# correlations distance and d'
dist_estimates2 <- merge(dist_estimates2, first_order2, by = "Pp")

cor.test(dist_estimates2$Auditory, dist_estimates2$dist_Auditory)
cor.test(dist_estimates2$Visual, dist_estimates2$dist_Visual)
cor.test(dist_estimates2$Tactile, dist_estimates2$dist_Tactile)
cor.test(dist_estimates2$Pain, dist_estimates2$dist_Pain)


## Dataset 3

# load H-mratio
H_dataset3 <- read.csv("./results/dataset3/Hierarchial_Mratio.csv", header=TRUE, sep=",", dec=".", fill  = TRUE)
# load included participants id
H_Pp3 <- read.csv("./results/dataset3/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)

# get individual fits per task
H_Mratio3 <- H_dataset3 %>% 
  filter(str_detect(name, "Mratio")) %>% 
  filter(!str_detect(name, "mu")) %>% 
  filter(!str_detect(name, "sigma")) %>% 
  mutate(Pp_num = str_match(name, "[0-9]+,"), 
         Task_num = str_match(name, ",[0-9]"),
         Modality = case_when(
           Task_num == ",1" ~ "Auditory",
           Task_num == ",2" ~ "Visual"),
         Num = as.numeric(str_match(name, "[0-9]+"))) %>% 
  dcast(Num ~ Modality, value.var = "mean") %>% 
  cbind(Pp = H_Pp3$Pp)

# correlations across estimates within tasks
Mratio_corr3 <- merge(Mratio3, H_Mratio3, by='Pp')

cor.test(Mratio_corr3$Auditory.x, Mratio_corr3$Auditory.y)
cor.test(Mratio_corr3$Visual.x, Mratio_corr3$Visual.y)

# calculate distance between estimates
dist_estimates3 <- Mratio_corr3 %>% 
  mutate(dist_Auditory = Auditory.x - Auditory.y,
         dist_Visual = Visual.x - Visual.y) %>% 
  select(Pp, dist_Auditory, dist_Visual)

# correlations distance and d'
dist_estimates3 <- merge(dist_estimates3, first_order3, by = "Pp")

cor.test(dist_estimates3$Auditory, dist_estimates3$dist_Auditory)
cor.test(dist_estimates3$Visual, dist_estimates3$dist_Visual)




## Structural equation modeling: per dataset --------------------------------------------

# Dataset 1

data_m1 <- Mratio1 %>% 
  select(Pp, 
         Mratio_EM = EM, 
         Mratio_VP = VP, 
         Mratio_SM = SM, 
         Mratio_EF = EF) 

data_m1 <- merge(data_m1, bias1, by='Pp')
data_m1 %<>% 
  select(Pp, 
         Mratio_EM, 
         Mratio_VP, 
         Mratio_SM, 
         Mratio_EF,
         bias_EM = EM,
         bias_VP = VP,
         bias_SM = SM,
         bias_EF = EF) 


data_m1 <- merge(data_m1, rt1, by='Pp')
data_m1 %<>% 
  select(Pp, 
         Mratio_EM, 
         Mratio_VP, 
         Mratio_SM, 
         Mratio_EF,
         bias_EM,
         bias_VP,
         bias_SM,
         bias_EF, 
         rt_EM = EM,
         rt_VP = VP,
         rt_SM = SM,
         rt_EF = EF)

m1 <- '
g_metacog_eff =~ Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF 
'

cfa1 <- sem(m1, data=data_m1, missing="ml")
varTable(cfa1)
summary(cfa1, fit.measures=TRUE, standardized=TRUE)

interpret(cfa1)

lavaanPlot(model = cfa1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

m1 <- '
# measurement model
g_metacog_bias =~ bias_EM + bias_VP + bias_SM + bias_EF 
g_rt =~ rt_EM + rt_VP + rt_SM + rt_EF 
g_metacog_eff =~ Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt
'

fit1 <- sem(m1, data=data_m1, missing="ml")
varTable(fit1)
summary(fit1, fit.measures=TRUE, standardized=TRUE)

interpret(fit1)

lavaanPlot(model = fit1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# Dataset 2

data_m2 <- Mratio2 %>% 
  select(Pp, 
         Mratio_Auditory = Auditory, 
         Mratio_Visual = Visual, 
         Mratio_Tactile = Tactile, 
         Mratio_Pain = Pain) 

data_m2 <- merge(data_m2, bias2, by='Pp')
data_m2 %<>% 
  select(Pp, 
         Mratio_Auditory, 
         Mratio_Visual, 
         Mratio_Tactile, 
         Mratio_Pain,
         bias_Auditory = Auditory,
         bias_Visual = Visual,
         bias_Pain = Pain,
         bias_Tactile = Tactile) 

data_m2 <- merge(data_m2, rt2, by='Pp')
data_m2 %<>% 
  select(Pp, 
         Mratio_Auditory, 
         Mratio_Visual, 
         Mratio_Tactile, 
         Mratio_Pain,
         bias_Auditory,
         bias_Visual,
         bias_Pain,
         bias_Tactile,
         rt_Auditory = Auditory,
         rt_Visual = Visual,
         rt_Tactile = Tactile, 
         rt_Pain = Pain) 

m2 <- '
G_efficiency =~ Mratio_Auditory + Mratio_Visual + Mratio_Tactile + Mratio_Pain 
'

fit2 <- sem(m2, data=data_m2, missing="ml")
summary(fit2, fit.measures=TRUE, standardized=TRUE)

interpret(fit2)

lavaanPlot(model = fit2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

m2 <- '
g_perception =~ Mratio_Auditory + Mratio_Visual
g_somato =~ Mratio_Tactile + Mratio_Pain 
'

fit2_2 <- sem(m2, data=data_m2, missing="ml")
summary(fit2_2, fit.measures=TRUE, standardized=TRUE)

interpret(fit2_2)

lavaanPlot(model = fit2_2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# AIC comparision
aictab(cand.set = list(fit2, fit2_2), modnames = c('full generality', 'perception and somato'))


m2 <- '
# measurement model
g_metacog_bias =~ bias_Auditory + bias_Visual + bias_Tactile + bias_Pain 
g_rt =~ rt_Auditory + rt_Visual + rt_Tactile + rt_Pain 
g_metacog_eff =~ Mratio_Auditory + Mratio_Visual + Mratio_Tactile + Mratio_Pain 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt
'

fit2 <- sem(m2, data=data_m2, missing="ml")
varTable(fit2)
summary(fit2, fit.measures=TRUE, standardized=TRUE)

interpret(fit2)

lavaanPlot(model = fit2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# Dataset 3 

data_m3 <- Mratio3 %>% 
  select(Pp, 
         Mratio_Auditory = Auditory, 
         Mratio_Visual = Visual) 

data_m3 <- merge(data_m3, first_order3, by='Pp')
data_m3 %<>% 
  select(Pp, 
         Mratio_Auditory, 
         Mratio_Visual, 
         d_Auditory = Auditory,
         d_Visual = Visual) 

conf3_clean_short <- conf3_clean %>% 
  dcast(Pp ~ Task, value.var = 'score')

data_m3 <- merge(data_m3, conf3_clean_short, by='Pp')
data_m3 %<>% 
  select(Pp, 
         Mratio_Auditory, 
         Mratio_Visual, 
         d_Auditory,
         d_Visual,
         conf_Auditory = Auditory,
         conf_Visual = Visual) 

data_m3 <- merge(data_m3, rt3, by='Pp')
data_m3 %<>% 
  select(Pp, 
         Mratio_Auditory, 
         Mratio_Visual, 
         d_Auditory,
         d_Visual,
         conf_Auditory,
         rt_Auditory = Auditory,
         conf_Visual,
         rt_Visual = Visual) 

m3 <- 'g_metacog_eff =~ Mratio_Auditory + Mratio_Visual' # under-identified 
fit3 <- sem(m3, data=data_m3, estimator = 'ML', missing = 'ml') # under-identified 


m3 <- '
# measurement model
g_metacog_bias =~ conf_Auditory + conf_Visual 
g_metacog_eff =~ Mratio_Auditory + Mratio_Visual 
g_rt =~ rt_Auditory + rt_Visual 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt
'

fit3 <- sem(m3, data=data_m3, estimator = 'DWLS')
varTable(fit3)
summary(fit3, fit.measures=TRUE, standardized=TRUE)

lavaanPlot(model = fit3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)




## Confirmatory factor analyses --------------------------------------------------


Mratio1_long <- Mratio1 %>% 
  select(Pp, EM, VP, SM, EF) %>% 
  gather(Task, Mratio, -Pp)

Mratio2_long <- Mratio2 %>% 
  select(Pp, Auditory, Visual, Tactile, Pain) %>% 
  gather(Task, Mratio, -Pp)

Mratio3_long <- Mratio3 %>% 
  select(Pp, 
         Auditory_study3 = Auditory, 
         Visual_study3 = Visual) %>% 
  gather(Task, Mratio, -Pp)

all_dataset_Mratio <- Mratio1_long %>% 
  rbind(Mratio2_long) %>% 
  rbind(Mratio3_long)


# Confirmatory factor analyses 

all_dataset_Mratio %<>%
  dcast(Pp ~ Task, value.var = "Mratio")

cfa_model_1 <- 'g_all =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + EM + SM + VP + EF'
cfa_model_2 <- 'g_perception =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + VP
                g_cognitive =~ EM + SM + EF'

# model 1
fit_cfa_1 <- sem(cfa_model_1, data=all_dataset_Mratio, missing="ML")
lavInspect(fit_cfa_1, "coverage")
varTable(fit_cfa_1)
summary(fit_cfa_1, fit.measures=TRUE, standardized=TRUE)
standardizedsolution(fit_cfa_1)

interpret(fit_cfa_1)

lavaanPlot(model = fit_cfa_1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# model 2
fit_cfa_2 <- sem(cfa_model_2, data=all_dataset_Mratio, missing="ML")
varTable(fit_cfa_2)
summary(fit_cfa_2, fit.measures=TRUE, standardized=TRUE)

interpret(fit_cfa_2)

lavaanPlot(model = fit_cfa_2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# AIC comparision
aictab(cand.set = list(fit_cfa_1, fit_cfa_2), modnames = c('full generality', 'perception and cognitive'))


## Dataset 1 and 3

all_dataset_Mratio_1_3 <- Mratio1_long %>% 
  rbind(Mratio3_long) %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  filter(EM != "")

cfa_full_1_3 <- 'g_metacog_eff =~ EM + VP + SM + EF + Visual_study3 + Auditory_study3'

fit_full_1_3 <- sem(cfa_full_1_3, data=all_dataset_Mratio_1_3, missing="ML")
varTable(fit_full_1_3)
summary(fit_full_1_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_full_1_3)

lavaanPlot(model = fit_full_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


cfa_model_1_3 <- 'g_perception =~ VP + Visual_study3 + Auditory_study3
                g_cognitive =~ EM + SM + EF'

fit_cfa_1_3 <- sem(cfa_model_1_3, data=all_dataset_Mratio_1_3, missing="ML")
varTable(fit_cfa_1_3)
summary(fit_cfa_1_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_cfa_1_3)

lavaanPlot(model = fit_cfa_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


cfa_study_1_3 <- 'g_study1 =~ EM + VP + SM + EF  
                g_study3 =~  Auditory_study3 + Visual_study3'

fit_study_1_3 <- sem(cfa_study_1_3, data=all_dataset_Mratio_1_3, missing="ML")
varTable(fit_study_1_3)
summary(fit_study_1_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_study_1_3)

lavaanPlot(model = fit_study_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# AIC comparision
aictab(cand.set = list(fit_full_1_3, fit_cfa_1_3, fit_study_1_3), modnames = c('full generality', 'per modality', 'per study'))


## Dataset 2 and 3

all_dataset_Mratio_2_3 <- Mratio2_long %>% 
  rbind(Mratio3_long) %>% 
  dcast(Pp ~ Task, value.var = "Mratio") %>% 
  filter(Auditory != "")

cfa_full_2_3 <- 'g_metacog_eff =~ Auditory_study3 + Visual_study3 + Auditory + Visual + Pain + Tactile'

fit_full_2_3 <- sem(cfa_full_2_3, data=all_dataset_Mratio_2_3, missing="ML")
varTable(fit_full_2_3)
summary(fit_full_2_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_full_2_3)

lavaanPlot(model = fit_full_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

  
cfa_model_2_3 <- 'g_visual =~ Visual + Visual_study3
                g_auditory =~ Auditory + Auditory_study3
                g_somato =~ Pain + Tactile'

fit_cfa_2_3 <- sem(cfa_model_2_3, data=all_dataset_Mratio_2_3, missing="ML")
varTable(fit_cfa_2_3)
summary(fit_cfa_2_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_cfa_2_3)

lavaanPlot(model = fit_cfa_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


cfa_study_2_3 <- 'g_study2 =~ Auditory + Visual + Pain + Tactile
                g_study3 =~ Auditory_study3 + Visual_study3'

fit_study_2_3 <- sem(cfa_study_2_3, data=all_dataset_Mratio_2_3, missing="ML")
varTable(fit_study_2_3)
summary(fit_study_2_3, fit.measures=TRUE, standardized=TRUE)

interpret(fit_study_2_3)

lavaanPlot(model = fit_study_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# AIC comparision
aictab(cand.set = list(fit_full_2_3, fit_cfa_2_3, fit_study_2_3), modnames = c('full generality', 'per modality', 'per study'))



## Models per dataset (under-identified)

cfa_model_dataset <- 'g_study1 =~ EM + VP + SM + EF
                      g_study2 =~ Auditory + Visual + Pain + Tactile 
                      g_study3 =~ Auditory_multisite + Visual_multisite'

# model 1
fit_cfa_dataset <- sem(cfa_model_dataset, data=all_dataset_Mratio, missing="ML")
summary(fit_cfa_dataset, fit.measures=TRUE, standardized=TRUE)

interpret(fit_cfa_dataset)

lavaanPlot(model = fit_cfa_dataset, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)



## Stuctural equation modelling --------------------------------------------

# bias: gather studies 

bias1_long <- bias1 %>% 
  select(Pp, EM, VP, SM, EF) %>% 
  gather(Task, bias, -Pp)

bias2_long <- bias2 %>% 
  select(Pp, Auditory, Visual, Tactile, Pain) %>% 
  gather(Task, bias, -Pp)

bias3_long <- bias3 %>% 
  select(Pp, 
         Auditory_study3 = Auditory, 
         Visual_study3 = Visual) %>% 
  gather(Task, bias, -Pp)

all_dataset_bias <- bias1_long %>% 
  rbind(bias2_long) %>% 
  rbind(bias3_long) %>% 
  dcast(Pp ~ Task, value.var = "bias") %>% 
  select(Pp = Pp,
         Bias_auditory = Auditory, 
         Bias_auditory_study3 = Auditory_study3,
         Bias_visual = Visual, 
         Bias_visual_study3 = Visual_study3,
         Bias_pain = Pain, 
         Bias_tactile = Tactile,
         Bias_EM = EM,
         Bias_VP = VP,
         Bias_SM = SM,
         Bias_EF = EF)

# reaction time: gather studies

rt1_long <- rt1 %>% 
  select(Pp, EM, VP, SM, EF) %>% 
  gather(Task, rt, -Pp)

rt2_long <- rt2 %>% 
  select(Pp, Auditory, Visual, Tactile, Pain) %>% 
  gather(Task, rt, -Pp)

rt3_long <- rt3 %>% 
  select(Pp, 
         Auditory_study3 = Auditory, 
         Visual_study3 = Visual) %>% 
  gather(Task, rt, -Pp)

all_dataset_rt <- rt1_long %>% 
  rbind(rt2_long) %>% 
  rbind(rt3_long) %>% 
  dcast(Pp ~ Task, value.var = "rt") %>% 
  select(Pp = Pp,
         RT_auditory = Auditory, 
         RT_auditory_study3 = Auditory_study3,
         RT_visual = Visual, 
         RT_visual_study3 = Visual_study3,
         RT_pain = Pain, 
         RT_tactile = Tactile,
         RT_EM = EM,
         RT_VP = VP,
         RT_SM = SM,
         RT_EF = EF)

# first-order perf: gather studies

first_order1_long <- first_order1 %>% 
  select(Pp, EM, VP, SM, EF) %>% 
  gather(Task, d, -Pp)

first_order2_long <- first_order2 %>% 
  select(Pp, Auditory, Visual, Tactile, Pain) %>% 
  gather(Task, d, -Pp)

first_order3_long <- first_order3 %>% 
  select(Pp, 
         Auditory_study3 = Auditory, 
         Visual_study3 = Visual) %>% 
  gather(Task, d, -Pp)

all_dataset_d <- first_order1_long %>% 
  rbind(first_order2_long) %>% 
  rbind(first_order3_long) %>% 
  dcast(Pp ~ Task, value.var = "d") %>% 
  select(Pp = Pp,
         d_auditory = Auditory, 
         d_auditory_study3 = Auditory_study3,
         d_visual = Visual, 
         d_visual_study3 = Visual_study3,
         d_pain = Pain, 
         d_tactile = Tactile,
         d_EM = EM,
         d_VP = VP,
         d_SM = SM,
         d_EF = EF)


# gather bias, rt, and Mratio

data_for_sem <- all_dataset_Mratio %>% 
  select(Pp = Pp,
         Mratio_auditory = Auditory, 
         Mratio_auditory_study3 = Auditory_study3,
         Mratio_visual = Visual, 
         Mratio_visual_study3 = Visual_study3,
         Mratio_pain = Pain, 
         Mratio_tactile = Tactile,
         Mratio_EM = EM,
         Mratio_VP = VP,
         Mratio_SM = SM,
         Mratio_EF = EF)

data_for_sem <- merge(data_for_sem, all_dataset_bias, by="Pp")
data_for_sem <- merge(data_for_sem, all_dataset_rt, by="Pp")


# Structural equation models

sem1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
g_d =~ d_auditory + d_auditory_study3 + d_visual + d_visual_study3 + d_pain + d_tactile + d_EM + d_VP + d_SM + d_EF 
g_rt =~ RT_auditory + RT_auditory_study3 + RT_visual + RT_visual_study3 + RT_pain + RT_tactile + RT_EM + RT_VP + RT_SM + RT_EF 
g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt + g_d'

sem1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
g_d =~ d_auditory + d_auditory_study3 + d_visual + d_visual_study3 + d_pain + d_tactile + d_EM + d_VP + d_SM + d_EF 
g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF 
# regressions
g_metacog_eff ~ g_metacog_bias + g_d'

sem1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
g_rt =~ RT_auditory + RT_auditory_study3 + RT_visual + RT_visual_study3 + RT_pain + RT_tactile + RT_EM + RT_VP + RT_SM + RT_EF 
g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt'

fit_sem1 <- sem(sem1, data=data_for_sem, missing="ML")
varTable(fit_sem1)
summary(fit_sem1, fit.measures=TRUE, standardized=TRUE)

interpret(fit_sem1)

lavaanPlot(model = fit_sem1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


sem1.1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
Perceptual_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_VP
Cognitive_metacog_eff =~  Mratio_EM + Mratio_SM + Mratio_EF 
# regressions
Perceptual_metacog_eff ~ g_metacog_bias
Cognitive_metacog_eff ~ g_metacog_bias'

sem1.2 <- '
# measurement model
Perceptual_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_VP
Cognitive_metacog_bias =~ Bias_EM + Bias_SM + Bias_EF
Perceptual_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_VP
Cognitive_metacog_eff =~  Mratio_EM + Mratio_SM + Mratio_EF 
# regressions
Perceptual_metacog_eff ~ Perceptual_metacog_bias
Cognitive_metacog_eff ~ Cognitive_metacog_bias'

sem1.3 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF
g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile + Mratio_EM + Mratio_VP + Mratio_SM + Mratio_EF
# regressions
g_metacog_eff ~ g_metacog_bias'


fit_sem1.1 <- sem(sem1.1, data=data_for_sem, missing="ML")
fit_sem1.2 <- sem(sem1.2, data=data_for_sem, missing="ML")
fit_sem1.3 <- sem(sem1.3, data=data_for_sem, missing="ML")

interpret(fit_sem1.1)
interpret(fit_sem1.2)
interpret(fit_sem1.3)

summary(fit_sem1.1, fit.measures=TRUE, standardized=TRUE)


# AIC comparision
aictab(cand.set = list(fit_sem1.1, fit_sem1.2, fit_sem1.3), modnames = c('full generality', 'perception and cognitive efficiency', 'perception and cognitive efficiency and bias'))


lavaanPlot(model = fit_sem1.1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)



sem2 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile 
g_rt =~ RT_auditory + RT_auditory_study3 + RT_visual + RT_visual_study3 + RT_pain + RT_tactile 
g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile 
# regressions
g_metacog_eff ~ g_metacog_bias + g_rt'


# sem2 <- '
# # measurement model
# g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile 
# g_metacog_eff =~ Mratio_auditory + Mratio_auditory_study3 + Mratio_visual + Mratio_visual_study3 + Mratio_pain + Mratio_tactile 
# # regressions
# g_metacog_eff ~ g_metacog_bias'


fit_sem2 <- sem(sem2, data=data_for_sem, missing="ML")
varTable(fit_sem2)
summary(fit_sem2, fit.measures=TRUE, standardized=TRUE)

interpret(fit_sem2)

lavaanPlot(model = fit_sem2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


