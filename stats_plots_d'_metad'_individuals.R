#####################################


# Statistics and plots for first-order performance and
# metacognitive bias, and individuals M-ratio
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

# after screening outlier exclusion for Mratio > 3 for dataset 1
dataset1 %<>%
  filter(Mratio < 3)

# after screening outlier exclusion for Mratio > 2 and < 0 for dataset 2
dataset2 %<>%
  filter(Mratio > 0 & Mratio < 2)

# after screening outlier exclusion for Mratio > 2.8 and < 0 for dataset 2
dataset3 %<>%
  filter(Mratio > 0)


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

# linear models
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

qqnorm(dataset1$Mratio)
qqline(dataset1$Mratio)

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


# correlation with the same participants as H-Meta-d'
HMeta_pp1 <- read.csv("./results/dataset1/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)
Mratio1_Hpp <- Mratio1[Mratio1$Pp %in% HMeta_pp1$Pp, ]

cor.test(Mratio1_Hpp$EM, Mratio1_Hpp$VP)
cor.test(Mratio1_Hpp$EM, Mratio1_Hpp$SM)
cor.test(Mratio1_Hpp$EM, Mratio1_Hpp$EF)
cor.test(Mratio1_Hpp$VP, Mratio1_Hpp$SM)
cor.test(Mratio1_Hpp$VP, Mratio1_Hpp$EF)
cor.test(Mratio1_Hpp$SM, Mratio1_Hpp$EF)


## Dataset 2

Mratio2 <- dataset2 %>% 
  dcast(Pp ~ Modality, value.var = "Mratio") %>% 
  mutate(D1 = Auditory - Visual,
         D2 = Auditory - Tactile,
         D3 = Auditory - Pain,
         D4 = Visual - Tactile,
         D5 = Visual - Pain,
         D6 = Tactile - Pain) 

# linear models
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


# correlation with the same participants as H-Meta-d'
HMeta_pp2 <- read.csv("./results/dataset2/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)
Mratio2_Hpp <- Mratio2[Mratio2$Pp %in% HMeta_pp2$Pp, ]

cor.test(Mratio2_Hpp$Auditory, Mratio2_Hpp$Visual)
cor.test(Mratio2_Hpp$Auditory, Mratio2_Hpp$Tactile)
cor.test(Mratio2_Hpp$Auditory, Mratio2_Hpp$Pain)
cor.test(Mratio2_Hpp$Visual, Mratio2_Hpp$Tactile)
cor.test(Mratio2_Hpp$Visual, Mratio2_Hpp$Pain)
cor.test(Mratio2_Hpp$Tactile, Mratio2_Hpp$Pain)


## Dataset 3

Mratio3 <- dataset3 %>%
  dcast(Pp ~ Modality, value.var = "Mratio") %>%
  mutate(D1 = Auditory - Visual) 

# linear models
D1 <- lm(D1 ~ 1, Mratio3)

qqnorm(residuals(D1))
qqline(residuals(D1))

data.frame(x = residuals(D1)) %>%
  ggplot(aes(x = x)) +
  geom_histogram()
shapiro.test(residuals(D1))

summary(D1)
cor.test(Mratio3$Auditory, Mratio3$Visual)
mean(Mratio3$Auditory, na.rm = TRUE)
mean(Mratio3$Visual, na.rm = TRUE)
sd(Mratio3$Auditory, na.rm = TRUE)
sd(Mratio3$Visual, na.rm = TRUE)

# correlation with the same participants as H-Meta-d'
HMeta_pp3 <- read.csv("./results/dataset3/participant_id_H-metad.csv", header=TRUE, sep=";", dec=".", fill  = TRUE)
Mratio3_Hpp <- Mratio3[Mratio3$Pp %in% HMeta_pp3$Pp, ]

cor.test(Mratio3_Hpp$Auditory, Mratio3_Hpp$Visual)



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
             position = position_jitterdodge(0.8),
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
  ylim(0, 4.9) +
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
  ylim(-0.40, 0.34) +
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
             position = position_jitterdodge(0.8),
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
  ylim(0, 2.85) +
  xlab("Task") +
  ylab("M-ratio value")


## Cross-task correlations

# EM and VP
cor1 <- tidy(cor.test(Mratio1$EM, Mratio1$VP))

Plot_cor1 <- Mratio1 %>% 
  ggplot(aes(x = EM, y = VP)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the VP task") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor1$estimate, 2),"p = ", round(cor1$p.value, 3), "*"))

# EM and SM
cor2 <- tidy(cor.test(Mratio1$EM, Mratio1$SM))

Plot_cor2 <- Mratio1 %>% 
  ggplot(aes(x = EM, y = SM)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the SM task") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor2$estimate, 2),"p < 0.001 *"))

# EM and EF
cor3 <- tidy(cor.test(Mratio1$EM, Mratio1$EF))

Plot_cor3 <- Mratio1 %>% 
  ggplot(aes(x = EM, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the EM task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor3$estimate, 2),"p = ", round(cor3$p.value, 3), "*"))

# VP and SM
cor4 <- tidy(cor.test(Mratio1$VP, Mratio1$SM))

Plot_cor4 <- Mratio1 %>% 
  ggplot(aes(x = VP, y = SM)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the VP task") +
  ylab("M-ratio for the SM task") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor4$estimate, 2),"p = ", round(cor4$p.value, 3)))

# VP and EF
cor5 <- tidy(cor.test(Mratio1$VP, Mratio1$EF))

Plot_cor5 <- Mratio1 %>% 
  ggplot(aes(x = VP, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the VP task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor5$estimate, 2),"p = ", round(cor5$p.value, 3), "*"))

# SM and EF
cor6 <- tidy(cor.test(Mratio1$SM, Mratio1$EF))

Plot_cor6 <- Mratio1 %>% 
  ggplot(aes(x = SM, y = EF)) +
  geom_point(colour = "#003366", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#003366", size = 0.75) +
  xlab("M-ratio for the SM task") +
  ylab("M-ratio for the EF task") +
  xlim(0, 2) +
  ylim(0, 2) +
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
             position = position_jitterdodge(0.8),
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
  ylim(0, 4.9) +
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
  ylim(-0.40, 0.34) +
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
             position = position_jitterdodge(0.8),
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
  ylim(0, 2.85) +
  xlab("Modality") +
  ylab("M-ratio value")


## Cross-task correlations

# auditory and visual 
cor1 <- tidy(cor.test(Mratio2$Auditory, Mratio2$Visual))

Plot_cor1 <- Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Visual)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the visual modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor1$estimate, 2), "p = ", round(cor2$p.value, 3)))


# auditory and tactile
cor2 <- tidy(cor.test(Mratio2$Auditory, Mratio2$Tactile))

Plot_cor2 <- Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Tactile)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the tactile modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor2$estimate, 2),"p = ", round(cor2$p.value, 3)))


# auditory and pain
cor3 <- tidy(cor.test(Mratio2$Auditory, Mratio2$Pain))

Plot_cor3 <- Mratio2 %>% 
  ggplot(aes(x = Auditory, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the auditory modality") +
  ylab("M-ratio for the pain modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor3$estimate, 2),"p = ", round(cor3$p.value, 3)))


# visual and tactile
cor4 <- tidy(cor.test(Mratio2$Visual, Mratio2$Tactile))

Plot_cor4 <- Mratio2 %>% 
  ggplot(aes(x = Visual, y = Tactile)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the visual modality") +
  ylab("M-ratio for the tactile modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor4$estimate, 2),"p = ", round(cor4$p.value, 3)))


# visual and pain
cor5 <- tidy(cor.test(Mratio2$Visual, Mratio2$Pain))

Plot_cor5 <- Mratio2 %>% 
  ggplot(aes(x = Visual, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the visual modality") +
  ylab("M-ratio for the pain modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor5$estimate, 2),"p = ", round(cor5$p.value, 3)))


# tactivle and pain
cor6 <- tidy(cor.test(Mratio2$Tactile, Mratio2$Pain))

Plot_cor6 <- Mratio2 %>% 
  ggplot(aes(x = Tactile, y = Pain)) +
  geom_point(colour = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", size = 0.75) +
  xlab("M-ratio for the tactile modality") +
  ylab("M-ratio for the pain modality") +
  xlim(0, 2) +
  ylim(0, 2) +
  ggtitle(paste("r = ", round(cor6$estimate, 2),"p = ", round(cor6$p.value, 3)))


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
             position = position_jitterdodge(0.8),
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
  ylim(0, 4.9) +
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
  ylim(-0.40, 0.34) +
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
             position = position_jitterdodge(0.8),
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
  ylim(0, 2.85) +
  xlab("Modality") +
  ylab("M-ratio value")


corr_AV <- dataset3 %>% 
  dcast(Pp ~ Modality, value.var = "Mratio")
cor1 <- tidy(cor.test(corr_AV$Auditory, corr_AV$Visual))

corr_AV %>% 
  ggplot(aes(x = Auditory, y = Visual)) +
  geom_point(shape = 1, colour = "#FF7400") +
  geom_smooth(method = "lm", se = FALSE, colour = "#FF7400", size = 0.75) +
  xlab("Mratio for the auditory") +
  ylab("Mratio for the visual") +
  ggtitle(paste("r = ", round(cor1$estimate, 2),"p < 0.001 "))


# Plot all datasets

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

# Gather Mratio across studies

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

all_dataset_Mratio %<>%
  dcast(Pp ~ Task, value.var = "Mratio")

write.csv2(all_dataset_Mratio, "Mratio_all_studies.csv")


# Gather metacognitive bias across studies 

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

write.csv2(all_dataset_bias, "bias_all_studies.csv")


## Residual correlations -------------------------------------------------------

# Merge bias and Mratio dataframes
all_bias_Mratio <- merge(all_dataset_bias, all_dataset_Mratio, by ="Pp")

# Dataset 1

EM <- lm(EM ~ Bias_EM, all_bias_Mratio)
res <- EM[[2]]
Res_EM <- all_dataset_Mratio %>% 
  select(Pp, EM) %>% 
  filter(EM != "") %>% 
  mutate(res_EM = res)

VP <- lm(VP ~ Bias_VP, all_bias_Mratio)
res <- VP[[2]]
Res_VP <- all_dataset_Mratio %>% 
  select(Pp, VP) %>% 
  filter(VP != "") %>% 
  mutate(res_VP = res)

residual1 <- merge(Res_EM, Res_VP, by='Pp')

SM <- lm(SM ~ Bias_SM, all_bias_Mratio)
res <- SM[[2]]
Res_SM <- all_dataset_Mratio %>% 
  select(Pp, SM) %>% 
  filter(SM != "") %>% 
  mutate(res_SM = res)

residual1 <- merge(residual1, Res_SM, by='Pp')

EF <- lm(EF ~ Bias_EF, all_bias_Mratio)
res <- EF[[2]]
Res_EF <- all_dataset_Mratio %>% 
  select(Pp, EF) %>% 
  filter(EF != "") %>% 
  mutate(res_EF = res)

residual1 <- merge(residual1, Res_EF, by='Pp')


cor.test(residual1$res_EM, residual1$res_VP, na.rm = TRUE)
cor.test(residual1$res_EM, residual1$res_SM, na.rm = TRUE)
cor.test(residual1$res_EM, residual1$res_EF, na.rm = TRUE)
cor.test(residual1$res_SM, residual1$res_VP, na.rm = TRUE)
cor.test(residual1$res_EF, residual1$res_VP, na.rm = TRUE)
cor.test(residual1$res_SM, residual1$res_EF, na.rm = TRUE)


# Dataset 2

auditory <- lm(Auditory ~ Bias_auditory, all_bias_Mratio)
res <- auditory[[2]]
Res_auditory <- all_bias_Mratio %>% 
  select(Pp, Auditory) %>% 
  filter(Auditory != "") %>% 
  mutate(res_auditory = res)

visual <- lm(Visual ~ Bias_visual, all_bias_Mratio)
res <- visual[[2]]
Res_visual <- all_bias_Mratio %>% 
  select(Pp, Visual) %>% 
  filter(Visual != "") %>% 
  mutate(res_visual = res)

residual2 <- merge(Res_auditory, Res_visual, by='Pp')

tactile <- lm(Tactile ~ Bias_tactile, all_bias_Mratio)
res <- tactile[[2]]
Res_tactile <- all_bias_Mratio %>% 
  select(Pp, Tactile, Bias_tactile) %>% 
  filter(Tactile != "") %>% 
  mutate(res_tactile = res)

residual2 <- merge(residual2, Res_tactile, by='Pp')

pain <- lm(Pain ~ Bias_pain, all_bias_Mratio)
res <- pain[[2]]
Res_pain <- all_bias_Mratio %>% 
  select(Pp, Pain, Bias_pain) %>% 
  filter(Pain != "") %>% 
  mutate(res_pain = res)

residual2 <- merge(residual2, Res_pain, by='Pp')


cor.test(residual2$res_auditory, residual2$res_visual)
cor.test(residual2$res_auditory, residual2$res_tactile)
cor.test(residual2$res_auditory, residual2$res_pain)
cor.test(residual2$res_tactile, residual2$res_visual)
cor.test(residual2$res_pain, residual2$res_visual)
cor.test(residual2$res_tactile, residual2$res_pain)


# Dataset 3

auditory3 <- lm(Auditory_study3 ~ Bias_auditory_study3, all_bias_Mratio)
res <- auditory[[2]]
Res_auditory3 <- all_bias_Mratio %>% 
  select(Pp, Auditory_study3) %>% 
  filter(Auditory_study3 != "") %>% 
  mutate(res_auditory = res)

visual3 <- lm(Visual_study3 ~ Bias_visual_study3, all_bias_Mratio)
res <- visual[[2]]
Res_visual3 <- all_bias_Mratio %>% 
  select(Pp, Visual_study3) %>% 
  filter(Visual_study3 != "") %>% 
  mutate(res_visual = res)

residual3 <- merge(Res_auditory3, Res_visual3, by='Pp')
cor.test(residual3$res_auditory, residual3$res_visual)

