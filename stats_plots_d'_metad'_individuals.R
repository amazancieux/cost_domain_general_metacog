#####################################


# Statistics and plots for first-order performance and 
# individuals meta-d' for the 3 datasets
# COST action "The neural architecture of consciousness"

# Audrey Mazancieux 2023


#####################################


## Packages and graphic theme ----------------------------------------------
library(tidyverse)
library(magrittr)
library(reshape2)


## Import preprocessed data -----------------------------------------------------------

dataset1 <- read.csv("./results/dataset1/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset2 <- read.csv("./results/dataset2/clean_Mratio_individual.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
dataset3 <- read.csv("./results/dataset3/clean_individual_Mratio.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)


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


## First-order performance all datasets (plots) ---------------------------------------------

Plot_d_1 <- dataset1 %>%
  select(Pp, 
         Task,
         score = d) %>% 
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = dataset1,
             aes(x = Task, y = d, color = Task),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = dataset1,
               aes(x = Task, y = d, fill = Task),
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

Plot_d_2 <- dataset2 %>%
  group_by(Modality) %>%
  summarise(VD = mean(d),
            sd = sd(d),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset2,
             aes(x = Modality, y = d, color = Modality),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = dataset2,
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

Plot_d_3 <- dataset3 %>%
  group_by(Modality) %>%
  summarise(VD = mean(d),
            sd = sd(d),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Modality, y = VD, color = Modality)) +
  geom_point(data = dataset3,
             aes(x = Modality, y = d, color = Modality),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = dataset3,
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


png(file="./plots/d_prime_all_datasets.png", width=10, height=8, units="in", res=300)
plot_grid(Plot_d_1, Plot_d_2, Plot_d_3, labels = c("A", "B", "C"), nrow = 1, ncol = 3)
dev.off()


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


## Individual Mratio all datasets (plots) ------------------------------------

Plot_Mratio <- Mratio %>%
  select(Pp, 
         Task,
         score = Mratio) %>% 
  group_by(Task) %>%
  summarise(VD = mean(score),
            sd = sd(score),
            se = sd/sqrt(nsubj),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = Task, y = VD, color = Task)) +
  geom_point(data = Mratio,
             aes(x = Task, y = Mratio, color = Task),
             position = position_jitterdodge(0.3),
             size = 1, shape = 1) +
  geom_boxplot(data = Mratio,
               aes(x = Task, y = Mratio, fill = Task),
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


