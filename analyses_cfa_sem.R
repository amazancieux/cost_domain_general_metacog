#####################################


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

all_dataset_Mratio <- read.csv("./Mratio_all_studies.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)
all_dataset_bias <- read.csv("./bias_all_studies.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)


## Confirmatory factor analyses: all studies --------------------------------------------

# full general model
cfa_full_general <- 'g_metacog_eff =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + EM + SM + VP + EF'
fit_full_general <- cfa(model = cfa_full_general, 
                        data = all_dataset_Mratio, 
                        missing="pairwise")

summary(fit_full_general, fit.measures=TRUE, standardized=TRUE)
interpret(fit_full_general)

lavaanPlot(model = fit_full_general, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# 2-factor model
cfa_2_factors <- 'g_perception =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + VP
                  g_cognitive =~ EM + SM + EF'
fit_2_factors <- cfa(model = cfa_2_factors, 
                     data=all_dataset_Mratio, 
                     missing="pairwise")

summary(fit_2_factors, fit.measures=TRUE, standardized=TRUE)
interpret(fit_2_factors)

lavaanPlot(model = fit_2_factors, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# 4-factor model
cfa_4_factors <- 'g_visual =~ + Visual + Visual_study3 + VP
                  g_cognitive =~ EM + SM + EF
                  g_auditory =~  Auditory + Auditory_study3
                  g_somato =~ Pain + Tactile'
fit_4_factors <- cfa(model = cfa_4_factors, 
                     data=all_dataset_Mratio, 
                     missing="pairwise")

summary(fit_4_factors, fit.measures=TRUE, standardized=TRUE)
interpret(fit_4_factors)


lavaanPlot(model = fit_4_factors, sig=.05,stars=c("regress","latent","covs"),
           graph_options = list(rankdir = "LR", layout = "circo"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# study model
# --> OVERFITS THE DATA 
cfa_3_studies <- 'study_1 =~ VP + EM + SM + EF
                  study_2 =~ Auditory + Visual + Pain + Tactile
                  study_3 =~ Visual_study3 + Auditory_study3'
fit_3_studies <- cfa(model = cfa_3_studies, 
                     data=all_dataset_Mratio, 
                     missing="pairwise")

summary(fit_3_studies, fit.measures=TRUE, standardized=TRUE)
interpret(fit_3_studies)

lavaanPlot(model = fit_3_studies, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# AIC comparision
aictab(cand.set = list(fit_full_general, fit_2_factors, fit_4_factors), modnames = c('full generality', '2 factors', '4 factors'))


## Confirmatory factor analyses: Study 1 and 3 -----------------------------

# full general model
cfa_full_1_3 <- 'g_metacog_eff =~ Auditory_study3 + Visual_study3 + EM + VP + SM + EF'
fit_full_1_3 <- fit_full_1_3 <- cfa(model = cfa_full_1_3, 
                                    data = all_dataset_Mratio, 
                                    missing="pairwise")

summary(fit_full_1_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_full_1_3)

lavaanPlot(model = fit_full_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# 2-factor model
cfa_2_factors_1_3 <- 'g_perception =~ VP + Visual_study3 + Auditory_study3
                      g_cognitive =~ EM + SM + EF'
fit_2_factors_1_3 <- cfa(model = cfa_2_factors_1_3, 
                         data = all_dataset_Mratio, 
                         missing="pairwise")
    
summary(fit_2_factors_1_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_2_factors_1_3)

lavaanPlot(model = fit_2_factors_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# model per study
cfa_study_1_3 <- 'g_study_1 =~ EM + VP + SM + EF  
                  g_study_3 =~  Auditory_study3 + Visual_study3'
fit_study_1_3 <- cfa(model = cfa_study_1_3, 
                     data = all_dataset_Mratio, 
                     missing="pairwise")

summary(fit_study_1_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_study_1_3)

lavaanPlot(model = fit_study_1_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# AIC comparision
aictab(cand.set = list(fit_full_1_3, fit_2_factors_1_3, fit_study_1_3), modnames = c('full generality', 'per modality', 'per study'))


## Confirmatory factor analyses: Study 2 and 3 -----------------------------

# full general model
cfa_full_2_3 <- 'g_metacog_eff =~ Auditory_study3 + Visual_study3 + Auditory + Visual + Pain + Tactile'
fit_full_2_3 <- cfa(model = cfa_full_2_3, 
                    data = all_dataset_Mratio, 
                    missing="pairwise")

summary(fit_full_2_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_full_2_3)

lavaanPlot(model = fit_full_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# 3-factor model
# --> OVERFITS THE DATA
cfa_3_factors_2_3 <- 'g_visual =~ Visual + Visual_study3
                      g_auditory =~ Auditory + Auditory_study3
                      g_somato =~ Pain + Tactile'
fit_3_factors_2_3 <- cfa(model = cfa_3_factors_2_3, 
                         data = all_dataset_Mratio, 
                         missing="pairwise")
  
summary(fit_3_factors_2_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_3_factors_2_3)

lavaanPlot(model = fit_3_factors_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# model per study
cfa_study_2_3 <- 'g_study2 =~ Auditory + Visual + Pain + Tactile
                  g_study3 =~ Auditory_study3 + Visual_study3'
fit_study_2_3 <- cfa(model = cfa_study_2_3, 
                     data = all_dataset_Mratio, 
                     missing="pairwise")

summary(fit_study_2_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_study_2_3)

lavaanPlot(model = fit_study_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# AIC comparision
aictab(cand.set = list(fit_full_2_3, fit_3_factors_2_3, fit_study_2_3), modnames = c('full generality', 'per modality', 'per study'))
aictab(cand.set = list(fit_full_2_3, fit_study_2_3), modnames = c('full generality', 'per study'))


## Exploratory structural equation modeling --------------------------------------------

data_for_sem <- merge(all_dataset_Mratio, all_dataset_bias, by="Pp")

# Define models for all datatsets

sem1.1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
Perceptual_metacog_eff =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + VP
Cognitive_metacog_eff =~ EM + SM + EF 
# regressions
Perceptual_metacog_eff ~ g_metacog_bias
Cognitive_metacog_eff ~ g_metacog_bias'

sem1.2 <- '
# measurement model
Perceptual_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile + Bias_VP
Cognitive_metacog_bias =~ Bias_EM + Bias_SM + Bias_EF
Perceptual_metacog_eff =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile + VP
Cognitive_metacog_eff =~  EM + SM + EF 
# regressions
Perceptual_metacog_eff ~ Perceptual_metacog_bias
Cognitive_metacog_eff ~ Cognitive_metacog_bias'


# Fit models

fit_sem1.1 <- sem(sem1.1, data=data_for_sem, missing="FIML")
fit_sem1.2 <- sem(sem1.2, data=data_for_sem, missing="FIML")

interpret(fit_sem1.1)
interpret(fit_sem1.2)

summary(fit_sem1.1, fit.measures=TRUE, standardized=TRUE)
summary(fit_sem1.2, fit.measures=TRUE, standardized=TRUE)


# AIC comparision
aictab(cand.set = list(fit_sem1.1, fit_sem1.2), modnames = c('perception and cognitive efficiency', 'perception and cognitive efficiency and bias'))


lavaanPlot(model = fit_sem1.2, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

lavaanPlot(model = fit_sem1.1, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)


# Define models for Study 1-3

sem2.1 <- '
# measurement model
g_metacog_bias =~ Bias_auditory_study3 + Bias_visual_study3 + Bias_EM + Bias_VP + Bias_SM + Bias_EF 
Perceptual_metacog_eff =~ Auditory_study3 + Visual_study3 + VP
Cognitive_metacog_eff =~ EM + SM + EF 
# regressions
Perceptual_metacog_eff ~ g_metacog_bias
Cognitive_metacog_eff ~ g_metacog_bias'

sem2.2 <- '
# measurement model
Perceptual_metacog_bias =~ Bias_auditory_study3 + Bias_visual_study3 + Bias_VP
Cognitive_metacog_bias =~ Bias_EM + Bias_SM + Bias_EF
Perceptual_metacog_eff =~ Auditory_study3 + Visual_study3 + VP
Cognitive_metacog_eff =~  EM + SM + EF 
# regressions
Perceptual_metacog_eff ~ Perceptual_metacog_bias
Cognitive_metacog_eff ~ Cognitive_metacog_bias'

# Fit models

fit_sem2.1 <- sem(sem2.1, data=data_for_sem, missing="FIML")
fit_sem2.2 <- sem(sem2.2, data=data_for_sem, missing="FIML")

interpret(fit_sem2.1)
interpret(fit_sem2.2)

summary(fit_sem2.1, fit.measures=TRUE, standardized=TRUE)
summary(fit_sem2.2, fit.measures=TRUE, standardized=TRUE)

# AIC comparision
aictab(cand.set = list(fit_sem2.1, fit_sem2.2), modnames = c('perception and cognitive efficiency', 'perception and cognitive efficiency and bias'))


# Define models for Study 2-3

sem3 <- '
# measurement model
g_metacog_bias =~ Bias_auditory + Bias_auditory_study3 + Bias_visual + Bias_visual_study3 + Bias_pain + Bias_tactile 
g_metacog_eff =~ Auditory + Auditory_study3 + Visual + Visual_study3 + Pain + Tactile
# regressions
g_metacog_eff ~ g_metacog_bias'


# Fit models

fit_sem3 <- sem(sem3, data=data_for_sem, missing="FIML")
interpret(fit_sem3)
summary(fit_sem3, fit.measures=TRUE, standardized=TRUE)




