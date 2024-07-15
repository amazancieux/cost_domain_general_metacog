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

all_dataset_Mratio <- read.csv("./data_for_cfa.csv", header=TRUE, sep=";", dec=",", fill  = TRUE)


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
aictab(cand.set = list(fit_full_general, fit_2_factors, fit_4_factors, fit_3_studies), modnames = c('full generality', '2 factors', '4 factors', 'per study'))


## Confirmatory factor analyses: Study 1 and 3 -----------------------------

# full general model
cfa_full_1_3 <- 'g_metacog_eff =~ EM + VP + SM + EF + Visual_study3 + Auditory_study3'
fit_full_1_3 <- fit_full_1_3 <- cfa(model = cfa_full_1_3, 
                                    data = all_dataset_Mratio, 
                                    missing="FIML")

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
                         missing="FIML")
    
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
                    missing="FIML")

summary(fit_full_2_3, fit.measures=TRUE, standardized=TRUE)
interpret(fit_full_2_3)

lavaanPlot(model = fit_full_2_3, sig=.05,stars=c("regress","latent","covs"),
           node_options = list(shape = "box", fontname = "Arial"),
           edge_options = list(color = "grey"),
           coefs = TRUE, covs = TRUE, stand=TRUE)

# 3-factor model
cfa_3_factors_2_3 <- 'g_visual =~ Visual + Visual_study3
                      g_auditory =~ Auditory + Auditory_study3
                      g_somato =~ Pain + Tactile'
fit_3_factors_2_3 <- cfa(model = cfa_3_factors_2_3, 
                         data = all_dataset_Mratio, 
                         missing="FIML")
  
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
