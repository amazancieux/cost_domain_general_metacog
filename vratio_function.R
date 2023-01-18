# v-ratio main function from the varatio_example_script. 
#
# This code was written in R-4.1.2, using the following package versions:
# Rcpp v1.0.7
# RcppZiggurat v0.1.6
# DEoptim v2.2.6
# reshape v0.8.8
#
# Code written by Kobe Desender and Luc Vermeylen, 2/12/2022

# Required Libraries
library(Rcpp) # to source, compile and run C++ functions
library(RcppZiggurat) #required for additional random number generation functions
# Note that Rcpp requires a c++ compiler, so you might have to install this; if so have a look here: https://teuder.github.io/rcpp4everyone_en/020_install.html
library(DEoptim) # optimization algorithm
library(reshape)
sourceCpp("DDM_with_confidence_slow_fullconfRT.cpp") # this will give R access to the function that is used to simulate from the v-ratio model (written in rcpp to increase speed)

# Below is the objective function that is used to fit the model. It requires three inputs:
# Params (in the following order):  
### v=drift rate (usual range 0-4)
### a=boundary (usual range .5-4)
### ter=non-decision time (usual range 0-2)
### vratio=the measure of interest; note that the scripts _directly_ estimates v-ratio (usual range 0-2)
### addmean=parameter to map confidence predictions to the empirical data
### addsd=parameter to map confidence predictions to the empirical data

# observations: the data you want to fit. Note that the following columns are required:
### "cor": accuracy, with 1=correct, 0=error
### "rt": reaction time, in seconds
### "rtcj": reaction time of the confidence judgments, in seconds,
### "cj": confidence judgment

#conf_levels: the number of confidence levels in your experiment. Defaults to 6, implying the levels 1 (=lowest confidence),2,3,4,5,6 (=highest confidence)

# returnFit: a logical indicating whether you want to return the fit (1) or predictions given these parameters (0)

chi_square_optim <- function(params, observations, conf_levels=6, returnFit){
  sourceCpp("DDM_with_confidence_slow_fullconfRT.cpp") # for parallel use, call the simulation script again in the function
  
  #Some defaults
  z <- .5 #starting point; note that the mode is accuracy coded, so doesn't allow for starting point estimation currently
  ntrials <- 5000 #number of simulated data points
  dt <- .001 #precision of the simulation
  sigma <- 1 #within-trial noise, fixed at 1 (note that this is sometimes fixed at .1 in the Ratcliff tradition)
  
  # 1. generate predictions from the given parameters
  names(params) <- c('v','a','ter','vratio','add_mean','add_sd')
  
  #we're using the empirically observed confidence RTs for the simulations, stitching them X times to reach sufficient simulations
  rtcj_simuls <- rep(observations$rtcj,ceiling(ntrials/length(observations$rtcj))) 
  #Simulate data 
  predictions <- data.frame(DDM_with_confidence_slow_fullconfRT(v=params['v'],a=params['a'],ter=params['ter'],z=z,ntrials=length(rtcj_simuls),s=sigma,dt=dt,t2distribution=rtcj_simuls,postdriftmod=params['vratio']))
  
  #rt=predicted RT, resp=predicted resp, cor=predicted accuracy, raw_evidence2 = evidence at the time when confidence is quantified, rt2=predicted rtcj, cj=predicted cj
  names(predictions) <- c('rt','resp','cor','raw_evidence2','rtcj','cj') 
  
  # 2. Linear scaling of confidence, to transform them from a.u. units onto a 1-6 scale (note; scale can be changed by varying conf_levels)
  predictions$cj <- (predictions$cj / params['add_sd']) + params['add_mean'] 
  predictions$cj[predictions$cj<1] <- 1
  predictions$cj[predictions$cj>conf_levels] <- conf_levels
  
  #only round numbers
  predictions$cj <- round(predictions$cj)
  
  # if we're only simulating data, return the predictions
  if(returnFit==0){ 
    return(predictions[,c('rt','cor','cj','rtcj')])
    
    # If we are fitting the model, now we'll compare predictions to the observations and compute the fit between both 
  }else{ 
    
    # First, separate the data in correct and error trials
    c_observed <- observations[observations$cor == 1,]
    e_observed <- observations[observations$cor == 0,]
    
    # same for the predictions
    c_predicted <- predictions[predictions$cor == 1,]
    e_predicted <- predictions[predictions$cor == 0,]
    
    # Now, get the quantile RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
    c_quantiles <- quantile(c_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    e_quantiles <- quantile(e_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    
    # to combine correct and incorrect we scale the expected interquantile probability by the proportion of correct and incorect respectively
    prop_obs_c <- dim(c_observed)[1] / dim(observations)[1]
    prop_obs_e <- dim(e_observed)[1] / dim(observations)[1]
    
    c_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
    e_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
    obs_props <- c(c_obs_proportion,e_obs_proportion)
    
    # now, get the proportion of responses that fall between the observed quantiles when applied to the predicted data (scale by N?)
    c_pred_proportion <- c(
      sum(c_predicted$rt <= c_quantiles[1]),
      sum(c_predicted$rt <= c_quantiles[2]) - sum(c_predicted$rt <= c_quantiles[1]),
      sum(c_predicted$rt <= c_quantiles[3]) - sum(c_predicted$rt <= c_quantiles[2]),
      sum(c_predicted$rt <= c_quantiles[4]) - sum(c_predicted$rt <= c_quantiles[3]),
      sum(c_predicted$rt <= c_quantiles[5]) - sum(c_predicted$rt <= c_quantiles[4]),
      sum(c_predicted$rt > c_quantiles[5])
    ) / dim(predictions)[1]
    
    e_pred_proportion <- c(
      sum(e_predicted$rt <= e_quantiles[1]),
      sum(e_predicted$rt <= e_quantiles[2]) - sum(e_predicted$rt <= e_quantiles[1]),
      sum(e_predicted$rt <= e_quantiles[3]) - sum(e_predicted$rt <= e_quantiles[2]),
      sum(e_predicted$rt <= e_quantiles[4]) - sum(e_predicted$rt <= e_quantiles[3]),
      sum(e_predicted$rt <= e_quantiles[5]) - sum(e_predicted$rt <= e_quantiles[4]),
      sum(e_predicted$rt > e_quantiles[5])
    ) / dim(predictions)[1]
    pred_props <- c(c_pred_proportion,e_pred_proportion)
    
    # avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    pred_props[pred_props==0] <- .0000001
    
    # Now, do the same for confidence
    obs_props_cj <- rep(NA,conf_levels*2)
    for(i in 1:conf_levels){
      obs_props_cj[i] <- sum(c_observed$cj==i)/dim(observations)[1]
      obs_props_cj[i+conf_levels] <- sum(e_observed$cj==i)/dim(observations)[1]
    }
    
    # now, get the proportion of responses that fall between the observed quantiles when applied to the predicted data (scale by N?)
    pred_props_cj <- rep(NA,conf_levels*2)
    for(i in 1:conf_levels){
      pred_props_cj[i] <- sum(c_predicted$cj==i)/dim(predictions)[1]
      pred_props_cj[i+conf_levels] <- sum(e_predicted$cj==i)/dim(predictions)[1]
    }
    
    # avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    pred_props_cj[pred_props_cj==0] <- .0000001
    
    # Combine the quantiles for rts and cj
    obs_props <- c(obs_props,obs_props_cj)
    pred_props <- c(pred_props,pred_props_cj)
    
    # calculate chi square
    chiSquare = sum( ( (obs_props - pred_props) ^ 2) / pred_props )
    
    if (is.na(chiSquare)) {
      chiSquare = 1000000 # bad but quick solution to the NA's in objective function error... probably occurs when there are no errors...
    }
    return(chiSquare)
  }
}