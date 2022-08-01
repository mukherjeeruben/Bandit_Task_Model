library(hBayesDM)
library(rstan)
library(bayesplot)
library(shinystan)
library(ggplot2)
library(cowplot)
library(abind)
library(dplyr)
library(ggpubr)
library(tidyr)
source("utils/fit_basic_model_functions.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
dataPath = system.file("C:/Users/PIYUSH/AppData/Local/R/win-library/4.2/hBayesDM/extdata/gng_exampleData.txt", package="hBayesDM")
data1 = read.csv("E:/MSc.Computing/CA681PracticumAI/Lili_Models/clean_data/voice_dataset_clean.csv")
data2 = read.csv('C:/Users/PIYUSH/Documents/Bandit_task/bandit_task_server/extracted_data/voice_dataset_clean.csv')
data3 = read.csv('C:/Users/PIYUSH/Documents/Bandit_task/bandit_task_server/extracted_data/conventional_dataset_clean.csv')
output = gng_m1(data="example", niter=2000, nwarmup=1000, nchain=4, ncore=4)
output1 = bandit2arm_delta(
  data = data1,
  niter = 4000,
  nwarmup = 1000,
  nchain = 4,
  ncore = 1,
  nthin = 1,
  inits = "vb",
  indPars = "mean",
  modelRegressor = FALSE,
  vb = FALSE,
  inc_postpred = FALSE,
  adapt_delta = 0.95,
  stepsize = 1,
  max_treedepth = 10
)
plot(output1, type="trace", fontSize=11)
#Check the Rhat Values
rhat(output1)
plot(output1)
printFit(output1)
output2 = bandit2arm_delta(
  data = data3,
  niter = 4000,
  nwarmup = 1000,
  nchain = 4,
  ncore = 1,
  nthin = 1,
  inits = "vb",
  indPars = "mean",
  modelRegressor = FALSE,
  vb = FALSE,
  inc_postpred = FALSE,
  adapt_delta = 0.95,
  stepsize = 1,
  max_treedepth = 10
)
plot(output2, type="trace", fontSize=11)
plot(output1$parVals$mu_A,type='hist')
plot(output2)
printFit(output1)
