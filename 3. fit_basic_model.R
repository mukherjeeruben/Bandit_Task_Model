rm(list=ls())
# setwd("~/Documents/rstudio/conspiracy")
library(rstan)
library(bayesplot)
library(shinystan)
library(ggplot2)
library(cowplot)
library(abind)
library(dplyr)
library(ggpubr)
library(tidyr)
library(readxl)
#source("utils/fit_basic_model_functions.R")
source("utils/new_basic_model_functions.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#schedule1_stable = fit_model("rat1_stable")
#schedule1_volatile = fit_model("rat1_volatile")
Schedule_voice = fit_model("voice_dataset_clean")
schedule_conventional = fit_model("conventional_dataset_clean")
#schedule2_stable = fit_model("rat2_stable")
#schedule2_volatile = fit_model("rat2_volatile")
# #raw_data <- data.table::fread(file = paste0("clean_data/", 'voice_dataset_clean', ".csv"),
#                              # header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE, 
#                               #stringsAsFactors = TRUE, logical01 = FALSE)
# raw_data <- raw_data %>% drop_na()
# data_list <- frame2list(raw_data)
# fit <- stan(
#   file = "stan_files/bandit2arm_delta.stan",
#   data = data_list,
#   iter = 4000,
#   warmup = 1000,
#   chains = 4
# )
