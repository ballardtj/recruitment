rm(list=ls())

###read in the arguments listed at the command line
args <- commandArgs(trailingOnly = F)

print(args)

i <- args[length(args)]
i <- strsplit(i,"--")[[1]][2]
i <- as.numeric(i)

print(i)

if(i<4){
  wave_ = 'early'
}
if(i>3){
  wave_ = 'late'
}
if(i==1|i==4){
  source_ = 'firstyear'
}
if(i==2|i==5){
  source_ = 'community'
}
if(i==3|i==6){
  source_ = 'mturk'
}

ncores = parallel::detectCores()

#set working directory
setwd("~/DMC/DCM-Sampling/analysis/")

#load packages
library(tidyverse)

#source dmc
source("dmc/dmc.R") #Note the version that was used for this project was DMC-180518

#load LBA
load_model("LBA","lba_B.R")

#load starting values
load(paste0("../model_output/starting_values_exp1_",source_,"_",wave_,".RData"))

#Gets rid of bad chains
system.time({unstuck_samples  <- h.run.unstuck.dmc(starting_samples, p.migrate = .05,h.p.migrate=.05, cores = ncores)})

#Runs until gelman diag is below 1.1 for each parameter, and then gets a fresh 100 samples
system.time({converged_samples <- h.run.converge.dmc(h.samples.dmc(nmc=100, samples=unstuck_samples), 
                                                 nmc=100,cores=ncores,finalrun=TRUE,finalI=100)})

save(converged_samples,file=paste0("../model_output/final_samples_exp1_",source_,"_",wave_,".RData"))



