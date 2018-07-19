
rm(list=ls())

#load packages
library(tidyverse)

#load trimmed data
load("../clean_data/trimmed_data_exp1.RData")

#format data for dmc
for(wave_ in c('early','late')){
  for(source_ in c('firstyear','community','mturk')){
  
  dmc_data = as.data.frame(
    trimmed_data %>%
      filter(response>-1,time>150,source==source_,wave==wave_) %>%
    mutate( s = as.factor(as.numeric(as.factor(subjectid))) ,
            St = factor(angle,levels=c(270,90),labels=c('left','right')),
            Coh = factor(coherence,levels=c(0.10,0.15,0.20,0.25),labels=c('c10','c15','c20','c25')),
            Emph = factor(emphasis),
            R = factor(response,levels=1:2,labels=c("LEFT","RIGHT")),
            RT = time/1000) %>%
    select(s,St,Coh,Emph,R,RT)
    ) 
                           

#source dmc
source("dmc/dmc.R") #Note the version that was used for this project was DMC-180518

#load LBA
load_model("lba","lba_B.R")

#--------------------------------
#set up model to fit
factors=list(St=c("left","right"),Coh=c("c10","c15","c20","c25"),Emph=c("accuracy","speed"))
responses=c("LEFT","RIGHT")
match.map=list(M=list(left="LEFT",right="RIGHT"))
consts <-c(sd_v=1,st0=0)
p.map=list(A="1",B=c("Emph"),mean_v=c("Coh","Emph","M"),sd_v="1",t0="1",st0="1")
model <- model.dmc(type="norm",constants=consts,p.map=p.map,
                   match.map=match.map,factors=factors,responses=responses)

data.model <- data.model.dmc(dmc_data, model)
#save(data.model,file="../model_output/data_and_model.RData")


#--------------------------------
#set priors
pop.mean <- c(rep(1,length(attr(model,"p.vector"))-1),0.2)
names(pop.mean) <- names(attr(model,"p.vector"))

pop.prior <- prior.p.dmc(
  dists = rep("tnorm",length(pop.mean)),
  p1=pop.mean,
  p2=c(rep(.1,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
       rep(.2,length(grep("mean_v",names(pop.mean)))),   
       rep(.1,length(grep("sd_v",names(pop.mean)))),
       rep(.05,length(grep("t0",names(pop.mean))))),
  lower=c(rep(0,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
          rep(NA,length(grep("mean_v",names(pop.mean)))),   
          rep(0,length(grep("sd_v",names(pop.mean)))),
          rep(.1,length(grep("t0",names(pop.mean))))),
  upper=c(rep(NA,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
          rep(NA,length(grep("mean_v",names(pop.mean)))),   
          rep(NA,length(grep("sd_v",names(pop.mean)))),
          rep(1,length(grep("t0",names(pop.mean)))))
)

mean.prior <- prior.p.dmc(
  dists = rep("tnorm",length(pop.mean)),
  p1=pop.mean,                           
  p2=c(rep(1,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
       rep(2,length(grep("mean_v",names(pop.mean)))),   
       rep(1,length(grep("sd_v",names(pop.mean)))),
       rep(1,length(grep("t0",names(pop.mean))))),
  lower=c(rep(0,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
          rep(NA,length(grep("mean_v",names(pop.mean)))),   
          rep(0,length(grep("sd_v",names(pop.mean)))),
          rep(.1,length(grep("t0",names(pop.mean))))),
  upper=c(rep(NA,length(grep("A",names(pop.mean)))+length(grep("B",names(pop.mean)))),
          rep(NA,length(grep("mean_v",names(pop.mean)))),   
          rep(NA,length(grep("sd_v",names(pop.mean)))),
          rep(1,length(grep("t0",names(pop.mean)))))
)


scale.prior <- prior.p.dmc(
  dists = rep("beta", length(pop.mean)),
  p1=(pop.mean>-1)*1, #hack to get p1 to be a vector of 1's with variable names
  p2=rep(1,length(pop.mean))
)

pp.prior <- list(mean.prior, scale.prior) 
#save(pp.prior,file="../model_output/priors.RData")

#-----------------------------
# Generate starting values

starting_samples <- h.samples.dmc(nmc=100,pop.prior,data.model,thin=10,pp.prior=pp.prior)
save(starting_samples,file=paste0("../model_output/starting_values_exp1_",source_,"_",wave_,".RData"))

  }
}
