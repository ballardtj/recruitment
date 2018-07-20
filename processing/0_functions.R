source("dmc/dmc.R")
load_model("lba","lba_B.R")
library(tidyverse)

get_pp_exp1 = function(converged_samples,wave_,source_,Nsamp=100,Nreps=50){

  collapse<-function(data,design){
    quant = data %>%
      group_by(s,St,Coh,Emph) %>%
      mutate(ncell=length(s)) %>%
      group_by(s,St,Coh,Emph,R) %>%
      summarise(nobs=length(s),
                prop=nobs/mean(ncell),
                q1=quantile(RT,0.1),
                q3=quantile(RT,0.3),
                q5=quantile(RT,0.5),
                q7=quantile(RT,0.7),
                q9=quantile(RT,0.9))
    #Remove quantiles from cells with fewer than five observations
    quant[quant$nobs<5,c('q1','q3','q5','q7','q9')]<-NA

    quant2=left_join(design,quant, by = c("s", "St", "Coh", "Emph", "R"))
    quant2$prop[is.na(quant2$prop)]<-0

    collapsed=quant2 %>%
      group_by(St,Coh,Emph,R) %>%
      summarise(prop.m=mean(prop),
                prop.upper=prop.m+sd(prop)/sqrt(length(R)),
                prop.lower=prop.m-sd(prop)/sqrt(length(R)),
                q1.m=mean(q1,na.rm=T),
                q1.upper=q1.m+sd(q1,na.rm=T)/sqrt(sum(!is.na(R))),
                q1.lower=q1.m-sd(q1,na.rm=T)/sqrt(sum(!is.na(R))),
                q3.m=mean(q3,na.rm=T),
                q3.upper=q3.m+sd(q3,na.rm=T)/sqrt(sum(!is.na(R))),
                q3.lower=q3.m-sd(q3,na.rm=T)/sqrt(sum(!is.na(R))),
                q5.m=mean(q5,na.rm=T),
                q5.upper=q5.m+sd(q5,na.rm=T)/sqrt(sum(!is.na(R))),
                q5.lower=q5.m-sd(q5,na.rm=T)/sqrt(sum(!is.na(R))),
                q7.m=mean(q7,na.rm=T),
                q7.upper=q7.m+sd(q7,na.rm=T)/sqrt(sum(!is.na(R))),
                q7.lower=q7.m-sd(q7,na.rm=T)/sqrt(sum(!is.na(R))),
                q9.m=mean(q9,na.rm=T),
                q9.upper=q9.m+sd(q9,na.rm=T)/sqrt(sum(!is.na(R))),
                q9.lower=q9.m-sd(q9,na.rm=T)/sqrt(sum(!is.na(R))))
    return(collapsed)
  }


  load("data/clean/trimmed_data_exp1.RData")

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

  Nsubj = length(converged_samples)

  full_design = expand.grid(s=factor(1:Nsubj),
                            St=unique(dmc_data$St),
                            Coh=unique(dmc_data$Coh),
                            Emph=unique(dmc_data$Emph),
                            R=c('LEFT','RIGHT'))

  observed_data=collapse(dmc_data,full_design)

  names(observed_data)[c(5,8,11,14,17,20)]<-c('prop.mid','q1.mid','q3.mid','q5.mid','q7.mid','q9.mid')
  observed_data$source='Data'


  #Generate Posterior Predictives
  model = attributes(converged_samples[[1]]$data)$model

  #Get sample parameter values for each subject
  mcmc.list=list()
  #use = matrix(NA,Nsubj,Nsamp)
  for(s in 1:Nsubj){
    mcmc.list[[s]]=as.matrix(theta.as.mcmc.list(converged_samples[[s]]))
  }
  use <- sample(1:dim(mcmc.list[[s]])[1],Nsamp)

  #for each iteration generate data based on the sampled value
  iter.list=list()
  ctr1=0
  # create progress bar
  #pb <- txtProgressBar(min = 0, max = Nsamp, style = 3)
  for(i in 1:Nsamp){
    p.mat = matrix(NA,Nsubj,dim(converged_samples[[1]]$theta)[2])
    colnames(p.mat) = dimnames(converged_samples[[1]]$theta)[[2]]
    rownames(p.mat) = c("p.vector",rep("",dim(p.mat)[1]-1))

    #create nsubject x nparameter matrix of sampled values for that iteration
    for(s in 1:Nsubj){
      p.mat[s,] = mcmc.list[[s]][use[i],]
    }

    #Simulate data based on parameter matrix for that iteration
    sim=h.simulate.dmc(model,ps=p.mat,ns=Nsubj,n=Nreps)

    #get RT quantile and choice proportion for each subject in each condition
    ctr1=ctr1+1
    iter.list[[ctr1]] <- collapse(sim,full_design) %>%
      mutate(source="Model",
             iter=ctr1)
    #setTxtProgressBar(pb, i) #increment progress bar
  }

  #create and save alternate posterior predictives object
  posterior_predictives_alt = bind_rows(iter.list)

  #calculate CIs by taking the quantiles of the different samples
  predicted_data = posterior_predictives_alt %>% group_by(St,Coh,Emph,R) %>%
    summarise(prop.mid = quantile(prop.m,0.5),
              prop.lower = quantile(prop.m,0.025),
              prop.upper = quantile(prop.m,0.975),
              q1.mid = quantile(q1.m,0.5,na.rm=T),
              q1.lower = quantile(q1.m,0.025,na.rm=T),
              q1.upper = quantile(q1.m,0.975,na.rm=T),
              q3.mid = quantile(q3.m,0.5,na.rm=T),
              q3.lower = quantile(q3.m,0.025,na.rm=T),
              q3.upper = quantile(q3.m,0.975,na.rm=T),
              q5.mid = quantile(q5.m,0.5,na.rm=T),
              q5.lower = quantile(q5.m,0.025,na.rm=T),
              q5.upper = quantile(q5.m,0.975,na.rm=T),
              q7.mid = quantile(q7.m,0.5,na.rm=T),
              q7.lower = quantile(q7.m,0.025,na.rm=T),
              q7.upper = quantile(q7.m,0.975,na.rm=T),
              q9.mid = quantile(q9.m,0.5,na.rm=T),
              q9.lower = quantile(q9.m,0.025,na.rm=T),
              q9.upper = quantile(q9.m,0.975,na.rm=T),
              source='Model')


  pp_data = rbind(observed_data,predicted_data) %>%
    ungroup() %>%
    mutate(Coh = factor(Coh,levels=c("c10","c15","c20","c25"),labels=c("Very Low","Low","High","Very High")),
           Emph = factor(Emph,levels=c("accuracy","speed"),labels=c("Accuracy","Speed")),
           St = factor(St,levels=c("left","right"),labels=c("Left","Right")))

  return(pp_data)
}



get_pp_exp2 = function(converged_samples,wave_,source_,Nsamp=100){

  collapse<-function(data,design){
    quant = data %>%
      group_by(s,St,Cont,Emph) %>%
      mutate(ncell=length(s)) %>%
      group_by(s,St,Cont,Emph,R) %>%
      summarise(nobs=length(s),
                prop=nobs/mean(ncell),
                q1=quantile(RT,0.1),
                q3=quantile(RT,0.3),
                q5=quantile(RT,0.5),
                q7=quantile(RT,0.7),
                q9=quantile(RT,0.9))
    #Remove quantiles from cells with fewer than five observations
    quant[quant$nobs<5,c('q1','q3','q5','q7','q9')]<-NA

    quant2=left_join(design,quant, by = c("s", "St", "Cont", "Emph", "R"))
    quant2$prop[is.na(quant2$prop)]<-0

    collapsed=quant2 %>%
      group_by(St,Cont,Emph,R) %>%
      summarise(prop.m=mean(prop),
                prop.upper=prop.m+sd(prop)/sqrt(length(R)),
                prop.lower=prop.m-sd(prop)/sqrt(length(R)),
                q1.m=mean(q1,na.rm=T),
                q1.upper=q1.m+sd(q1,na.rm=T)/sqrt(sum(!is.na(R))),
                q1.lower=q1.m-sd(q1,na.rm=T)/sqrt(sum(!is.na(R))),
                q3.m=mean(q3,na.rm=T),
                q3.upper=q3.m+sd(q3,na.rm=T)/sqrt(sum(!is.na(R))),
                q3.lower=q3.m-sd(q3,na.rm=T)/sqrt(sum(!is.na(R))),
                q5.m=mean(q5,na.rm=T),
                q5.upper=q5.m+sd(q5,na.rm=T)/sqrt(sum(!is.na(R))),
                q5.lower=q5.m-sd(q5,na.rm=T)/sqrt(sum(!is.na(R))),
                q7.m=mean(q7,na.rm=T),
                q7.upper=q7.m+sd(q7,na.rm=T)/sqrt(sum(!is.na(R))),
                q7.lower=q7.m-sd(q7,na.rm=T)/sqrt(sum(!is.na(R))),
                q9.m=mean(q9,na.rm=T),
                q9.upper=q9.m+sd(q9,na.rm=T)/sqrt(sum(!is.na(R))),
                q9.lower=q9.m-sd(q9,na.rm=T)/sqrt(sum(!is.na(R))))
    return(collapsed)
  }

  load("data/clean/trimmed_data_exp2.RData")

  dmc_data = as.data.frame(
    trimmed_data %>%
      filter(response>-1,time>150,source==source_,wave==wave_) %>%
      mutate( s = as.factor(as.numeric(as.factor(subjectid))) ,
              St = factor(1*(brightness<0.5),levels=0:1,labels=c('light','dark')),
              Cont = factor(abs(brightness-0.5),levels=c(0.02,0.03,0.04,0.05),labels=c('c02','c03','c04','c05')),
              Emph = factor(emphasis),
              R = factor(response,levels=1:2,labels=c("LIGHT","DARK")),
              RT = time/1000) %>%
      select(s,St,Cont,Emph,R,RT)
  )

  Nsubj = length(converged_samples)

  full_design = expand.grid(s=factor(1:Nsubj),
                            St=unique(dmc_data$St),
                            Cont=unique(dmc_data$Cont),
                            Emph=unique(dmc_data$Emph),
                            R=c('LIGHT','DARK'))

  observed_data=collapse(dmc_data,full_design)

  names(observed_data)[c(5,8,11,14,17,20)]<-c('prop.mid','q1.mid','q3.mid','q5.mid','q7.mid','q9.mid')
  observed_data$source='Data'


  #Generate Posterior Predictives
  model = attributes(converged_samples[[1]]$data)$model

  #Get sample parameter values for each subject
  mcmc.list=list()
  #use = matrix(NA,Nsubj,Nsamp)
  for(s in 1:Nsubj){
    mcmc.list[[s]]=as.matrix(theta.as.mcmc.list(converged_samples[[s]]))
  }
  use <- sample(1:dim(mcmc.list[[s]])[1],Nsamp)

  #for each iteration generate data based on the sampled value
  iter.list=list()
  ctr1=0
  # create progress bar
  #pb <- txtProgressBar(min = 0, max = Nsamp, style = 3)
  for(i in 1:Nsamp){
    p.mat = matrix(NA,Nsubj,dim(converged_samples[[1]]$theta)[2])
    colnames(p.mat) = dimnames(converged_samples[[1]]$theta)[[2]]
    rownames(p.mat) = c("p.vector",rep("",dim(p.mat)[1]-1))

    #create nsubject x nparameter matrix of sampled values for that iteration
    for(s in 1:Nsubj){
      p.mat[s,] = mcmc.list[[s]][use[i],]
    }

    #Simulate data based on parameter matrix for that iteration
    sim=h.simulate.dmc(model,ps=p.mat,ns=Nsubj,n=Nreps)

    #get RT quantile and choice proportion for each subject in each condition
    ctr1=ctr1+1
    iter.list[[ctr1]] <- collapse(sim,full_design) %>%
      mutate(source="Model",
             iter=ctr1)
    #setTxtProgressBar(pb, i) #increment progress bar
  }

  #create and save alternate posterior predictives object
  posterior_predictives_alt = bind_rows(iter.list)

  #calculate CIs by taking the quantiles of the different samples
  predicted_data = posterior_predictives_alt %>% group_by(St,Cont,Emph,R) %>%
    summarise(prop.mid = quantile(prop.m,0.5),
              prop.lower = quantile(prop.m,0.025),
              prop.upper = quantile(prop.m,0.975),
              q1.mid = quantile(q1.m,0.5,na.rm=T),
              q1.lower = quantile(q1.m,0.025,na.rm=T),
              q1.upper = quantile(q1.m,0.975,na.rm=T),
              q3.mid = quantile(q3.m,0.5,na.rm=T),
              q3.lower = quantile(q3.m,0.025,na.rm=T),
              q3.upper = quantile(q3.m,0.975,na.rm=T),
              q5.mid = quantile(q5.m,0.5,na.rm=T),
              q5.lower = quantile(q5.m,0.025,na.rm=T),
              q5.upper = quantile(q5.m,0.975,na.rm=T),
              q7.mid = quantile(q7.m,0.5,na.rm=T),
              q7.lower = quantile(q7.m,0.025,na.rm=T),
              q7.upper = quantile(q7.m,0.975,na.rm=T),
              q9.mid = quantile(q9.m,0.5,na.rm=T),
              q9.lower = quantile(q9.m,0.025,na.rm=T),
              q9.upper = quantile(q9.m,0.975,na.rm=T),
              source='Model')

  pp_data = rbind(observed_data,predicted_data) %>%
    ungroup() %>%
    mutate(Cont = factor(Cont,levels=c("c02","c03","c04","c05"),labels=c("Very Low","Low","High","Very High")),
           Emph = factor(Emph,levels=c("accuracy","speed"),labels=c("Accuracy","Speed")),
           St = factor(St,levels=c("light","dark"),labels=c("Light","Dark")))


  return(pp_data)
}



get_parms_exp1 = function(converged_samples){

  Nsubj = length(converged_samples)
  Nsamp = 10#dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]
  mcmc.list=list()
  use = matrix(NA,Nsubj,Nsamp)
  for(s in 1:Nsubj){
    mcmc.list[[s]]=data.frame(as.matrix(theta.as.mcmc.list(converged_samples[[s]])))
    mcmc.list[[s]]$s = s
    mcmc.list[[s]]$iter = 1:Nsamp
  }

  #calculate differences and sums of drift rates
  parameters = bind_rows(mcmc.list)

  mean_v = parameters %>%
    select(iter,s,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>%
    gather(key,value,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>% arrange(iter) %>%
    extract(col=key,into=c('Parm','Coh','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
    mutate(Emph = factor(Emph),
           Coh = factor(Coh),
           M = factor(M,levels=c("true","false"),labels=c("correct","incorrect"))) %>%
    group_by(iter,Coh,Emph,M) %>%
    summarise(value.mc.mean = mean(value)) %>%
    group_by(Coh,Emph,M) %>%
    summarise(mean_v.lower = quantile(value.mc.mean,0.025),
              mean_v.median = quantile(value.mc.mean,0.5),
              mean_v.upper = quantile(value.mc.mean,0.975))

  B = parameters %>%
    select(iter,s,B.accuracy:B.speed) %>%
    gather(key,value,B.accuracy:B.speed) %>% arrange(iter) %>%
    extract(col=key,into=c('Parm','Emph'),regex="(.+)\\.(.+)") %>%
    mutate(Emph = factor(Emph)) %>%
    group_by(iter,Emph) %>%
    summarise(value.mc.mean = mean(value)) %>%
    group_by(Emph) %>%
    summarise(mean_v.lower = quantile(value.mc.mean,0.025),
              mean_v.median = quantile(value.mc.mean,0.5),
              mean_v.upper = quantile(value.mc.mean,0.975))

  t0 = parameters %>%
    select(iter,s,t0) %>%
    group_by(iter) %>%
    summarise(value.mc.mean = mean(t0)) %>%
    ungroup() %>%
    summarise(mean_v.lower = quantile(value.mc.mean,0.025),
              mean_v.median = quantile(value.mc.mean,0.5),
              mean_v.upper = quantile(value.mc.mean,0.975))

  A = parameters %>%
    select(iter,s,A) %>%
    group_by(iter) %>%
    summarise(value.mc.mean = mean(A)) %>%
    ungroup() %>%
    summarise(mean_v.lower = quantile(value.mc.mean,0.025),
              mean_v.median = quantile(value.mc.mean,0.5),
              mean_v.upper = quantile(value.mc.mean,0.975))

  parms = list(mean_v,B,t0,A)
  return(parms)
}

