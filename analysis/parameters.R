#clear workspace
rm(ls=list())

#load libraries
library(tidyverse)
library(latex2exp)
library(gridExtra)
source('dmc/dmc.R')

##############################
### Experiment 1 (Study 2) ###
##############################

#Create list of parameters values fof each participant group
diff_v_ls = list() #difference in drift rate
B_ls = list()      #threshold (averaged across responses)
t0_ls = list()     #non-decision time

#loop through participant groups
ctr=0
for (wave in c('early','late')){
  for (group in c('firstyear','community','mturk')){
    ctr=ctr+1
    load(paste0("data/derived/final_samples_exp1_",group,"_",wave,".RData"))

    #get samples
    Nsubj = length(converged_samples)
    Nsamp = 100#dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]
    mcmc.list=list()
    draws = sample(1:(dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]),size=Nsamp) #need to edit this if we only want to sample from final n draws
    for(s in 1:Nsubj){
      mcmc.list[[s]]=data.frame(as.matrix(theta.as.mcmc.list(converged_samples[[s]]))[draws,])
      mcmc.list[[s]]$s = s
      mcmc.list[[s]]$iter = 1:Nsamp
    }
    parameters = bind_rows(mcmc.list)

    #calculate differences and sums of drift rates
    diff_v_tmp = parameters %>%
      select(iter,s,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>%
      gather(key,value,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>% arrange(iter) %>%
      extract(col=key,into=c('Parm','Coh','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
      spread(key=M,value=value) %>%
      mutate(time_of_semester = wave,
             population = group)
    #  mutate(Emph = factor(Emph),
    #         Coh = factor(Coh),
    #         value = true-false) #%>%
     # group_by(iter,Coh,Emph) %>%
    #  summarise(value.mean = mean(value),
     #           time_of_semester = wave,
    #            population = group)



    #%>%
    #group_by(Coh,Emph) %>%
    #summarise(mean_v.lower = quantile(value.mc.mean,0.025),
    #          mean_v.median = quantile(value.mc.mean,0.5),
    #          mean_v.upper = quantile(value.mc.mean,0.975))


    B_tmp = parameters %>%
      select(iter,s,B.accuracy.LEFT:B.speed.RIGHT) %>%
      gather(key,value,B.accuracy.LEFT:B.speed.RIGHT) %>% arrange(iter) %>%
      extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
      spread(key=R,value=value) %>%
      mutate(time_of_semester = wave,
             population = group)

    #%>%
     # mutate(Emph = factor(Emph),
    #         value = (LEFT+RIGHT)/2) %>%
    #  group_by(iter,Emph) %>%
    #  summarise(value.mean = mean(value),
    #            time_of_semester = wave,
    #            population = group)

    t0_tmp = parameters %>%
      select(iter,s,t0) %>%
      mutate(time_of_semester = wave,
             population = group)#%>%
     # group_by(iter) %>%
     # summarise(value.mean = mean(t0),
    #            time_of_semester = wave,
     #           population = group)

    diff_v_ls[[ctr]] = diff_v_tmp
    B_ls[[ctr]] = B_tmp
    t0_ls[[ctr]] = t0_tmp

  }
}

#collapse lists into data frame
diff_v = bind_rows(diff_v_ls) %>%
  ungroup() %>%
  mutate(difficulty = factor(Coh,levels=c('c25','c20','c15','c10'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

diff_v_subj = diff_v %>%
  group_by(s,difficulty,emphasis,population,time_of_semester) %>%
  summarise(value = mean(true-false))

diff_v_plot = diff_v %>%
  group_by(iter,difficulty,emphasis,population,time_of_semester) %>%
  summarise(value = mean(true-false)) %>%
  ggplot(aes(x=difficulty,y=value,colour=emphasis)) +
  #geom_jitter(data=diff_v_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Quality of Information ($\\v_{correct}-v_{incorrect}$)'),x='Difficulty',colour='Emphasis') +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(time_of_semester~population) + theme(legend.position = 'none')

#Threshold
B = bind_rows(B_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         value = (LEFT+RIGHT)/2)

B_subj = B %>%
  group_by(s,emphasis,population,time_of_semester) %>%
  summarise(value = mean(value))

B_plot = B %>%
  group_by(iter,emphasis,population,time_of_semester) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x=population,y=value,colour=emphasis)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Threshold ($B$)'),x='Population',colour="Emphasis") +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(.~time_of_semester) + theme(legend.position = 'bottom')


#Non decision time
t0 = bind_rows(t0_ls) %>%
  ungroup() %>%
  mutate(population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early','Late')))

t0_subj = t0 %>%
  group_by(s,population,time_of_semester) %>%
  summarise(value = mean(t0))

t0_plot = t0 %>%
  group_by(iter,population,time_of_semester) %>%
  summarise(value = mean(t0)) %>%
  ggplot(aes(x=population,y=value,colour=time_of_semester)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Non-Decision Time ($t_0$)'),x='Population',colour="Time of Semester") +
  theme(legend.position='bottom') +
  scale_colour_manual(values=c('orange','forestgreen'))
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  #facet_grid(.~population)

#Combined into column of plots

exp1_panel = arrangeGrob(
  diff_v_plot,
  arrangeGrob(
    B_plot,
    t0_plot,
    nrow=1,
    widths=c(1.8,1)
    #top="Study 2 (Motion Discrimination)"
  ),
  top="Study 2 (Motion Discrimination)"
)


##############################
### Experiment 2 (Study 3) ###
##############################

#Create list of parameters values fof each participant group
diff_v_ls = list() #difference in drift rate
B_ls = list()      #threshold (averaged across responses)
t0_ls = list()     #non-decision time

#loop through participant groups
ctr=0
for (wave in c('early','late')){
  for (group in c('firstyear','community','mturk')){
    ctr=ctr+1
    load(paste0("data/derived/final_samples_exp2_",group,"_",wave,".RData"))

    #get samples
    Nsubj = length(converged_samples)
    Nsamp = 100#dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]
    mcmc.list=list()
    draws = sample(1:(dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]),size=Nsamp) #need to edit this if we only want to sample from final n draws
    for(s in 1:Nsubj){
      mcmc.list[[s]]=data.frame(as.matrix(theta.as.mcmc.list(converged_samples[[s]]))[draws,])
      mcmc.list[[s]]$s = s
      mcmc.list[[s]]$iter = 1:Nsamp
    }
    parameters = bind_rows(mcmc.list)

    #calculate differences and sums of drift rates
    diff_v_tmp = parameters %>%
      select(iter,s,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>%
      gather(key,value,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>% arrange(iter) %>%
      extract(col=key,into=c('Parm','Cont','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
      spread(key=M,value=value) %>%
      mutate(time_of_semester = wave,
             population = group)
    #  mutate(Emph = factor(Emph),
    #         Coh = factor(Coh),
    #         value = true-false) #%>%
    # group_by(iter,Coh,Emph) %>%
    #  summarise(value.mean = mean(value),
    #           time_of_semester = wave,
    #            population = group)



    #%>%
    #group_by(Coh,Emph) %>%
    #summarise(mean_v.lower = quantile(value.mc.mean,0.025),
    #          mean_v.median = quantile(value.mc.mean,0.5),
    #          mean_v.upper = quantile(value.mc.mean,0.975))


    B_tmp = parameters %>%
      select(iter,s,B.accuracy.LIGHT:B.speed.DARK) %>%
      gather(key,value,B.accuracy.LIGHT:B.speed.DARK) %>% arrange(iter) %>%
      extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
      spread(key=R,value=value) %>%
      mutate(time_of_semester = wave,
             population = group)

    #%>%
    # mutate(Emph = factor(Emph),
    #         value = (LEFT+RIGHT)/2) %>%
    #  group_by(iter,Emph) %>%
    #  summarise(value.mean = mean(value),
    #            time_of_semester = wave,
    #            population = group)

    t0_tmp = parameters %>%
      select(iter,s,t0) %>%
      mutate(time_of_semester = wave,
             population = group)#%>%
    # group_by(iter) %>%
    # summarise(value.mean = mean(t0),
    #            time_of_semester = wave,
    #           population = group)

    diff_v_ls[[ctr]] = diff_v_tmp
    B_ls[[ctr]] = B_tmp
    t0_ls[[ctr]] = t0_tmp

  }
}

#collapse lists into data frame
diff_v = bind_rows(diff_v_ls) %>%
  ungroup() %>%
  mutate(difficulty = factor(Cont,levels=c('c05','c04','c03','c02'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

diff_v_subj = diff_v %>%
  group_by(s,difficulty,emphasis,population,time_of_semester) %>%
  summarise(value = mean(true-false))

diff_v_plot = diff_v %>%
  group_by(iter,difficulty,emphasis,population,time_of_semester) %>%
  summarise(value = mean(true-false)) %>%
  ggplot(aes(x=difficulty,y=value,colour=emphasis)) +
  #geom_jitter(data=diff_v_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Quality of Information ($\\v_{correct}-v_{incorrect}$)'),x='Difficulty',colour='Emphasis') +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(time_of_semester~population) + theme(legend.position = 'none')

#Threshold
B = bind_rows(B_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         value = (LEFT+RIGHT)/2)

B_subj = B %>%
  group_by(s,emphasis,population,time_of_semester) %>%
  summarise(value = mean(value))

B_plot = B %>%
  group_by(iter,emphasis,population,time_of_semester) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x=population,y=value,colour=emphasis)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Threshold ($B$)'),x='Population',colour="Emphasis") +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(.~time_of_semester) + theme(legend.position = 'bottom')


#Non decision time
t0 = bind_rows(t0_ls) %>%
  ungroup() %>%
  mutate(population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early','Late')))

t0_subj = t0 %>%
  group_by(s,population,time_of_semester) %>%
  summarise(value = mean(t0))

t0_plot = t0 %>%
  group_by(iter,population,time_of_semester) %>%
  summarise(value = mean(t0)) %>%
  ggplot(aes(x=population,y=value,colour=time_of_semester)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Non-Decision Time ($t_0$)'),x='Population',colour="Time of Semester") +
  theme(legend.position='bottom') +
  scale_colour_manual(values=c('orange','forestgreen'))
#scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
#facet_grid(.~population)

#Combined into column of plots

exp2_panel = arrangeGrob(
  diff_v_plot,
  arrangeGrob(
    B_plot,
    t0_plot,
    nrow=1,
    widths=c(1.8,1)
    #top="Study 2 (Motion Discrimination)"
  ),
  top="Study 3 (Brightness Discrimination)"
)

dev.off()
parameter_plot = grid.arrange(
  exp1_panel,
  exp2_panel,
  nrow=2
)

ggsave(file=paste0("figures/parameters.pdf"),plot=parameter_plot,height=34,width=26,units="cm")



#####################################
### Bayes Factors for comparisons ###
#####################################

### Comparison 1: pairwise comparison between each participant group ###

load("data/derived/final_samples_exp1_firstyear_early.RData")


load("data/derived/final_samples_exp1_firstyear_late.RData")



###### calculate prior density by sampling from prior #######

#number of samples from the prior
nsamples = 1E5

#for each subject, sample from prior for threshold and drift parameters
prior_list = list()
for(i in 1:length(converged_samples)){
  print(i)
  priors_tmp = data.frame(
    B.accuracy.LIGHT = rtnorm(n = nsamples,
                         mean=rtnorm(n=nsamples,mean=1,sd=1,lower=0,upper=Inf),
                         sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf),
                         lower = 0,
                         upper = Inf),
    B.speed.LIGHT = rtnorm(n = nsamples,
                         mean=rtnorm(n=nsamples,mean=1,sd=1,lower=0,upper=Inf),
                         sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf),
                         lower = 0,
                         upper = Inf),
    B.accuracy.DARK = rtnorm(n = nsamples,
                              mean=rtnorm(n=nsamples,mean=1,sd=1,lower=0,upper=Inf),
                             sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf),
                              lower = 0,
                              upper = Inf),
    B.speed.DARK = rtnorm(n = nsamples,
                           mean=rtnorm(n=nsamples,mean=1,sd=1,lower=0,upper=Inf),
                          sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf),
                           lower = 0,
                           upper = Inf),
    mean_v.c02.accuracy.true = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.03.accuracy.true = rnorm(n = nsamples,
                                 mean=rnorm(n=nsamples,mean=1,sd=2),
                                 sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.04.accuracy.true = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.05.accuracy.true = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.c02.speed.true = rnorm(n = nsamples,
                                     mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.03.speed.true = rnorm(n = nsamples,
                                 mean=rnorm(n=nsamples,mean=1,sd=2),
                                 sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.04.speed.true = rnorm(n = nsamples,
                                    mean=rnorm(n=nsamples,mean=1,sd=2),
                                    sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.05.speed.true = rnorm(n = nsamples,
                                    mean=rnorm(n=nsamples,mean=1,sd=2),
                                    sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.c02.accuracy.false = rnorm(n = nsamples,
                                     mean=rnorm(n=nsamples,mean=1,sd=2),
                                     sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.03.accuracy.false = rnorm(n = nsamples,
                                    mean=rnorm(n=nsamples,mean=1,sd=2),
                                    sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.04.accuracy.false = rnorm(n = nsamples,
                                    mean=rnorm(n=nsamples,mean=1,sd=2),
                                    sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.05.accuracy.false = rnorm(n = nsamples,
                                    mean=rnorm(n=nsamples,mean=1,sd=2),
                                    sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.c02.speed.false = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.03.speed.false = rnorm(n = nsamples,
                                 mean=rnorm(n=nsamples,mean=1,sd=2),
                                 sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.04.speed.false = rnorm(n = nsamples,
                                 mean=rnorm(n=nsamples,mean=1,sd=2),
                                 sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    mean_v.05.speed.false = rnorm(n = nsamples,
                                 mean=rnorm(n=nsamples,mean=1,sd=2),
                                 sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf)),
    t0 = rtnorm(n = nsamples,
                mean=rtnorm(n=nsamples,mean=0.2,sd=1),
                sd=rtnorm(n=nsamples,mean=0,sd=1,lower=0,upper=Inf),
                lower = 0.1,
                upper = 1)
  )
  priors_tmp$s = i
  priors_tmp$iter = 1:nsamples
  prior_list[[i]] = priors_tmp
}

    mean_v.hard.loss.true = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=runif(n=nsamples,min=0,max=1)),
    mean_v.easy.none.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1)),
    mean_v.med.none.false = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=runif(n=nsamples,min=0,max=1)),
    mean_v.hard.none.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1)),
    mean_v.easy.gain.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1)),
    mean_v.med.gain.false = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=runif(n=nsamples,min=0,max=1)),
    mean_v.hard.gain.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1)),
    mean_v.easy.loss.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1)),
    mean_v.med.loss.false = rnorm(n = nsamples,
                                  mean=rnorm(n=nsamples,mean=1,sd=2),
                                  sd=runif(n=nsamples,min=0,max=1)),
    mean_v.hard.loss.false = rnorm(n = nsamples,
                                   mean=rnorm(n=nsamples,mean=1,sd=2),
                                   sd=runif(n=nsamples,min=0,max=1))

  )
  priors_tmp$s = i
  priors_tmp$iter = 1:nsamples
  prior_list[[i]] = priors_tmp
  print(i)
}





#posterior
emph = diff_v %>%
  group_by(iter,s,Emph) %>%
  summarise(value = mean(true-false)) %>%
  spread(Emph,value) %>%
  group_by(iter) %>%
  summarise(value = mean(accuracy-speed))

d_posterior = approxfun(density(emph$value),rule=2)
d_posterior(0)

behavioural_figure <- grid.arrange(
  arrangeGrob(
    arrangeGrob(
      motion_accuracy,
      motion_rt,
      nrow=2,
      top="Study 2 (Motion Discrimination)"
    ),
    arrangeGrob(
      brightness_accuracy,
      brightness_rt  + theme(legend.position="none"),
      nrow=2,
      top="Study 3 (Brightness Discrimination)"
    ),
    nrow=1,ncol=2)#,
  #legend, nrow=2,heights=c(20, 1))




  B = bind_rows(B_ls) %>% ungroup()
  t0 = bind_rows(t0_ls) %>% ungroup()

ggplot(data=pp_smry,aes(x=Coh,y=prop_m,group=R,colour=source)) +
  geom_errorbar(aes(ymax = prop_u, ymin = prop_l), width= 0.2) +
  geom_point(pch=21, size=2) +
  geom_line(aes(group=interaction(R,source))) +
  ylab("Response Proportion") + xlab('Difficulty') +
  scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(Emph~St) #+ theme_minimal()


         source = factor(source,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         wave = factor(wave,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         difficulty = factor(coherence,levels=c(0.25,0.2,0.15,0.1),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(emphasis,levels=c('speed','accuracy'),labels=c('Speed','Accuracy'))) %>%





mutate(Coh = factor(Coh,levels=c('c25','c20','c15','c10'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
       St = factor(St,levels=c('left','right'),labels=c('Leftward Motion','Rightward Motion')),
       Emph = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed Emphasis','Accuracy Emphasis')))




