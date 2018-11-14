#clear workspace
rm(list=ls())

#load libraries
library(tidyverse)
library(latex2exp)
library(gridExtra)
library(reshape2)
source('dmc/dmc.R')

##############################
### Experiment 1 (Study 2) ###
##############################

#Create list object for storing individual level parameters
diff_v_ind_ls = list() #difference in drift rate
B_ind_ls = list()      #threshold (averaged across responses)
t0_ind_ls = list()     #non-decision time

#Create list object for storing hyperparameters
diff_v_hyp_ls = list() #difference in drift rate
B_hyp_ls = list()      #threshold (averaged across responses)
t0_hyp_ls = list()     #non-decision time

#loop through participant groups
ctr=0
for(exp in 1:2){
  for (wave in c('early','late')){
    for (group in c('firstyear','community','mturk')){
      ctr=ctr+1
      print(ctr)
      load(paste0("data/derived/final_samples_exp",exp,"_",group,"_",wave,".RData"))

      #get individual level parameter samples
      Nsubj = length(converged_samples)
      Nsamp = 1000#dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]
      mcmc.list=list()
      draws = sample(1:(dim(converged_samples[[1]]$theta)[1]*dim(converged_samples[[1]]$theta)[3]),size=Nsamp) #need to edit this if we only want to sample from final n draws
      for(s in 1:Nsubj){
        #print(s)
        mcmc.list[[s]]=data.frame(as.matrix(theta.as.mcmc.list(converged_samples[[s]]))[draws,])
        mcmc.list[[s]]$s = s
        mcmc.list[[s]]$iter = 1:Nsamp
      }
      parameters = bind_rows(mcmc.list)

      if(exp==1){
        diff_v_tmp_ind = parameters %>%
          select(iter,s,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>%
          gather(key,value,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Coh','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
          spread(key=M,value=value) %>%
          mutate(value = true-false,
                 difficulty = factor(Coh,levels=c('c25','c20','c15','c10'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
                 time_of_semester = wave,
                 population = group,
                 exp=1) %>%
          select(iter,s,Emph,difficulty,time_of_semester,population,exp,value)

        B_tmp_ind = parameters %>%
          select(iter,s,B.accuracy.LEFT:B.speed.RIGHT) %>%
          gather(key,value,B.accuracy.LEFT:B.speed.RIGHT) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
          spread(key=R,value=value) %>%
          mutate(value=(LEFT+RIGHT)/2,
                 time_of_semester = wave,
                 population = group,
                 exp=1) %>%
          select(iter,s,Emph,time_of_semester,population,exp,value)
      }
      if(exp==2){
        diff_v_tmp_ind = parameters %>%
          select(iter,s,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>%
          gather(key,value,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Cont','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
          spread(key=M,value=value) %>%
          mutate(value = true-false,
                 difficulty = factor(Cont,levels=c('c05','c04','c03','c02'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
                 time_of_semester = wave,
                 population = group,
                 exp=2) %>%
          select(iter,s,Emph,difficulty,time_of_semester,population,exp,value)

        B_tmp_ind = parameters %>%
          select(iter,s,B.accuracy.LIGHT:B.speed.DARK) %>%
          gather(key,value,B.accuracy.LIGHT:B.speed.DARK) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
          spread(key=R,value=value) %>%
          mutate(value=(LIGHT+DARK)/2,
                 time_of_semester = wave,
                 population = group,
                 exp=2) %>%
          select(iter,s,Emph,time_of_semester,population,exp,value)

      }

      t0_tmp_ind = parameters %>%
        select(iter,s,t0) %>%
        mutate(time_of_semester = wave,
               population = group,
               exp=exp)

      diff_v_ind_ls[[ctr]] = diff_v_tmp_ind
      B_ind_ls[[ctr]] = B_tmp_ind
      t0_ind_ls[[ctr]] = t0_tmp_ind


      #get population mean parameter samples
      hyper_mean_array = attr(converged_samples,'hyper')[[1]][[1]]
      hyperparameters = data.frame(apply(hyper_mean_array,2,rbind)) %>%
        mutate(iter = 1:n())

      if(exp == 1){
        diff_v_tmp_hyp = hyperparameters %>%
          select(iter,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>%
          gather(key,value,mean_v.c10.accuracy.true:mean_v.c25.speed.false) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Coh','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
          spread(key=M,value=value) %>%
          mutate(value = true-false,
                 difficulty = factor(Coh,levels=c('c25','c20','c15','c10'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
                 time_of_semester = wave,
                 population = group,
                 exp=1) %>%
          select(iter,Emph,difficulty,time_of_semester,population,exp,value)

        B_tmp_hyp = hyperparameters %>%
          select(iter,B.accuracy.LEFT:B.speed.RIGHT) %>%
          gather(key,value,B.accuracy.LEFT:B.speed.RIGHT) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
          spread(key=R,value=value) %>%
          mutate(value=(LEFT+RIGHT)/2,
                 time_of_semester = wave,
                 population = group,
                 exp=1) %>%
          select(iter,Emph,time_of_semester,population,exp,value)
      }
      if(exp == 2){
        diff_v_tmp_hyp = hyperparameters %>%
          select(iter,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>%
          gather(key,value,mean_v.c02.accuracy.true:mean_v.c05.speed.false) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Cont','Emph',"M"),regex="(.+)\\.(.+)\\.(.+)\\.(.+)") %>%
          spread(key=M,value=value) %>%
          mutate(value = true-false,
                 difficulty = factor(Cont,levels=c('c05','c04','c03','c02'),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
                 time_of_semester = wave,
                 population = group,
                 exp=2) %>%
          select(iter,Emph,difficulty,time_of_semester,population,exp,value)

        B_tmp_hyp = hyperparameters %>%
          select(iter,B.accuracy.LIGHT:B.speed.DARK) %>%
          gather(key,value,B.accuracy.LIGHT:B.speed.DARK) %>% arrange(iter) %>%
          extract(col=key,into=c('Parm','Emph','R'),regex="(.+)\\.(.+)\\.(.+)") %>%
          spread(key=R,value=value) %>%
          mutate(value=(LIGHT+DARK)/2,
                 time_of_semester = wave,
                 population = group,
                 exp=2) %>%
          select(iter,Emph,time_of_semester,population,exp,value)
      }

      t0_tmp_hyp = hyperparameters %>%
        select(iter,t0) %>%
        mutate(time_of_semester = wave,
               population = group,
               exp=exp)

      diff_v_hyp_ls[[ctr]] = diff_v_tmp_hyp
      B_hyp_ls[[ctr]] = B_tmp_hyp
      t0_hyp_ls[[ctr]] = t0_tmp_hyp

    }
  }
}

#remove big objects from workspace
rm(mcmc.list)
rm(converged_samples)
rm(parameters)
rm(hyperparameters)


#collapse lists into data frame
diff_v_hyp = bind_rows(diff_v_hyp_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

rm(diff_v_hyp_ls)

diff_v_ind = bind_rows(diff_v_ind_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

rm(diff_v_ind_ls)

B_hyp = bind_rows(B_hyp_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

rm(B_hyp_ls)

B_ind = bind_rows(B_ind_ls) %>%
  ungroup() %>%
  mutate(emphasis = factor(Emph,levels=c('speed','accuracy'),labels=c('Speed','Accuracy')),
         population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')))

rm(B_ind_ls)

t0_hyp = bind_rows(t0_hyp_ls) %>%
  ungroup() %>%
  mutate(population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early','Late')))

rm(t0_hyp_ls)

t0_ind = bind_rows(t0_ind_ls) %>%
  ungroup() %>%
  mutate(population = factor(population,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early','Late')))

rm(t0_ind_ls)

######################
#### CREATE PLOTS ####
######################

## Experiment 1 ##

#Drift rates
diff_v_plot = diff_v_hyp %>%
  filter(exp==1) %>%
  ggplot(aes(x=difficulty,y=value,colour=emphasis)) +
  #geom_jitter(data=diff_v_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Quality of Information ($\\v_{correct}-v_{incorrect}$)'),x='Difficulty',colour='Emphasis') +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(time_of_semester~population) + theme(legend.position = 'none')

#Threshold
B_plot = B_hyp %>%
  filter(exp==1) %>%
  ggplot(aes(x=population,y=value,colour=emphasis)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Threshold ($B$)'),x='Population',colour="Emphasis") +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(.~time_of_semester) + theme(legend.position = 'bottom')

#Non decision time
t0_plot = t0_hyp %>%
  filter(exp==1) %>%
  ggplot(aes(x=population,y=t0,colour=time_of_semester)) +
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

## Experiment 2 ##

#Draft Rates
diff_v_plot = diff_v_hyp %>%
  filter(exp==2) %>%
  ggplot(aes(x=difficulty,y=value,colour=emphasis)) +
  #geom_jitter(data=diff_v_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Quality of Information ($\\v_{correct}-v_{incorrect}$)'),x='Difficulty',colour='Emphasis') +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(time_of_semester~population) + theme(legend.position = 'none')

#Threshold
B_plot = B_hyp %>%
  filter(exp==2) %>%
  ggplot(aes(x=population,y=value,colour=emphasis)) +
  #geom_jitter(data=B_subj,alpha=0.15) +
  geom_violin() +
  labs(y=TeX('Threshold ($B$)'),x='Population',colour="Emphasis") +
  #scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1)) +
  facet_grid(.~time_of_semester) + theme(legend.position = 'bottom')


#Non decision time
t0_plot = t0_hyp %>%
  filter(exp==2) %>%
  ggplot(aes(x=population,y=t0,colour=time_of_semester)) +
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

ggsave(file=paste0("figures/parameters2.pdf"),plot=parameter_plot,height=34,width=26,units="cm")


#####################################
### Bayes Factors for comparisons ###
#####################################

results = matrix(NA,30,12)

##### ANALYSIS OF DRIFT RATES #####

col=c(1,7)

### Comparison 1: pairwise comparison between each participant population (averaged across early vs late and within subjects manipulations) ###

#get prior density at 0 for these comparisons
prior = apply( matrix(rnorm(n=16*100000,mean=1,sd=2),ncol=16) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rnorm(n=16*100000,mean=1,sd=2),ncol=16) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across early vs late group and the experimental manipulations
diff_v_tmp = diff_v_hyp %>%
  group_by(exp,iter,population) %>%
  summarise(value = mean(value)) %>% #average across 8 different difficulty x emphasis conditions x 2 time of semester groups (so 16 posteriors being averaged)
  spread(population,value)

for(exp in 1:2){

  #local credit vs local paid
  posterior = diff_v_tmp[diff_v_tmp$exp==exp,'Local, Credit'] - diff_v_tmp[diff_v_tmp$exp==exp,'Local, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[1,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[1,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = diff_v_tmp[diff_v_tmp$exp==exp,'Local, Credit'] - diff_v_tmp[diff_v_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[2,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[2,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = diff_v_tmp[diff_v_tmp$exp==exp,'Local, Paid'] - diff_v_tmp[diff_v_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[3,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[3,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

}



# #individual level
#
# diff_v_tmp = diff_v_ind %>%
#   group_by(iter,s,population) %>%
#   summarise(value = mean(true-false)) %>% #average across 8 different difficulty x emphasis conditions x 2 time of semester groups (so 16 posteriors being averaged)
#   group_by(iter,population) %>%
#   summarise(value = mean(value),
#             n = length(iter))
#
# n_col_lc = diff_v_tmp[diff_v_tmp$population=='Local, Credit',]$n[1]*4*2
# n_col_lp = diff_v_tmp[diff_v_tmp$population=='Local, Paid',]$n[1]*4*2
# n_col_op = diff_v_tmp[diff_v_tmp$population=='Online, Paid',]$n[1]*4*2
#
# BF_diff_ind = rep(NA,3)
#
# #Local, Credit vs Local, Paid
# prior = apply( matrix(rnorm(n=n_col_lc*10000,mean=1,sd=2),ncol=n_col_lc) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
#   apply( matrix(rnorm(n=n_col_lp*10000,mean=1,sd=2),ncol=n_col_lp) , 1 , 'mean')
# d_prior = approxfun(density(prior),rule=2)
#
# #get posterior density at 0
# posterior = diff_v_tmp[diff_v_tmp$population=='Local, Credit',]$value - diff_v_tmp[diff_v_tmp$population=='Local, Paid',]$value
# d_posterior = approxfun(density(posterior),rule=2)
# BF_diff_ind[1] = d_prior(0) / d_posterior(0)
#
# #Local, Credit vs Online, Paid
# prior = apply( matrix(rnorm(n=n_col_lc*10000,mean=1,sd=2),ncol=n_col_lc) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
#   apply( matrix(rnorm(n=n_col_op*10000,mean=1,sd=2),ncol=n_col_op) , 1 , 'mean')
# d_prior = approxfun(density(prior),rule=2)
#
# #get posterior density at 0
# posterior = diff_v_tmp[diff_v_tmp$population=='Local, Credit',]$value - diff_v_tmp[diff_v_tmp$population=='Online, Paid',]$value
# d_posterior = approxfun(density(posterior),rule=2)
# BF_diff_ind[2] = d_prior(0) / d_posterior(0)
#
#
# #Local, Credit vs Local, Paid
# prior = apply( matrix(rnorm(n=n_col_lp*10000,mean=1,sd=2),ncol=n_col_lp) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
#   apply( matrix(rnorm(n=n_col_op*10000,mean=1,sd=2),ncol=n_col_op) , 1 , 'mean')
# d_prior = approxfun(density(prior),rule=2)
#
# #get posterior density at 0
# posterior = diff_v_tmp[diff_v_tmp$population=='Local, Paid',]$value - diff_v_tmp[diff_v_tmp$population=='Online, Paid',]$value
# d_posterior = approxfun(density(posterior),rule=2)
# BF_diff_ind[3] = d_prior(0) / d_posterior(0)



### Comparison 2: pairwise comparison between early vs late groups within each participant population (averaged across within subjects manipulations) ###

#get prior density at 0 for these comparisons
prior = apply( matrix(rnorm(n=8*100000,mean=1,sd=2),ncol=8) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rnorm(n=8*100000,mean=1,sd=2),ncol=8) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across the experimental manipulations
diff_v_tmp = diff_v_hyp %>%
  group_by(exp,iter,population,time_of_semester) %>%
  summarise(value = mean(value)) %>% #average across 8 different difficulty x emphasis conditions
  spread(time_of_semester,value)

ctr=3
#Local Credit: Early vs Late
for(population in c('Local, Credit','Local, Paid','Online, Paid')){
  ctr=ctr+1
  for(exp in 1:2){

  posterior = diff_v_tmp[diff_v_tmp$exp == exp & diff_v_tmp$population == population,'Late in Semester'] -
    diff_v_tmp[diff_v_tmp$exp == exp & diff_v_tmp$population == 'Local, Credit','Early in Semester']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  }
}

### Comparison 3: pairwise comparison between speed vs accuracy conditions within each participant population (averaged across difficulty conditions) ###

# #HYPERPARAMETERS
#
# #get prior density at 0 for these comparisons
# prior = apply( matrix(rnorm(n=4*100000,mean=1,sd=2),ncol=4) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
#   apply( matrix(rnorm(n=4*100000,mean=1,sd=2),ncol=4) , 1 , 'mean')
# d_prior = approxfun(density(prior),rule=2)
#
# #get posterior of average parameter across the experimental manipulations
# diff_v_x_population_x_tos_x_emphasis = diff_v_hyp %>%
#   group_by(iter,population,time_of_semester,emphasis) %>%
#   summarise(value = mean(true-false)) %>% #average across 8 different difficulty x emphasis conditions
#   spread(emphasis,value)
#
# #EARLY IN SEMESTER
#
# #Local Credit: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Credit' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Credit' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)
#
# #Local Paid: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)
#
# #Online Paid: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Online, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Online, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)
#
# #LATE IN SEMESTER
#
# #Local Credit: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Credit' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Late in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Credit' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Late in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)
#
# #Local Paid: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Local, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Late in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)
#
# #Online Paid: Speed vs Acurracy
# posterior = diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Online, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Early in Semester' ,'Accuracy'] -
#   diff_v_x_population_x_tos_x_emphasis[diff_v_x_population_x_tos_x_emphasis$population == 'Online, Paid' & diff_v_x_population_x_tos_x_emphasis$time_of_semester == 'Late in Semester' ,'Speed']
# d_posterior = approxfun(density(unlist(posterior)),rule=2)
# BF_diff = d_prior(0) / d_posterior(0)



#### INDIVIDUAL LEVEL PARAMETERS ####

#get posterior of average parameter across the experimental manipulations
diff_v_tmp = diff_v_ind %>%
  group_by(exp,iter,s,population,time_of_semester,emphasis) %>%
  summarise(value = mean(value)) %>% #average across 4 different difficulty conditions
  spread(emphasis,value) %>%
  group_by(exp,iter,population,time_of_semester) %>%
  summarise(value = mean(Accuracy - Speed),
            n = length(Accuracy)) #take difference between accuracy and speed and collapse across people

for(time_of_semester in c('Early in Semester','Late in Semester')){
  for(population in c('Local, Credit','Local, Paid','Online, Paid')){
    ctr=ctr+1
    for(exp in 1:2){

    #reduce dataset down to relevant condition
    diff_v_tmp2 = diff_v_tmp[diff_v_tmp$exp == exp & diff_v_tmp$population==population  & diff_v_tmp$time_of_semester==time_of_semester,]

    #get prior density at 0 for this comparisons
    n_col = diff_v_tmp2$n[1]*4
    prior = apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
      apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean')
    d_prior = approxfun(density(prior),rule=2)

    #get posterior density at 0
    d_posterior = approxfun(density(diff_v_tmp2$value),rule=2)

    results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(diff_v_tmp2$value,0.025),2)),", ",sprintf("%.2f",round(quantile(diff_v_tmp2$value,0.975),2)),"]")
    results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

    }
  }
}


### Comparison 4: pairwise comparison between very easy vs easy conditions within each participant population ###

#get posterior of average parameter across the experimental manipulations
diff_v_tmp = diff_v_ind %>%
  group_by(exp,iter,s,population,time_of_semester,difficulty) %>%
  summarise(value = mean(value)) %>% #average across 2 different emphasis conditions
  spread(difficulty,value) %>%
  group_by(exp,iter,population,time_of_semester) %>%
  summarise(e_m_ve = mean(Easy - `Very\nEasy`),
            h_m_e = mean(Hard - Easy),
            vh_m_h = mean(`Very\nHard` - Hard),
            n = length(Hard))

for(time_of_semester in c('Early in Semester','Late in Semester')){
  for(population in c('Local, Credit','Local, Paid','Online, Paid')){
    ctr=ctr+1
    for(exp in 1:2){

    #reduce dataset down to relevant condition
    diff_v_tmp2 = diff_v_tmp[diff_v_tmp$exp == exp & diff_v_tmp$population==population  & diff_v_tmp$time_of_semester==time_of_semester,]

    #get prior density at 0 for this comparisons
    n_col = diff_v_tmp2$n[1]
    prior = apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
      apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean')
    d_prior = approxfun(density(prior),rule=2)

    #get posterior density at 0 for easy vs very easy
    d_posterior = approxfun(density(diff_v_tmp2$e_m_ve),rule=2)
    results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(diff_v_tmp2$e_m_ve,0.025),2)),", ",sprintf("%.2f",round(quantile(diff_v_tmp2$e_m_ve,0.975),2)),"]")
    results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

    #get posterior density at 0 for hard vs easy
    d_posterior = approxfun(density(diff_v_tmp2$h_m_e),rule=2)
    results[6+ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(diff_v_tmp2$h_m_e,0.025),2)),", ",sprintf("%.2f",round(quantile(diff_v_tmp2$h_m_e,0.975),2)),"]")
    results[6+ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

    #get posterior density at 0 for very hard vs hard
    d_posterior = approxfun(density(diff_v_tmp2$vh_m_h),rule=2)
    results[12+ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(diff_v_tmp2$vh_m_h,0.025),2)),", ",sprintf("%.2f",round(quantile(diff_v_tmp2$vh_m_h,0.975),2)),"]")
    results[12+ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

    }
  }
}

##### ANALYSIS OF THRESHOLDS #####

col=c(3,9)

### Comparison 5: pairwise comparison between each participant population (averaged across early vs late and within subject manipulation of emphasis) ###

#get prior density at 0 for these comparisons
prior = apply( matrix(rtnorm(n=4*100000,mean=1,sd=1,lower=0,upper=Inf),ncol=4) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rtnorm(n=4*100000,mean=1,sd=1,lower=0,upper=Inf),ncol=4) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across early vs late group and the experimental manipulations
B_tmp = B_hyp %>%
  group_by(exp,iter,population) %>%
  summarise(value = mean(value)) %>% #average across 2 x emphasis conditions x 2 time of semester groups (so 4 posteriors being averaged)
  spread(population,value)

for(exp in 1:2){
  #local credit vs local paid
  posterior = B_tmp[B_tmp$exp==exp,'Local, Credit'] - B_tmp[B_tmp$exp==exp,'Local, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[1,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[1,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = B_tmp[B_tmp$exp==exp,'Local, Credit'] - B_tmp[B_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[2,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[2,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = B_tmp[B_tmp$exp==exp,'Local, Paid'] - B_tmp[B_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[3,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[3,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

}

### Comparison 6: pairwise comparison between early vs late groups within each participant population (averaged across emphasis manipulation) ###

#get prior density at 0 for these comparisons
prior = apply( matrix(rtnorm(n=2*100000,mean=1,sd=1,lower=0,upper=Inf),ncol=2) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rtnorm(n=2*100000,mean=1,sd=1,lower=0,upper=Inf),ncol=2) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across the experimental manipulations
B_tmp = B_hyp %>%
  group_by(exp,iter,population,time_of_semester) %>%
  summarise(value = mean(value)) %>% #average across 2 x emphasis conditions
  spread(time_of_semester,value)

#calculate bf for each comparison

ctr=3
for(population in c('Local, Credit','Local, Paid','Online, Paid')){
  ctr=ctr+1
  for(exp in 1:2){
  posterior = B_tmp[B_tmp$exp == exp & B_tmp$population == population,'Late in Semester'] -
  B_tmp[B_tmp$exp == exp & B_tmp$population == population,'Early in Semester']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)
  }
}

### Comparison 7: pairwise comparison between speed vs accuracy conditions within each participant population ###

#get posterior of average parameter across the experimental manipulations
B_tmp = B_ind %>%
  group_by(exp,iter,s,population,time_of_semester,emphasis) %>%
  summarise(value = mean(value)) %>% #average across 4 different difficulty conditions
  spread(emphasis,value) %>%
  group_by(exp,iter,population,time_of_semester) %>%
  summarise(value = mean(Accuracy - Speed),
            n = length(Accuracy)) #take difference between accuracy and speed and collapse across people

for(time_of_semester in c('Early in Semester','Late in Semester')){
  for(population in c('Local, Credit','Local, Paid','Online, Paid')){
    ctr=ctr+1
    for(exp in 1:2){

    #reduce dataset down to relevant condition
    B_tmp2 = B_tmp[B_tmp$exp == exp & B_tmp$population==population  & B_tmp$time_of_semester==time_of_semester,]

    #get prior density at 0 for this comparisons
    n_col = B_tmp2$n[1]*4
    prior = apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
      apply( matrix(rnorm(n=n_col*100000,mean=1,sd=2),ncol=n_col) , 1 , 'mean')
    d_prior = approxfun(density(prior),rule=2)

    #get posterior density at 0
    results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(B_tmp2$value,0.025),2)),", ",sprintf("%.2f",round(quantile(B_tmp2$value,0.975),2)),"]")
    results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  }
}
}


##### ANALYSIS OF NON-DECISION TIME #####

col=c(5,11)

### Comparison 8: pairwise comparison between each participant population (averaged across early vs late groups)

#get prior density at 0 for these comparisons
prior = apply( matrix(rtnorm(n=2*100000,mean=0.2,sd=1,lower=0.1,upper=1),ncol=2) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rtnorm(n=2*100000,mean=0.2,sd=1,lower=0.1,upper=1),ncol=2) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across early vs late group and the experimental manipulations
t0_tmp = t0_hyp %>%
  group_by(exp,iter,population) %>%
  summarise(value = mean(t0)) %>% #average across 2 x emphasis conditions x 2 time of semester groups (so 4 posteriors being averaged)
  spread(population,value)

for(exp in 1:2){
  #local credit vs local paid
  posterior = t0_tmp[t0_tmp$exp==exp,'Local, Credit'] - t0_tmp[t0_tmp$exp==exp,'Local, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[1,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[1,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = t0_tmp[t0_tmp$exp==exp,'Local, Credit'] - t0_tmp[t0_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[2,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[2,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  #local credit vs online paid
  posterior = t0_tmp[t0_tmp$exp==exp,'Local, Paid'] - t0_tmp[t0_tmp$exp==exp,'Online, Paid']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[3,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[3,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

}

### Comparison 9: pairwise comparison between early vs late groups within each participant population

#get prior density at 0 for these comparisons
prior = apply( matrix(rtnorm(n=100000,mean=1,sd=1,lower=0,upper=Inf),ncol=1) , 1 , 'mean') -  #simulated prior for 1 group take simulated prior for another group
  apply( matrix(rtnorm(n=100000,mean=1,sd=1,lower=0,upper=Inf),ncol=1) , 1 , 'mean')
d_prior = approxfun(density(prior),rule=2)

#get posterior of average parameter across the experimental manipulations
t0_tmp = t0_hyp %>%
  #group_by(iter,population,time_of_semester) %>%
  #summarise(value = mean(value)) %>% #average across 2 x emphasis conditions
  spread(time_of_semester,t0)

ctr=3
#calculate bf for each comparison
for(population in c('Local, Credit','Local, Paid','Online, Paid')){
  ctr=ctr+1
  for(exp in 1:2){
  posterior = t0_tmp[t0_tmp$exp == exp & t0_tmp$population == population,'Late'] -
    t0_tmp[t0_tmp$exp == exp & t0_tmp$population == population,'Early']
  d_posterior = approxfun(density(unlist(posterior)),rule=2)

  results[ctr,col[exp]] = paste0("[",sprintf("%.2f",round(quantile(unlist(posterior),0.025),2)),", ",sprintf("%.2f",round(quantile(unlist(posterior),0.975),2)),"]")
  results[ctr,col[exp]+1] = sprintf("%.2f",round(d_prior(0) / d_posterior(0) ,2)) #bf (rounds to second digits and keeps trailing 0s)

  }
}

#TODO:

#1) Write function to print CI and BF text in appropriate way. CI text should have space before if positive number and no space if negative. Both functions should round to appropriate value.
#2) Create rest of table


#Create TABLE
