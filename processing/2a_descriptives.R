rm(list=ls())

#load packages
library(tidyverse)
library(xtable)
library(grid)
library(gridExtra)
library(BayesFactor)
library(brms)

###########################
## load and combine data ##
###########################

load("data/clean/trimmed_data_exp1.RData")
trimmed_data_e1 = trimmed_data
trimmed_data_e1$expt = 'Motion'

load("data/clean/trimmed_data_exp2.RData")
trimmed_data_e2 = trimmed_data
trimmed_data_e2$expt = 'Brightness'
trimmed_data_e2$age = as.numeric(as.character(trimmed_data_e2$age)) #one person didn't report their age in the brightness experiment, so age gets read as a factor. Here, we convert to numeric

combined_data = bind_rows(trimmed_data_e1,trimmed_data_e2)

###################################################
## calculate non-respnse and fast response rates ##
###################################################

#response rates
response_rates = combined_data %>%
  #our program logs rt and response for non-responses as -1.
  #Here, we replace -1 with NA.
  mutate(expt = factor(expt,levels=c('Motion','Brightness')),
         source = factor(source,levels=c('firstyear','community','mturk'),labels=c('local credit','local paid','online paid')),
         rt = if_else(time>0,time,NA_integer_),
         response = if_else(response>0,response,NA_integer_),
         iscorrect = if_else(iscorrect>-1,iscorrect,NA_integer_)) %>%
  group_by(expt,wave,source) %>%
  summarise(n_total = length(response),
            nr_rate = round(mean(is.na(response))*100,digits=1),
            too_fast_rate = round(mean(rt < 150,na.rm=T)*100,digits=1),
            n_valid = sum( (!is.na(response))  & (time>=150) ) )

############################################
## construct table showing response rates ##
############################################

response_rate_matrix = as.matrix(response_rates[,4:ncol(response_rates)])
rownames(response_rate_matrix) = as.character(response_rates$source)

latex_table=xtable(response_rate_matrix,
                   align=c("X",rep("c",ncol(response_rate_matrix))),
                   #caption="Sample details for motion and brightness discrimination experiments",
                   label = "tab:response_rates",
                   digits = 2)

print(latex_table,
      # add.to.row=addtorow,
      tabular.environment = "tabularx",
      width = "\\textwidth",
      hline.after=c(-1,0,12),
      caption.placement = "top",
      math.style.exponents = TRUE)


#Note, the latex table is further edited in the latex document


##################################################
## Create figure displaying behavioural results ##
##################################################

### Motion experiment ###

subject_data = combined_data %>%
  #our program logs rt and response for non-responses as -1.
  filter(time >= 150,response>0,expt=='Motion') %>%
  mutate(source = factor(source,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         wave = factor(wave,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         difficulty = factor(coherence,levels=c(0.25,0.2,0.15,0.1),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(emphasis,levels=c('speed','accuracy'),labels=c('Speed','Accuracy'))) %>%
  group_by(subjectid,wave,source,emphasis,difficulty) %>%
  summarise(mean_RT = mean(time,na.rm=T),
            accuracy = mean(iscorrect,na.rm=T))

motion_accuracy = subject_data %>%
  group_by(wave,source,emphasis,difficulty) %>%
  summarise(accuracy = mean(accuracy)) %>%
  ggplot(aes(x=difficulty,y=accuracy,group=emphasis,colour=emphasis)) +
  geom_jitter(data=subject_data,alpha=0.15) +
  geom_line(size=2) +
  geom_point(size=4) +
  geom_point(size=2.5,colour="white") +
  facet_grid(wave~source) +
  labs(y='Proportion Correct',x='Difficulty',colour = 'Emphasis') +
  scale_y_continuous(breaks = seq(0.25,1,0.25),limits = c(0.25,1)) +
  theme(legend.position = 'none')

motion_rt = subject_data %>%
  group_by(wave,source,emphasis,difficulty) %>%
  summarise(mean_RT = mean(mean_RT)) %>%
  ggplot(aes(x=difficulty,y=mean_RT,group=emphasis,colour=emphasis)) +
  geom_jitter(data=subject_data,alpha=0.15) +
  geom_line(size=2) +
  geom_point(size=4) +
  geom_point(size=2.5,colour="white") +
  facet_grid(wave~source) +
  labs(y='Mean Response Time (ms)',x='Difficulty',colour = 'Emphasis') +
  theme(legend.position = 'none')+
  scale_y_continuous(breaks = seq(500,2000,500),limits = c(150,2000))

### Brightness experiment ###

subject_data = combined_data %>%
  #our program logs rt and response for non-responses as -1.
  filter(time >= 150,response>0,expt=='Brightness') %>%
  mutate(source = factor(source,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         wave = factor(wave,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         difficulty = factor(abs(brightness-0.5),levels=c(0.05,0.04,0.03,0.02),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(emphasis,levels=c('speed','accuracy'),labels=c('Speed','Accuracy'))) %>%
  group_by(subjectid,wave,source,emphasis,difficulty) %>%
  summarise(mean_RT = mean(time,na.rm=T),
            accuracy = mean(iscorrect,na.rm=T))

brightness_accuracy = subject_data %>%
  group_by(wave,source,emphasis,difficulty) %>%
  summarise(accuracy = mean(accuracy)) %>%
  ggplot(aes(x=difficulty,y=accuracy,group=emphasis,colour=emphasis)) +
  geom_jitter(data=subject_data,alpha=0.15) +
  geom_line(size=2) +
  geom_point(size=4) +
  geom_point(size=2.5,colour="white") +
  facet_grid(wave~source) +
  labs(y='Proportion Correct',x='Difficulty',colour = 'Emphasis') +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0.25,1,0.25),limits = c(0.25,1))

brightness_rt = subject_data %>%
  group_by(wave,source,emphasis,difficulty) %>%
  summarise(mean_RT = mean(mean_RT)) %>%
  ggplot(aes(x=difficulty,y=mean_RT,group=emphasis,colour=emphasis)) +
  geom_jitter(data=subject_data,alpha=0.15) +
  geom_line(size=2) +
  geom_point(size=4) +
  geom_point(size=2.5,colour="white") +
  facet_grid(wave~source) +
  labs(y='Mean Response Time (ms)',x='Difficulty',colour = 'Emphasis') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = seq(500,2000,500),limits = c(150,2000))

### Combined Figure ###

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend<-g_legend(brightness_rt)

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
    nrow=1,ncol=2),
  legend, nrow=2,heights=c(20, 1))

ggsave(file="figures/behavioural_results.pdf",plot=behavioural_figure,height=20,width=26,units="cm")

####################################################
### Bayes Factor Analysis of Behavioural Results ###
####################################################

### Motion experiment ###

motion_data = filter(combined_data,time >= 150,response>0,expt=='Motion') %>%
  mutate(local_paid = as.numeric(source=="community"),
         online_paid = as.numeric(source=="mturk"),
         time_of_semester = -1*(wave=="early") + 1*(wave=="late"),
         difficulty = -1.5*(coherence==0.25) + -0.5*(coherence==0.20) + 0.5*(coherence==0.15) + 1.5*(coherence==0.10),
         emphasis = -1*(emphasis=="speed") + 1*(emphasis=="accuracy"))

#set prior
prior = c(
  set_prior("student_t(3,0,10)", class = "Intercept"),
  set_prior("student_t(3,0,10)", class = "b"),
  set_prior("student_t(3,0,10)", class = "sd")
)

#run accuracy model
fit_accuracy = brm(iscorrect ~ local_paid + online_paid + time_of_semester + difficulty + emphasis + (1|subjectid),
                   prior = prior,
                   data=motion_data,
                   family=bernoulli(),
                   cores=4)



save(fit_accuracy,file="../statistical_output/brms_accuracy_results.RData")


#get Bayes factors via savage dickey method
samples <- posterior_samples(fit_accuracy)
bfs = rep(NA,5)

d_post = approxfun(density(samples$b_Intercept),rule=2) #approximate posterior density function based on sampled values
bfs[1] = dstudent_t(0,3,0,10)/d_post(0)                 #analytical density of prior at 0 divided by approximate density of posterior at 0

d_post = approxfun(density(samples$b_gainornot),rule=2)
bfs[2] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_lossornot),rule=2)
bfs[3] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_diffnumeric),rule=2)
bfs[4] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$sd_s__Intercept),rule=2)
bfs[5] = 2*dstudent_t(0,3,0,10)/d_post(0)  #numerator is multiplied by 2 because for sigma the distribution is folded at 0.





bf_1 = anovaBF(accuracy ~ source*wave*difficulty*emphasis,whichRandom="subject",data=motion_data)
bf_2 = anovaBF(mean_RT ~ source+wave+difficulty+emphasis,whichRandom="subject",data=motion_data,whichModels="top")





### Brightness experiment ###

brightness_data = filter(combined_data,time >= 150,response>0,expt=='Brightness') %>%
  mutate(source = factor(source,levels=c('firstyear','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')),
         wave = factor(wave,levels=c('early','late'),labels=c('Early in Semester','Late in Semester')),
         difficulty = factor(abs(brightness-0.5),levels=c(0.05,0.04,0.03,0.02),labels=c('Very\nEasy','Easy','Hard','Very\nHard')),
         emphasis = factor(emphasis,levels=c('speed','accuracy'),labels=c('Speed','Accuracy'))) %>%
  group_by(subjectid,wave,source,emphasis,difficulty) %>%
  summarise(mean_RT = mean(time,na.rm=T),
            accuracy = mean(iscorrect,na.rm=T))

bf_3 = anovaBF(accuracy ~ source+wave+difficulty+emphasis,whichRandom="subject",data=brightness_data,whichModels="top")
bf_4 = anovaBF(mean_RT ~ source+wave+difficulty+emphasis,whichRandom="subject",data=brightness_data,whichModels="top")

#############################################
### Construct table with analysis results ###
#############################################

models = c('Time of Semester',
          'Population',
          'Emphasis',
          'Difficulty',
          'Population $\\times$ Time',
          'Emphasis $\\times$ Time',
          'Emphasis $\\times$ Population',
          'Difficulty $\\times$ Time',
          'Difficulty $\\times$ Population',
          'Difficulty $\\times$ Emphasis',
          'Emphasis $\\times$ Population $\\times$ Time',
          'Difficulty $\\times$ Population $\\times$ Time',
          'Difficulty $\\times$ Emphasis $\\times$ Time',
          'Difficulty $\\times$ Emphasis $\\times$ Population',
          'Difficulty $\\times$ Emphasis $\\times$ Population $\\times$ Time')

#Store BFs in matrix. Reverse order so that main effects are presented first and four way interaction
#presented last. Also invert BFs, so that higher values indicate support for the effect (i.e., the numerator
#is the full model and the denominator is the model with that effect removed)

results_tmp = cbind( 1/rev(extractBF(bf_1)$bf),
                 1/rev(extractBF(bf_3)$bf),
                 1/rev(extractBF(bf_2)$bf),
                 1/rev(extractBF(bf_4)$bf) )

#function that sets the number of digits based on the scale of the value
format_digits = function(x){
  formatted_results = matrix(NA,nrow=nrow(x),ncol=ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:(ncol(x))){
      value = x[i,j]
      if(abs(value)>10000){
        tmp = sprintf(fmt="%.2e",value)
        tmp = str_remove( str_remove(tmp,pattern="\\+0") ,"\\+" ) #remove the plus sign and any 0's immediately after the plus
        formatted_results[i,j] = paste0('$',str_replace(tmp,pattern="e",'\\\\times 10^{'),'}$')
      } else if(value > 0.1){
        formatted_results[i,j] = paste0('$',sprintf(fmt="%.2f",value),'$')
      } else if(value > 0.01) {
        formatted_results[i,j] = paste0('$',sprintf(fmt="%.2f",value),'$')
      } else {
        formatted_results[i,j] = paste0('$',sprintf(fmt="%.1g",value),'$')
      }
    }
  }
  return(formatted_results)
}

results = format_digits(results_tmp)
rownames(results) <- models
colnames(results) <- rep(c('Study 2','Study 3'),2)

latex_table=xtable(results,
                   align=rep("l",ncol(results)+1),
                   caption="Bayes Factor Results for the Effects of Population, Time of Semester, Speed/Accuracy Emphasis, and Difficulty on Accuracy and Mean Response Times",
                   label = "tab:behavioural_results"
                   #digits = set_digits(results),
                   #display = c("s","g","g","g","g"),
                   )

addtorow <- list()
addtorow$pos <- list(dim(results)[1])
addtorow$command <- c(
  "\\hline \\multicolumn{5}{p\\textwidth}{
  \\small{Note: The BFs were obtained using a top-down model comparison approach. The numerator in each comparison was the full model and the denominator was a model with the relevant effect removed. Thus, BFs > 1 indicate evidence in support of the effect, whereas BFs < 1 indicate evidence against it.}} \\\\ ")

print(latex_table,
      add.to.row=addtorow,
      tabular.environment = "tabularx",
      width = "\\textwidth",
      hline.after=c(-1,0),
      caption.placement = "top",
      math.style.exponents = TRUE,
      sanitize.text.function=identity,
      size='small')


set.seed(10)
X <- as.data.frame(matrix(sample(1:1000, 8), nrow = 2), stringsAsFactors = FALSE)
X[3, ] <- (X[1,]/(X[1,]+X[2,])) * 100
X
#         V1        V2        V3        V4
#1 508.00000 427.00000  85.00000 273.00000
#2 307.00000 692.00000 225.00000 271.00000
#3  62.33129  38.15907  27.41935  50.18382
X <- as.data.frame(lapply(X, sprintf, fmt = c("%.0e", "%.0f", "%.6f")))



ggplot(data=motion_data) +
  geom_histogram(aes(x=time)) +
  facet_grid(source + difficulty ~ wave + emphasis)

ggplot(data=motion_data) +
  geom_histogram(aes(x=accuracy)) +
  facet_grid(source + difficulty ~ wave + emphasis)







