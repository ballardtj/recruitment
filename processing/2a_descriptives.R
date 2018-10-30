rm(list=ls())

#load packages
library(tidyverse)
library(xtable)
library(grid)
library(gridExtra)

###########################
## load and combine data ##
###########################

load("data/clean/trimmed_data_exp1.RData")
trimmed_data_e1 = trimmed_data
trimmed_data_e1$expt = 'Motion'

load("data/clean/trimmed_data_exp2.RData")
trimmed_data_e2 = trimmed_data
trimmed_data_e2$expt = 'Brightness'
trimmed_data_e2$age = as.numeric(as.character(trimmed_data_e2$age))

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
#colnames(sample_details) = c('')
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


###################################################
## calculate non-respnse and fast response rates ##
###################################################

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

############################
### Statistical Analysis ###
############################





