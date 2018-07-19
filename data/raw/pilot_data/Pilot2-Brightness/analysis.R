rm(list=ls())
#load packages
library(dplyr)
library(ggplot2)

#set directory
setwd("~/Dropbox/Research/Projects/Methods-Sampling/Pilot2-Brightness")

files = list.files(path = ".", pattern = '.csv')

data = read.table(file=files[1],header=T,sep=",") %>% 
  arrange(subjectid,order)
  


#Check number of trial_types per block are equal
data %>% group_by(brightness) %>% summarise(nObs=length(block)) 

acc_data = data %>%
  filter(subjectid!=1002,subjectid!=1003,subjectid!=1036) %>%
  group_by(brightness) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))
  
ggplot(data=acc_data,aes(x=factor(brightness),y=correct,group=1)) +
geom_line() +# facet_wrap(~subjectid) +
  xlab("brightness") + coord_cartesian(ylim=c(0.5,1)) 

ggplot(data=acc_data,aes(x=factor(brightness),y=rt,group=1)) +
geom_line() + #facet_wrap(~subjectid)
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 


#Swap random dot task so that
  #a) stimuli move left-right
  #b) persistence < 15
  #c) coherence = 
seq(0.38,0.62,by=.02)

seq(0.05,.6,by=0.05)

#N=25 for each exp




