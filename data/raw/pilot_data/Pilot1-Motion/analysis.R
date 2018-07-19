rm(list=ls())
#load packages
library(dplyr)
library(ggplot2)

#set directory
setwd("~/Dropbox/Research/Projects/Methods-Sampling/E1-Motion")

files = list.files(path = ".", pattern = '.csv')

#data = read.table(file='timstest1.csv',header=T,sep=",")

data1 = read.table(file=files[1],header=T,sep=",") # %>% filter(age!="TEST",gender!="TEST") %>% mutate(age = as.numeric(as.character(age)))
 
 
data2 = read.table(file=files[3],header=T,sep=",") # %>% filter(age=="TEST",gender=="TEST") %>% mutate(age = as.numeric(as.character(age)))

data = bind_rows(data1,data2) %>% 
  mutate(emphasis = factor( 1*(experiment=="study3aa"),levels=0:1,labels=c('Speed',"Accuracy"))) %>%
   arrange(subjectid,emphasis,order)
  


#Check number of trial_types per block are equal
data %>% group_by(angle,coherence,emphasis) %>% summarise(nObs=length(block)) 

acc_data = data %>%
  group_by(coherence,emphasis,subjectid) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))
  
ggplot(data=acc_data,aes(x=factor(coherence),y=correct,group=emphasis,color=emphasis)) +
geom_line() +#+ facet_wrap(~subjectid)
  xlab("coherence") + coord_cartesian(ylim=c(0.5,1)) 

ggplot(data=acc_data,aes(x=factor(coherence),y=rt,group=emphasis,color=emphasis)) +
geom_line() +#+ facet_wrap(~subjectid)
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 










