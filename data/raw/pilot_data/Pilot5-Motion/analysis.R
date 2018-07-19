rm(list=ls())
#load packages
library(dplyr)
library(ggplot2)

#set directory
setwd("~/Dropbox/Research/Projects/Methods-Sampling/Pilot5-Motion")

files = list.files(path = ".", pattern = '.csv')

data1 = read.table(file=files[1],header=T,sep=",") %>% mutate(emphasis="accuracy")
data2 = read.table(file=files[2],header=T,sep=",") %>% mutate(emphasis="speed")

data = bind_rows(data1,data2) %>% 
  filter(subjectid!=1021) %>% #filter out subject who didn't finish
  arrange(subjectid,emphasis,order)
  
#Check number of obs for each subject
data %>% group_by(subjectid) %>% summarise(nObs=length(block)) 

#Check number of trial_types per block are equal
data %>% group_by(subjectid,angle,coherence,emphasis) %>% summarise(nObs=length(block)) 

acc_data = data %>%
  group_by(coherence,emphasis) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))

ggplot(data=acc_data,aes(x=factor(coherence),y=correct,group=emphasis,color=emphasis)) +
  geom_line() + #facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0.5,1)) 
ggsave(file="Motion-Accuracy-Collapsed.pdf",height=9,width=9)

ggplot(data=acc_data,aes(x=factor(coherence),y=rt,group=emphasis,color=emphasis)) +
  geom_line() + #facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 
ggsave(file="Motion-RT-Collapsed.pdf",height=9,width=9)

acc_data = data %>%
  group_by(coherence,emphasis,subjectid) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))
  
ggplot(data=acc_data,aes(x=factor(coherence),y=correct,group=emphasis,color=emphasis)) +
geom_line() + facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0.5,1)) 
ggsave(file="Motion-Accuracy-BySubject.pdf",height=9,width=9)

ggplot(data=acc_data,aes(x=factor(coherence),y=rt,group=emphasis,color=emphasis)) +
geom_line() + facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 
ggsave(file="Motion-RT-BySubject.pdf",height=9,width=9)










