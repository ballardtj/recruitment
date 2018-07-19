rm(list=ls())
#load packages
library(dplyr)
library(ggplot2)

#set directory
setwd("~/Dropbox/Research/Projects/Methods-Sampling/Pilot3-Brightness")

files = list.files(path = ".", pattern = '.csv')

data1 = read.table(file=files[1],header=T,sep=",") %>% mutate(emphasis="accuracy")
data2 = read.table(file=files[2],header=T,sep=",") %>% mutate(emphasis="speed")

data = bind_rows(data1,data2) %>% 
  filter(subjectid>1007) %>% #filter out Gina's initial tests
  arrange(subjectid,emphasis,order)

#Check number of obs for each subject
data %>% group_by(subjectid) %>% summarise(nObs=length(block)) 

#Check number of trial_types per block are equal
data %>% group_by(brightness,emphasis) %>% summarise(nObs=length(block)) 

acc_data = data %>%
  group_by(brightness,emphasis) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))

ggplot(data=acc_data,aes(x=factor(brightness),y=correct,group=emphasis,color=emphasis)) +
  geom_line() + #facet_wrap(~subjectid) +
  xlab("brightness") + coord_cartesian(ylim=c(0.5,1)) 
ggsave(file="Brightness-Accuracy-Collapsed.pdf",height=9,width=9)

ggplot(data=acc_data,aes(x=factor(brightness),y=rt,group=emphasis,color=emphasis)) +
  geom_line() + #facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 
ggsave(file="Brightness-RT-Collapsed.pdf",height=9,width=9)

acc_data = data %>%
  group_by(brightness,emphasis,subjectid) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))
  
ggplot(data=acc_data,aes(x=factor(brightness),y=correct,group=emphasis,color=emphasis)) +
geom_line() + facet_wrap(~subjectid) +
  xlab("brightness") + coord_cartesian(ylim=c(0.5,1))
ggsave(file="Brightness-Accuracy-BySubject.pdf",height=9,width=9)

ggplot(data=acc_data,aes(x=factor(brightness),y=rt,group=emphasis,color=emphasis)) +
geom_line() + facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 
ggsave(file="Brightness-RT-BySubject.pdf",height=9,width=9)


#Swap random dot task so that
  #a) stimuli move left-right
  #b) persistence < 15
  #c) coherence = 
seq(0.38,0.62,by=.02)

seq(0.05,.6,by=0.05)

#N=25 for each exp




