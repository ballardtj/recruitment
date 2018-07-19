rm(list=ls())
#load packages
library(dplyr)
library(ggplot2)

#set directory
setwd("~/Dropbox/Research/Projects/Methods-Sampling/Pilot2-Motion")

files = list.files(path = ".", pattern = '.csv')

data = read.table(file=files[1],header=T,sep=",") %>% 
  arrange(subjectid,order)
  
  
#For Amy
data.amy = data %>%
  filter(subjectid!=1002) %>%
  mutate(direction = angle) %>%
  group_by(subjectid) %>%
  mutate(block= as.numeric(as.factor(resultid)) ) %>%
  select(resultid,subjectid,age,gender,block,order,coherence,direction,time,response,iscorrect) %>%
  arrange(subjectid,block,order)

data.amy$direction[data.amy$direction==270]="left"
data.amy$direction[data.amy$direction==90]="right"
data.amy$response[data.amy$response==1]="LEFT"
data.amy$response[data.amy$response==2]="RIGHT"

save(data.amy,file="amy.data.RData")
  





#Check number of trial_types per block are equal
data %>% group_by(angle,coherence) %>% summarise(nObs=length(block)) 

acc_data = data %>%
  filter(subjectid!=1023,subjectid!=1030) %>%
  group_by(coherence) %>%
  summarise(correct = mean(iscorrect==1),
            rt = mean(time))
  
ggplot(data=acc_data,aes(x=factor(coherence),y=correct,group=1)) +
geom_line() + #facet_wrap(~subjectid) +
  xlab("coherence") + coord_cartesian(ylim=c(0.5,1)) 

ggplot(data=acc_data,aes(x=factor(coherence),y=rt,group=1)) +
geom_line() +#+ facet_wrap(~subjectid)
  xlab("coherence") + coord_cartesian(ylim=c(0,2000)) 










