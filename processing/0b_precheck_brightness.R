rm(list=ls())
#load packages
library(tidyverse)

#There are four files in the directory, but the first one (the one that says Study2, not 2a, 2b, or 2c) 
#has data from only the practice trials. 2a, 2b, and 2c contain data from the blocks 1-3, 4-6, and 7-9 
#respectively.
files = list.files(path = "../raw_data/exp2_brightness", pattern = 'special')
files=paste0("../raw_data/exp2_brightness/",files)

data1 = read.table(file=files[grep('6bp',files)],header=T,sep=",") %>% mutate(emphasis="practice")
data2 = read.table(file=files[grep('6ba',files)],header=T,sep=",") %>% mutate(emphasis="accuracy")
data3 = read.table(file=files[grep('6bs',files)],header=T,sep=",") %>% mutate(emphasis="speed")

data = bind_rows(data1,data2,data3) %>% 
  filter(subjectid==1177) %>% #filter out subject who didn't finish
  arrange(subjectid,emphasis,order)

#Check accuracy of practice trial = 0.75
data %>% filter(emphasis=="practice")

#Check number of trial_types per block are equal
as.data.frame(
data %>% 
  filter(emphasis!='practice') %>%
  group_by(resultid,brightness,emphasis) %>% 
  summarise(nObs=length(block),
            accuracy=mean(iscorrect),
            response=mean(response),
            num_nr = sum(iscorrect==-1)) 
)

#Check each individual manipulation
data %>% 
    filter(emphasis!='practice') %>%
    group_by(resultid) %>% 
    summarise(nObs=length(block)) 

data %>% 
  filter(emphasis!='practice') %>%
  group_by(brightness) %>% 
  summarise(nObs=length(block)) 

data %>% 
  filter(emphasis!='practice') %>%
  group_by(emphasis) %>% 
  summarise(nObs=length(block)) 

