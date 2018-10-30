rm(list=ls())
#load packages
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------
#Compile data files. These are the ones with "special" in the filename. The one with '6ap' contains the
#practice trials, the one with '6aa' contains the accuracy blocks, and the one with '6as' contains the
#speed blocks

files = list.files(path = "data/raw/exp2_brightness", pattern = 'special')
files=paste0("data/raw/exp2_brightness/",files)

data1 = read.table(file=files[grep('6bp',files)],header=T,sep=",") %>% mutate(emphasis="practice")
data2 = read.table(file=files[grep('6ba',files)],header=T,sep=",") %>% mutate(emphasis="accuracy")
data3 = read.table(file=files[grep('6bs',files)],header=T,sep=",") %>% mutate(emphasis="speed")

#last of wave 1 = 1164, first of wave 2 = 1178
data = bind_rows(data1,data2,data3) %>%
  filter(subjectid!=1165, #filter out test done by Gina
         subjectid!=1167, #test done by Gina
         subjectid!=1169, #test done by Tiffany
         subjectid!=1177) %>%  #filter out test run done on 7 May
  arrange(subjectid,emphasis,order)

#-------------------------------------------------------------------------
#Extract the session info from the other files. The session info is needed to determine
#which wave participants were in.

session = read.table(file="data/raw/exp2_brightness/bright-study6bp-20180704092600.csv",header=T,sep=",") %>%
  mutate(datetime = dmy_hms(Created),
         wave = factor( 1*(datetime > ymd("2018-05-01")),levels=0:1,labels=c('early','late'))) %>%
  select(subjectid,datetime,wave) %>% arrange(subjectid)

#merge session info into data
imported_data=left_join(data,session,by="subjectid")

#save imported data
save(imported_data,file="data/clean/imported_data_exp2.RData")

#-----------------------------------------------
#Check number of trials per participant
as.data.frame(
  imported_data %>%
    group_by(subjectid) %>%
    summarise(n_trials=length(trial),
              complete = n_trials==808,
              source=source[1],
              wave=wave[1],
              datetime=datetime[1])
)

#Check percentage of completes across wave and source conditions
imported_data %>%
    group_by(subjectid) %>%
    summarise(n_trials=length(trial),
              complete = n_trials==808,
              source=source[1],
              wave=wave[1]) %>%
    group_by(wave,source) %>%
    summarise(total = length(complete),
              complete_rate = mean(complete),
              complete_total = sum(complete))

#Cut out participants out practice trials and participants who did not complete all
#800 experimental trials
trimmed_data = imported_data %>%
  filter(emphasis!='practice') %>%
  group_by(subjectid) %>%
  mutate(n_trials=length(trial),
         complete=n_trials==800) %>%
  filter(complete) %>%
  ungroup()

#Double check all remaining participants have 800 trials
as.data.frame(
  trimmed_data %>%
    group_by(subjectid) %>%
    summarise(n_trials=length(trial))
)

#-----------------------------------------------------------------
#Double check trials are evenly distributed across within-participant conditions
as.data.frame(
  trimmed_data %>%
    group_by(brightness,emphasis) %>%
    summarise(n_trials=length(trial))
)

save(trimmed_data,file="data/clean/trimmed_data_exp2.RData")
