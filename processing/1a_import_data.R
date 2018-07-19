rm(list=ls())
#load packages
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------
#Compile data files. These are the ones with "special" in the filename. The one with '6ap' contains the
#practice trials, the one with '6aa' contains the accuracy blocks, and the one with '6as' contains the 
#speed blocks

files = list.files(path = "../raw_data/exp1_motion", pattern = 'special')
files=paste0("../raw_data/exp1_motion/",files)

data1 = read.table(file=files[grep('6ap',files)],header=T,sep=",") %>% mutate(emphasis="practice")
data2 = read.table(file=files[grep('6aa',files)],header=T,sep=",") %>% mutate(emphasis="accuracy")
data3 = read.table(file=files[grep('6as',files)],header=T,sep=",") %>% mutate(emphasis="speed")

data = bind_rows(data1,data2,data3) %>% 
  filter(subjectid!=1193, #filter out test run done on 7 May
         subjectid!=1181, #test done by Gina
         subjectid!=1183, #other tests done between waves (Paul?)
         subjectid!=1185) %>% #filter out test run done on 7 May
  arrange(subjectid,emphasis,order)

#-------------------------------------------------------------------------
#Extract the session info from the other files. The session info is needed to determine
#which wave participants were in.

session = read.table(file="../raw_data/exp1_motion/rdm-study6ap-20180704091936.csv",header=T,sep=",") %>% 
  mutate(datetime = dmy_hms(Created),
         wave = factor( 1*(datetime > ymd("2018-05-01")),levels=0:1,labels=c('early','late'))) %>% 
  select(subjectid,datetime,wave) %>% arrange(subjectid) 

#merge session info into data
imported_data=left_join(data,session,by="subjectid")

#save imported data
save(imported_data,file="../clean_data/imported_data_exp1.RData")

#-----------------------------------------------
#Check number of trials per participant
as.data.frame(
  imported_data %>% 
    group_by(subjectid) %>% 
    summarise(n_trials=length(trial),
              complete = n_trials==808,
              source=source[1],
              wave=wave[1])
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
    group_by(angle,coherence,emphasis) %>% 
    summarise(n_trials=length(trial))
)

#The output of the above reveals what appears to be a technical error. There is 
#one observation for which angle is NA, and one observation for which coherence is NA.

#    angle coherence emphasis n_trials
# 1     90      0.10 accuracy    13050
# 2     90      0.10    speed    13050
# 3     90      0.15 accuracy    13050
# 4     90      0.15    speed    13050
# 5     90      0.20 accuracy    13049
# 6     90      0.20    speed    13050
# 7     90      0.25 accuracy    13050
# 8     90      0.25    speed    13050
# 9     90        NA accuracy        1
# 10   270      0.10 accuracy    13050
# 11   270      0.10    speed    13050
# 12   270      0.15 accuracy    13050
# 13   270      0.15    speed    13050
# 14   270      0.20 accuracy    13049
# 15   270      0.20    speed    13050
# 16   270      0.25 accuracy    13050
# 17   270      0.25    speed    13050
# 18    NA      0.20 accuracy        1

#From the above, it's obvious that the NA value for angle should be 270
#and that the NA value for coherence should be 0.20. So we edit those values directly below
trimmed_data$angle[is.na(trimmed_data$angle)] = 270
trimmed_data$coherence[is.na(trimmed_data$coherence)] = 0.20

#Triple check trials are evenly distributed across within-participant conditions
as.data.frame(
  trimmed_data %>% 
    group_by(angle,coherence,emphasis) %>% 
    summarise(n_trials=length(trial))
)

save(trimmed_data,file="../clean_data/trimmed_data_exp1.RData")
