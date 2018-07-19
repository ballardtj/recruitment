rm(list=ls())

#load packages
library(tidyverse)

#load data
load("../clean_data/trimmed_data_exp2.RData")

#response rates
trimmed_data %>%
  #our program logs rt and response for non-responses as -1.
  #Here, we replace -1 with NA.
  mutate(rt = if_else(time>0,time,NA_integer_),
         response = if_else(response>0,response,NA_integer_),
         iscorrect = if_else(iscorrect>-1,iscorrect,NA_integer_)) %>%
  group_by(wave,source) %>%
  summarise(nr_rate = mean(is.na(response)),
            too_fast_rate = mean(rt < 150,na.rm=T))

#plot accuracy, mean RTs, and non-response rates by condition

descriptives = trimmed_data %>%
  #our program logs rt and response for non-responses as -1.
  #Here, we replace -1 with NA.
  mutate(rt = if_else(time>0,time,NA_integer_),
         response = if_else(response>0,response,NA_integer_),
         iscorrect = if_else(iscorrect>-1,iscorrect,NA_integer_)) %>%
  filter(rt > 250) %>%
  group_by(subjectid,wave,source,emphasis,brightness) %>%
  summarise(mean_RT = mean(rt,na.rm=T),
            accuracy = mean(iscorrect,na.rm=T),
            response = mean(response),
            nr_rate = mean(is.na(response))) %>%
group_by(wave,source,emphasis,brightness) %>%
  summarise(mean_RT = mean(mean_RT),
            accuracy = mean(accuracy),
            response = mean(response),
            nr_rate = mean(nr_rate))

ggplot(data=descriptives) +
  geom_line(aes(x=brightness,y=accuracy,group=emphasis,colour=emphasis)) +
  facet_grid(wave~source) +
  theme_minimal()

ggplot(data=descriptives) +
  geom_line(aes(x=brightness,y=mean_RT,group=emphasis,colour=emphasis)) +
  facet_grid(wave~source) +
  theme_minimal()

ggplot(data=descriptives) +
  geom_line(aes(x=brightness,y=nr_rate,group=emphasis,colour=emphasis)) +
  facet_grid(wave~source) +
  theme_minimal()
