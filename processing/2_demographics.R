rm(list=ls())

#load packages
library(tidyverse)


#sample details

#load data from motion experiment
load("data/clean/imported_data_exp1.RData")

#
subject_data_1 = imported_data %>%
    group_by(subjectid) %>%
    summarise(n_trials=length(trial),
              complete = n_trials==808,
              source=source[1],
              wave=wave[1],
              age=age[1],
              gender=gender[1],
              expt = "Motion")

#load data from brightness experiment
load("data/clean/imported_data_exp2.RData")

subject_data_2 = imported_data %>%
  group_by(subjectid) %>%
  summarise(n_trials=length(trial),
            complete = n_trials==808,
            source=source[1],
            wave=wave[1],
            age =as.numeric(as.character(age[1])),
            gender=gender[1],
            expt = "Brightness")

subject_data = bind_rows(subject_data_1,subject_data_2)


#gender breakdown
subject_data %>%
  count(expt,wave,source,gender) %>% data.frame()

subject_data %>%
  count(expt,wave,source,age)%>% data.frame()

#N and complete rate breakdown

sample_details_tmp = subject_data %>%
  mutate(expt = factor(expt,levels=c('Motion','Brightness')),
         source = factor(source,levels=c('firstyear','community','mturk'),labels=c('local credit','local paid','online paid'))) %>%
  group_by(expt,wave,source) %>%
  summarise(n_total = length(complete),
            n_complete = sum(complete),
            perc_complete = mean(complete)*100,
            mean_age = mean(age),
            sd_age = sd(age),
            perc_female = mean(gender=="female")*100,
            perc_male = mean(gender=="male")*100,
            perc_other = mean(gender=="other")*100) %>%
  ungroup()



#construct latex table
library(xtable)

sample_details = as.matrix(sample_details_tmp[,4:ncol(sample_details_tmp)])
#colnames(sample_details) = c('')
rownames(sample_details) = as.character(sample_details_tmp$source)

latex_table=xtable(sample_details,
                   align=c("X",rep("c",ncol(sample_details))),
                   caption="Sample details for motion and brightness discrimination experiments",
                   label = "tab:sample_details",
                   digits = 2)

#addtorow <- list()
#addtorow$pos <- list(7)
#addtorow$command <- c(
#  "\\hline \\multicolumn{3}{p\\textwidth}{
#  \\small{Note: The BFs for the time of semester and sample effects were calculated by comparing a model with only that main effect to an intercept-only model. The BFs for the interaction were calculated by comparing a model with both main effects and the interaction to a model with both main effects but without the interaction.}} \\\\ ")

print(latex_table,
     # add.to.row=addtorow,
      tabular.environment = "tabularx",
      width = "\\textwidth",
      hline.after=c(-1,0,12),
      caption.placement = "top",
      math.style.exponents = TRUE)


#Note, the latex table is further edited in the latex document












