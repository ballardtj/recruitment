

library(tidyverse)

survey_file = list.files(path="data/raw",pattern=".csv",full.names=T)
survey_data = read_csv(survey_file[1])

#cut out first two lines, which qualtrics includes by default
survey_data = survey_data[3:nrow(survey_data),]

#cut out survey previews (which were done by experimenter)
survey_data = filter(survey_data,Status!="Survey Preview")

#number of participants who opened the survey
nrow(survey_data)
#80

#number of participants actually started the survey
nrow(filter(survey_data,as.numeric(Progress)>17))
#70

#number of participants who finished the survey
nrow(filter(survey_data,Finished=="True"))
#51

performance_levels = c('Extremely Poorly','Poorly','Somewhat Poorly','Moderately','Somewhat Well','Well','Extremely Well')
caution_levels = c('Extremely Low Caution','Low Caution','Somewhat Low Caution','Moderate Caution','Somewhat High Caution','High Caution','Extremely High Caution')


data = filter(survey_data,Finished=="True") %>%
  mutate(subject = 1:n(),
         age = as.numeric(Q13_1),
         gender = Q14,
         career_stage = Q15,
         years_since_phd = as.numeric(Q16_1),
         research_focus = Q17,
         performance__early__undergrads = as.numeric(factor(Q19_1,levels=performance_levels)),
         performance__early__community = as.numeric(factor(Q19_2,levels=performance_levels)),
         performance__early__mturk = as.numeric(factor(Q19_3,levels=performance_levels)),
         performance__late__undergrads = as.numeric(factor(Q21_1,levels=performance_levels)),
         performance__late__community = as.numeric(factor(Q21_2,levels=performance_levels)),
         performance__late__mturk = as.numeric(factor(Q21_3,levels=performance_levels)),
         caution__early__undergrads = as.numeric(factor(Q32_1,levels=caution_levels)),
         caution__early__community = as.numeric(factor(Q32_2,levels=caution_levels)),
         caution__early__mturk = as.numeric(factor(Q32_3,levels=caution_levels)),
         caution__late__undergrads = as.numeric(factor(Q33_1,levels=caution_levels)),
         caution__late__community = as.numeric(factor(Q33_2,levels=caution_levels)),
         caution__late__mturk = as.numeric(factor(Q33_3,levels=caution_levels)),
         open_ended = Q27) %>%
  select(subject:open_ended) %>%
  gather(key=key,value=value,performance__early__undergrads:caution__late__mturk) %>%
  separate(col=key,into=c('outcome','time_of_semester','population')) %>%
  mutate(outcome = factor(outcome,levels=c('performance','caution'),labels=c('Performance','Caution')),
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('Early','Late')),
         population = factor(population,levels=c('undergrads','community','mturk'),labels=c('Local, Credit','Local, Paid','Online, Paid')))

#Subject breakdown
subject_data = data %>%
  group_by(subject) %>%
  summarise(gender = gender[1],
            age = age[1],
            career_stage = career_stage[1],
            years_since_phd = years_since_phd[1],
            research_focus = research_focus[1])
#gender
count(subject_data,gender)

#age
count(subject_data,age) %>% data.frame()

#career stage
count(subject_data,career_stage)

#number of participants with phds
subject_data %>%
  summarise(phds = sum(!is.na(years_since_phd)))

#time since phd
subject_data %>%
  summarise(mean_ysp = mean(years_since_phd,na.rm=T),
            sd_ysp = sd(years_since_phd,na.rm=T))


subject_data %>%
  summarise(mean_age = mean(age,na.rm=T),
            sd_age = sd(age,na.rm=T))



plot = data %>%
    group_by(outcome,time_of_semester,population) %>%
    summarise(value=mean(value,na.rm=T)) %>%
ggplot(aes(y=value,x=time_of_semester,colour=population)) +
    geom_jitter(data=data,alpha=0.2) +
    geom_line(size=2,aes(group=1)) +
    geom_point(size=6) +
    geom_point(size=3.5,colour="white") +
    facet_grid(outcome~population) +
    labs(y='Expected Level',x='Time of Semester') +
    theme(legend.position = "none")


ggsave(file="figures/survey_results.pdf",plot=plot,height=11,width=14,units="cm")


##################################################
### Mixed Modelling Analysis of Survey Results ###
##################################################

#Run statistical analysis and store results
library(brms)
library(xtable)
set.seed(12345)

#set prior
brms_prior = c(
  set_prior("student_t(3,0,10)", class = "Intercept"),
  set_prior("student_t(3,0,10)", class = "b"),
  set_prior("student_t(3,0,10)", class = "sd")
)

#run performance model
performance_data = data %>%
  filter(outcome=="Performance") %>%
  mutate(local_paid = as.numeric(population=="Paid Local Sample"),
         online_paid = as.numeric(population=="Mechanical Turk Sample"),
         time_of_semester = -1*(time_of_semester =="First\n3 Weeks") + 1*(time_of_semester =="Final\n3 Weeks"))

fit_performance = brm(value ~ local_paid + online_paid + time_of_semester + (1|subject),
          prior = brms_prior,
          data=performance_data,
          cores=4,
          seed = 12345,
          control=list(adapt_delta=0.99,max_treedepth=20))

#run caution model
caution_data = data %>%
  filter(outcome=="Caution") %>%
  mutate(local_paid = as.numeric(population=="Paid Local Sample"),
         online_paid = as.numeric(population=="Mechanical Turk Sample"),
         time_of_semester = -1*(time_of_semester =="First\n3 Weeks") + 1*(time_of_semester =="Final\n3 Weeks"))

fit_caution = brm(value ~ local_paid + online_paid + time_of_semester + (1|subject),
                  prior = brms_prior,
                  data=caution_data,
                  cores=4,
                  seed=12345,
                  control=list(adapt_delta=0.99,max_treedepth=20))

#calculate bayes factors using savage dickey method
samples <- posterior_samples(fit_performance)
bfs_perf = rep(NA,5)

d_post = approxfun(density(samples$b_Intercept),rule=2) #approximate posterior density function based on sampled values
bfs_perf[1] = dstudent_t(0,3,0,10)/d_post(0)                 #analytical density of prior at 0 divided by approximate density of posterior at 0

d_post = approxfun(density(samples$b_local_paid),rule=2)
bfs_perf[2] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_online_paid),rule=2)
bfs_perf[3] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_time_of_semester),rule=2)
bfs_perf[4] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$sd_subject__Intercept),rule=2)
bfs_perf[5] = 2*dstudent_t(0,3,0,10)/d_post(0)  #numerator is multiplied by 2 because for sigma the distribution is folded at 0.

d_post = approxfun(density(samples$sigma),rule=2)
bfs_perf[6] = 2*dstudent_t(0,3,0,500)/d_post(0)  #numerator is multiplied by 2 because for sigma the distribution is folded at 0.


#same as above for caution results
samples <- posterior_samples(fit_caution)
bfs_caut = rep(NA,5)

d_post = approxfun(density(samples$b_Intercept),rule=2) #approximate posterior density function based on sampled values
bfs_caut[1] = dstudent_t(0,3,0,10)/d_post(0)                 #analytical density of prior at 0 divided by approximate density of posterior at 0

d_post = approxfun(density(samples$b_local_paid),rule=2)
bfs_caut[2] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_online_paid),rule=2)
bfs_caut[3] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$b_time_of_semester),rule=2)
bfs_caut[4] = dstudent_t(0,3,0,10)/d_post(0)

d_post = approxfun(density(samples$sd_subject__Intercept),rule=2)
bfs_caut[5] = 2*dstudent_t(0,3,0,10)/d_post(0)  #numerator is multiplied by 2 because for sigma the distribution is folded at 0.

d_post = approxfun(density(samples$sigma),rule=2)
bfs_caut[6] = 2*dstudent_t(0,3,0,500)/d_post(0)  #numerator is multiplied by 2 because for sigma the distribution is folded at 0.

#prepare performance results table
results_tmp =round(
            rbind(
              rep(NA,6),
              summary(fit_performance)$fixed,
              summary(fit_performance)$random$subject,
              summary(fit_performance)$spec_pars,
              rep(NA,6),
              summary(fit_caution)$fixed,
              summary(fit_caution)$random$s,
              summary(fit_performance)$spec_pars),
            digits=2)

results = cbind(results_tmp,round(c(NA,bfs_perf,NA,bfs_caut),2))

rownames(results) <- c("Performance","Intercept","Local Paid","Online Paid","Time of Semester","Intercept SD","Residual SD",
                       "Caution","Intercept","Local Paid","Online Paid","Time of Semester","Intercept SD","Residual SD")
colnames(results) <- c("Estimate","SE","Lower CI","Upper CI","Eff. Sample","Rhat","BF")

latex_table=xtable(results,
                   align=c("X",rep("r",ncol(results)-1)),
                   caption="Results for the Bayesian Mixed Modeling Analysis of Expected Performance and Caution in Study 1",
                   label = "tab:study1_res")

addtorow <- list()
addtorow$pos <- list(14)
addtorow$command <- c("\\hline  \\multicolumn{8}{p\\textwidth}{Note:
                      The lower CI and Upper CI represent the lower and upper bounds on the 95\\% credible interval.} \\\\ ")


print(latex_table,
      add.to.row=addtorow,
      tabular.environment = "tabularx",
      width = "\\textwidth",
      hline.after=c(-1,0),
      caption.placement = "top")



