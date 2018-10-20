

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
         time_of_semester = factor(time_of_semester,levels=c('early','late'),labels=c('First\n3 Weeks','Final\n3 Weeks')),
         population = factor(population,levels=c('undergrads','community','mturk'),labels=c('Undergraduate Sample','Paid Local Sample','Mechanical Turk Sample')))



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


#Run statistical analysis and store results

library(BayesFactor)
library(xtable)

options(scipen = 1)
options(digits = 2)

result1 = extractBF(anovaBF(value ~ time_of_semester*population,whichRandom="subject", data=data[data$outcome=="Performance",]))
result2 = extractBF(anovaBF(value ~ time_of_semester*population,whichRandom="subject", data=data[data$outcome=="Caution",]))

models=c('Time of Semester','Sample','Both Main Effects','Main Effects + Interation')

results = cbind(result1[,1:2],result2[1:2])
rownames(results)<-models
colnames(results)<-c('BF','Error','BF','Error')
latex_table=xtable(results,
                   align=c("X",rep("c",ncol(result1))),
                   caption="Results for the Bayesian ANOVAs on Performance and Caution",
                   label = "tab:perf_res",
                   digits = -2)

addtorow <- list()
addtorow$pos <- list(-1,4)
addtorow$command <- c("\\hline  &  \\multicolumn{2}{c}{Performance} & \\multicolumn{2}{c}{Caution}  \\\\ \\cline{2-5} ",
                      "\\hline \\multicolumn{5}{p\\textwidth}{
                      Note: The reported Bayes factors represent comparisons with the intercept-only model.} \\\\ ")

print(latex_table,
      add.to.row=addtorow,
      tabular.environment = "tabularx",
      width = "\\textwidth",
      hline.after=c(0),
      caption.placement = "top",
      math.style.exponents = TRUE)

#Need to manually adjust columns to delete instances of "x 10^1"






