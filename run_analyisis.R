##### REPRODUCIBLE RESEARCH PROJECT 1 ########
setwd("/Users/marlonvinan/Documents/Doctorado/Reproducible_research_course/RepData_PeerAssessment1")
#### LOAD DATA #####
unzip("activity.zip")
activity <- read.csv("activity.csv")
str(activity)
summary(activity)

