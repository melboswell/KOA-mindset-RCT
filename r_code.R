library(dplyr)
library(tidyverse)
data = read.csv("deidentified_data.csv")
n = as.numeric(table(data$t2_Group.ID))


table_1 = data %>% select(matches('Group.ID|PEM|IMI|KOAS|AAM'))

table_1_summary = table_1 %>% group_by(t2_Group.ID) 
k = (ncol(table_1_summary) - 1)/3
names_working = colnames(table_1_summary)
table_1_summary = table_1_summary %>% summarise_all(c(mean = mean, sd = sd))


final_table_1_summary = NULL
final_table_1_summary = data.frame(Group_ID = table_1_summary$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(table_1_summary[i,(2*j)], table_1_summary[i,(2*j+1)], sep= ","))
  }
  final_table_1_summary = cbind(final_table_1_summary, a)
  colnames(final_table_1_summary)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(table_1_summary[i,(2*j)], table_1_summary[i,(2*j+1)], sep= ","))
  }
  final_table_1_summary = cbind(final_table_1_summary, a)
  colnames(final_table_1_summary)[(j+1)] = names_working[(j+1)]
}



differences_table_1 = table_1

differences_table_1[,1] = as.factor(table_1$t2_Group.ID)
colnames(differences_table_1)[1] = 'Group_ID'



for(i in 2:(k+1)){
  differences_table_1[, i] = table_1[,i+k] - table_1[(i-1)] 
  colnames(differences_table_1)[i] = paste("t3_minus",colnames(table_1)[(i-1)],sep="_")
  
}



differences_table_1 = differences_table_1 [,1:(k+1)]
differences_table_1_summarised = differences_table_1 %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))



table_1_cohen = data.frame(Group_ID = table_1_summary$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_table_1_summarised[i,(2*j)],"(", differences_table_1_summarised[i,(2*j)] - 1.96* differences_table_1_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_table_1_summarised[i,(2*j)] + 1.96* differences_table_1_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  table_1_cohen = cbind(table_1_cohen, a)
  cohen_d = differences_table_1_summarised[,(2*j)]/differences_table_1_summarised[,(2*j+1)]
  table_1_cohen = cbind(table_1_cohen, cohen_d)
  colnames(table_1_cohen)[2*j] = colnames(differences_table_1)[(j+1)]
  colnames(table_1_cohen)[2*j+1] = paste(colnames(differences_table_1)[(j+1)], "cohen_d", sep = "_")
}

table_1_summarised = merge(final_table_1_summary, table_1_cohen, by = "Group_ID")




n1 = as.numeric(table(data$t2_Group.ID)[1])
n2 = as.numeric(table(data$t2_Group.ID)[2])
n3 = as.numeric(table(data$t2_Group.ID)[3])
differences_comparison_between_groups = matrix(0,3,k)
p_values = matrix(0,3,k)
for(i in 1:k){
  mean = as.numeric(differences_table_1_summarised[1,2*i] - differences_table_1_summarised[3,2*i])
  sdv = as.numeric(sqrt(((n1-1)*(differences_table_1_summarised[1,2*i+1])^2 + (n3-1)*(differences_table_1_summarised[3,2*i+1])^2)*(1/n1+1/n3)/(n1+n3-2)))
  statistics = mean/sdv
  pval = 1-pnorm(abs(mean/sdv))
  differences_comparison_between_groups[1,i] = paste(mean,sdv,sep=",")
  p_values[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}

for(i in 1:k){
  mean = as.numeric(differences_table_1_summarised[2,2*i] - differences_table_1_summarised[3,2*i])
  sdv = as.numeric(sqrt(((n2-1)*(differences_table_1_summarised[2,2*i+1])^2 + (n3-1)*(differences_table_1_summarised[3,2*i+1])^2)*(1/n2+1/n3)/(n2+n3-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups[2,i] = paste(mean,sdv,sep=",")
  p_values[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}

for(i in 1:k){
  mean = as.numeric(differences_table_1_summarised[2,2*i] - differences_table_1_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n2-1)*(differences_table_1_summarised[2,2*i+1])^2 + (n1-1)*(differences_table_1_summarised[1,2*i+1])^2)*(1/n2+1/n1)/(n2+n1-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups[3,i] = paste(mean,sdv,sep=",")
  p_values[3,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups = data.frame(differences_comparison_between_groups)

colnames(differences_comparison_between_groups) = colnames(differences_table_1)[2:(k+1)]  
rownames(differences_comparison_between_groups) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values = data.frame(p_values)
colnames(p_values) = colnames(differences_table_1)[2:(k+1)]
rownames(p_values) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")


write.csv(table_1_summarised,"table_1_part_1_summarised.csv",row.names = FALSE)
write.csv(differences_comparison_between_groups,"differences_comparison_between_groups_table_1.csv")

write.csv(p_values,"p_values_table_1.csv")


rm(list = ls())

data = read.csv("deidentified_data.csv")
n = as.numeric(table(data$t2_Group.ID))

table_2 = data %>% select(matches('Group.ID|pain|PASE_total|shortMAC_pain_sum|shortMAC_function_sum|t1_surgery|t3_surgery|Global_physical_avg|Global_mental_avg|t1_TSK_avg|t1_ASE_pain_avg|t3_ASE_pain_avg|t1_ASE_function_avg|t3_ASE_function_avg|t3_TSK_avg|t3_pain_ASE_avg|t1_management_|t3_management_') & !matches('pain.time|t2_ASE'))


table_2_summary = table_2 %>% group_by(t2_Group.ID) 
k = (ncol(table_2_summary) - 1)/2
names_working = colnames(table_2_summary)
table_2_summary = table_2_summary %>% summarise(across(everything(), list(mean = mean, sd = sd)))


final_table_2_summary = NULL
final_table_2_summary = data.frame(Group_ID = table_2_summary$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(table_2_summary[i,(2*j)], table_2_summary[i,(2*j+1)], sep= ","))
  }
  final_table_2_summary = cbind(final_table_2_summary, a)
  colnames(final_table_2_summary)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(table_2_summary[i,(2*j)], table_2_summary[i,(2*j+1)], sep= ","))
  }
  final_table_2_summary = cbind(final_table_2_summary, a)
  colnames(final_table_2_summary)[(j+1)] = names_working[(j+1)]
}

#View(table_2_summary)


differences_table_2 = table_2

differences_table_2[,1] = as.factor(table_2$t2_Group.ID)
colnames(differences_table_2)[1] = 'Group_ID'



for(i in 2:(k+1)){
  differences_table_2[, i] = table_2[,i+k] - table_2[(i-1)]
  colnames(differences_table_2)[i] = paste("t3_minus",colnames(table_2)[(i-1)],sep="_")
  
}



differences_table_2 = differences_table_2 [,1:(k+1)]
differences_table_2_summarised = differences_table_2 %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


table_2_cohen = data.frame(Group_ID = table_2_summary$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_table_2_summarised[i,(2*j)],"(", differences_table_2_summarised[i,(2*j)] - 1.96* differences_table_2_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_table_2_summarised[i,(2*j)] + 1.96* differences_table_2_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  table_2_cohen = cbind(table_2_cohen, a)
  cohen_d = differences_table_2_summarised[,(2*j)]/differences_table_2_summarised[,(2*j+1)]
  table_2_cohen = cbind(table_2_cohen, cohen_d)
  colnames(table_2_cohen)[2*j] = colnames(differences_table_2)[(j+1)]
  colnames(table_2_cohen)[2*j+1] = paste(colnames(differences_table_2)[(j+1)], "cohen_d", sep = "_")
}

table_2_summarised = merge(final_table_2_summary, table_2_cohen, by = "Group_ID")

n1 = as.numeric(table(data$t2_Group.ID)[1])
n2 = as.numeric(table(data$t2_Group.ID)[2])
n3 = as.numeric(table(data$t2_Group.ID)[3])
differences_comparison_between_groups_table_2 = matrix(0,3,k)
p_values_table_2 = matrix(0,3,k)
for(i in 1:k){
  mean = as.numeric(differences_table_2_summarised[1,2*i] - differences_table_2_summarised[3,2*i])
  sdv = as.numeric(sqrt(((n1-1)*(differences_table_2_summarised[1,2*i+1])^2 + (n3-1)*(differences_table_2_summarised[3,2*i+1])^2)*(1/n1+1/n3)/(n1+n3-2)))
  statistics = mean/sdv
  pval = 1-pnorm(abs(mean/sdv))
  differences_comparison_between_groups_table_2[1,i] = paste(mean,sdv,sep=",")
  p_values_table_2[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}

for(i in 1:k){
  mean = as.numeric(differences_table_2_summarised[2,2*i] - differences_table_2_summarised[3,2*i])
  sdv = as.numeric(sqrt(((n2-1)*(differences_table_2_summarised[2,2*i+1])^2 + (n3-1)*(differences_table_2_summarised[3,2*i+1])^2)*(1/n2+1/n3)/(n2+n3-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_table_2[2,i] = paste(mean,sdv,sep=",")
  p_values_table_2[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}

for(i in 1:k){
  mean = as.numeric(differences_table_2_summarised[2,2*i] - differences_table_2_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n2-1)*(differences_table_2_summarised[2,2*i+1])^2 + (n1-1)*(differences_table_2_summarised[1,2*i+1])^2)*(1/n2+1/n1)/(n2+n1-2)))
  statistics = mean/sdv
  pval = 1-pnorm(abs(mean/sdv))
  differences_comparison_between_groups_table_2[3,i] = paste(mean,sdv,sep=",")
  p_values_table_2[3,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_table_2 = data.frame(differences_comparison_between_groups_table_2)

colnames(differences_comparison_between_groups_table_2) = colnames(differences_table_2)[2:(k+1)]  
rownames(differences_comparison_between_groups_table_2) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_table_2 = data.frame(p_values_table_2)
colnames(p_values_table_2) = colnames(differences_table_2)[2:(k+1)]
rownames(p_values_table_2) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")


write.csv(table_2_summarised,"table_2_part_1_summarised.csv",row.names = FALSE)

write.csv(differences_comparison_between_groups_table_2,"differences_comparison_between_groups_table_2.csv")

write.csv(p_values_table_2,"p_values_table_2.csv")

rm(list = ls())

data = read.csv("deidentified_data.csv")
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
for(colm in c("t1_med.cond","t1_employment","t1_race")){
  for(i in 1:nrow(data)){
    data[[colm]][i] = strsplit(data[[colm]][i][[1]], ",")
  }
}

l = list()
l[[1]] = c(1:14,17,18)
l[[2]] = c(1:10)
l[[3]] = 1:7
l[[4]] = c(0,1,3)
l[[5]] = c(0,1,2,5)
l[[6]] = c(4,5)
k = 1
for(colm in c("t1_med.cond","t1_employment","t1_race","t1_sex","t1_gender","t1_uni.bi")){
  for(j in l[[k]]){
    s = paste(colm,as.character(j),sep="_")
    data[[s]] = numeric(nrow(data))
    for(i in 1:nrow(data)){
      data[[s]][i] = ifelse(as.character(j) %in% data[[colm]][i][[1]],1,0)
    }
  }
  k = k+1
}




data_categorical = data %>% select(matches('Group|sex_|gender_|race_|employment_|uni.bi_|med.cond_'))

data_categorical$t2_Group.ID = as.factor(data_categorical$t2_Group.ID)
data_categorical_summary = data_categorical %>% group_by(t2_Group.ID) %>% summarise_all(list(sum = sum))
data_categorical_min_max = data_categorical%>% group_by(t2_Group.ID) %>% summarise_all(list(min = min, max = max))
k = ncol(data_categorical_summary)
n = as.numeric(table(data$t2_Group.ID))
p_values = c()
for(i in 2:ncol(data_categorical)){
  if(!(all(data_categorical_summary[,i]==c(0,0,0)))){
    fit = chisq.test(x = data_categorical_summary[,i], p = rep(1/3,3))
    p_values = c(p_values, fit$p.value)
  }else{
    p_values = c(p_values, NA)
  }
  
}
#View(data_categorical) 


new_results = matrix(0,3,k-1)
for(j in 2:k){
  for(i in 1:3){
    mu = as.numeric(data_categorical_summary[i,j]/n[i])
    minimum = as.numeric(data_categorical_min_max[i,j])
    maximum = as.numeric(data_categorical_min_max[i,(k+j-1)])
    new_results[i,j-1] = paste(mu*100, "%," , 100*sqrt(mu*(1-mu)), "%", sep ="")
  }
  
}
new_results = data.frame(new_results)
colnames(new_results) = colnames(data_categorical_summary)[2:k]
new_results = rbind(new_results,p_values)
rownames(new_results) = c("Group 1", "Group 2", "Group 3", "p value")

write.csv(new_results,"Supplementary_table_for_categorical.csv")
data_continuous = data %>% select(any_of(c('t2_Group.ID',paste("t1",c('age','pain','bmi', 'pain.time'),sep="_"))))

data_continuous_summary = data_continuous %>% group_by(t2_Group.ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))
data_continuous_min_max = data_continuous %>% group_by(t2_Group.ID) %>% summarise(across(everything(), list(min = min, max = max)))
#table_1_continuous_summary = table_1_continuous_summary[, !apply(is.na(table_1_continuous_summary), 2, any)]

data_continuous$t2_Group.ID = as.factor(data_continuous$t2_Group.ID)
k = ncol(data_continuous)

n = as.numeric(table(data$t2_Group.ID))


#View(data_categorical) 


new_results = matrix(0,3,k-1)
for(j in 1:(k-1)){
  for(i in 1:3){
    mean = data_continuous_summary[i,2*j]
    std = data_continuous_summary[i,(2*j+1)]
    minimum = data_continuous_min_max[i,2*j]
    maximum = data_continuous_min_max[i,(2*j+1)]
    new_results[i,j] = paste(mean , std, minimum, maximum, sep =",")
  }
  
}

p_values = c()
for(i in 2:ncol(data_continuous)){
  fit = lm(data_continuous[,i]~data_continuous[,1])
  p_values = c(p_values, overall_p(fit))
}
new_results = data.frame(new_results)
colnames(new_results) = colnames(data_continuous)[2:k]
new_results = rbind(new_results,p_values)
rownames(new_results) = c("Group 1", "Group 2", "Group 3", "p value")

write.csv(new_results,"Supplementary_table_for continuous.csv")

enrolled_participants = read.csv("enrolled_participants.csv")


enrolled_participants = enrolled_participants %>% select(matches('Email|_age|bmi|sex|gender|employment|med.cond|uni.bi|pain.time|pain|complete|race'))
enrolled_participants = enrolled_participants[, -1]
# View(enrolled_participants)
for(colm in c("t1_med.cond","t1_employment","t1_race", "t1_uni.bi")){
  for(i in 1:nrow(enrolled_participants)){
    enrolled_participants[[colm]][i] = strsplit(as.character(enrolled_participants[[colm]][i][[1]]), ",")
  }
}

l = list()
l[[1]] = c(1:14,17,18)
l[[2]] = c(1:10)
l[[3]] = 1:7
l[[4]] = c(0,1,3)
l[[5]] = c(0,1,2,5)
l[[6]] = c(4,5)
k = 1
for(colm in c("t1_med.cond","t1_employment","t1_race","t1_sex","t1_gender","t1_uni.bi")){
  for(j in l[[k]]){
    s = paste(colm,as.character(j),sep="_")
    enrolled_participants[[s]] = numeric(nrow(enrolled_participants))
    for(i in 1:nrow(enrolled_participants)){
      enrolled_participants[[s]][i] = ifelse(as.character(j) %in% enrolled_participants[[colm]][i][[1]],1,0)
    }
  }
  k = k+1
}


enrolled_participants$t1_employment_1 = ifelse(enrolled_participants$t1_employment_1>0 | enrolled_participants$t1_employment_2>0 , 1, 0)

enrolled_participants = enrolled_participants %>% select(matches('Email|pain.|age|gender_|pain|employment_1|race_|uni.bi_|sex_|med.cond_|complete|bmi'))

View(enrolled_participants)

enrolled_participants = enrolled_participants %>% select(!matches('employment_10|pain_sum'))


enrolled_participants_categorical = enrolled_participants %>% select(matches('med.cond_|employment_1|race_|sex_|gender_|uni.bi_|complete'))

View(enrolled_participants_categorical)

enrolled_participants_categorical_summary = enrolled_participants_categorical %>% group_by(complete) %>% summarise_all(list(sum = sum))
write.csv(enrolled_participants_categorical_summary, "enrolled_participants_cat_summary.csv")


enrolled_participants_continuous = enrolled_participants %>% select(matches('complete|pain|age|bmi'))
enrolled_participants_continuous_summary = enrolled_participants_continuous %>% group_by(complete) %>% summarise_all(list(mean = mean, sd = sd))

View(enrolled_participants_continuous_summary)
write.csv(enrolled_participants_continuous_summary, "enrolled_participants_cont_summary.csv")


p_values = c()
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,c(2:14,16,17)]))
p_values = c(p_values, fit$p.value)
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,18]))
p_values = c(p_values, fit$p.value)
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,c(19:21,23:25)]))
p_values = c(p_values, fit$p.value)
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,c(26:28)]))
p_values = c(p_values, fit$p.value)
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,c(29:32)]))
p_values = c(p_values, fit$p.value)
fit = chisq.test(as.matrix(enrolled_participants_categorical_summary[,c(33:34)]))
p_values = c(p_values, fit$p.value)

p_values = data.frame(name = c("med.cond", "employment", "race", "sex", "gender", "uni.bi"), p_values = p_values)
write.csv(p_values, "p_values_categorical_dropouts.csv")


supplementary_table = data %>% select(matches('Group.ID|total.time'))
supplementary_table = na.omit(supplementary_table)
supplementary_table = supplementary_table[,-1]
supplementary_table = supplementary_table[!(supplementary_table$t1_stepCount==""),]
supplementary_table = supplementary_table[!(supplementary_table$t3_stepCount==""),]
supplementary_table = supplementary_table[!(supplementary_table$t1_stepCount==0),]
supplementary_table = supplementary_table[!(supplementary_table$t3_stepCount==0),]
for(j in 1:ncol(supplementary_table)){
  supplementary_table[,j] = as.numeric(supplementary_table[,j])
}

#supplementary_table$t2_Group.ID = ifelse(supplementary_table$t2_Group.ID == 1, 2, supplementary_table$t2_Group.ID)
supplementary_table$t2_Group.ID = as.factor(supplementary_table$t2_Group.ID)

dim(supplementary_table)

summary_supplementary_table = supplementary_table %>% group_by(t2_Group.ID) 
k = (ncol(summary_supplementary_table) - 1)/2
names_working = colnames(summary_supplementary_table)
summary_supplementary_table = summary_supplementary_table %>% summarise(across(everything(), list(mean = mean, sd = sd)))

#head(summary_supplementary_table)

## group sizes
n = as.numeric(table(supplementary_table$t2_Group.ID))

n

final_summary_supplementary_table = NULL
final_summary_supplementary_table = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[(j+1)]
}


differences_supplementary_table = supplementary_table

differences_supplementary_table[,1] = as.factor(differences_supplementary_table$t2_Group.ID)
colnames(differences_supplementary_table)[1] = 'Group_ID'

for(i in 2:(k+1)){
  differences_supplementary_table[, i] = supplementary_table[(i-1)] - supplementary_table[,i+k] 
  colnames(differences_supplementary_table)[i] = paste("t1_minus",colnames(supplementary_table)[(i+k)],sep="_")
  
}

differences_supplementary_table = differences_supplementary_table[,1:(k+1)]
differences_supplementary_table_summarised = differences_supplementary_table %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


differences_supplementary_table_cohen = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_supplementary_table_summarised[i,(2*j)],"(", differences_supplementary_table_summarised[i,(2*j)] - 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_supplementary_table_summarised[i,(2*j)] + 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, a)
  cohen_d = differences_supplementary_table_summarised[,(2*j)]/differences_supplementary_table_summarised[,(2*j+1)]
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, cohen_d)
  colnames(differences_supplementary_table_cohen)[2*j] = colnames(differences_supplementary_table_summarised[(j+1)])
  colnames(differences_supplementary_table_cohen)[2*j+1] = paste(colnames(differences_supplementary_table_summarised)[(j+1)], "cohen_d", sep = "_")
}

final_summary_supplementary_table = merge(final_summary_supplementary_table, differences_supplementary_table_cohen, by = "Group_ID")



differences_comparison_between_groups_supplementary_table = matrix(0,3,k)
p_values_supplementary_table = matrix(0,3,k)
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[1,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[1]+1/n[3])/(n[1]+n[3]-2)))
#   statistics = mean/sdv
#   pval = 1-pnorm(abs(mean/sdv))
#   differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }
# 
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[2]+1/n[3])/(n[2] + n[3] - 2)))
#   pval = 1-pnorm(abs(mean/sdv))
#   statistics = mean/sdv
#   differences_comparison_between_groups_supplementary_table[2,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }

for(i in 1:k){
  mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2)*(1/n[2]+1/n[1])/(n[2]+n[1]-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
  p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_supplementary_table = data.frame(differences_comparison_between_groups_supplementary_table)

colnames(differences_comparison_between_groups_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]  
rownames(differences_comparison_between_groups_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_supplementary_table = data.frame(p_values_supplementary_table)
colnames(p_values_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]
rownames(p_values_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")


supplementary_table = data %>% select(matches('Group.ID|stepCount'))
supplementary_table = na.omit(supplementary_table)
supplementary_table = supplementary_table[!(supplementary_table$t1_stepCount==""),]
supplementary_table = supplementary_table[!(supplementary_table$t3_stepCount==""),]
supplementary_table = supplementary_table[!(supplementary_table$t1_stepCount==0),]
supplementary_table = supplementary_table[!(supplementary_table$t3_stepCount==0),]
for(j in 1:ncol(supplementary_table)){
  supplementary_table[,j] = as.numeric(supplementary_table[,j])
}

#supplementary_table$t2_Group.ID = ifelse(supplementary_table$t2_Group.ID == 1, 2, supplementary_table$t2_Group.ID)
supplementary_table$t2_Group.ID = as.factor(supplementary_table$t2_Group.ID)

dim(supplementary_table)

summary_supplementary_table = supplementary_table %>% group_by(t2_Group.ID) 
k = (ncol(summary_supplementary_table) - 1)/2
names_working = colnames(summary_supplementary_table)
summary_supplementary_table = summary_supplementary_table %>% summarise(across(everything(), list(mean = mean, sd = sd)))

#head(summary_supplementary_table)

## group sizes
n = as.numeric(table(supplementary_table$t2_Group.ID))

n

final_summary_supplementary_table = NULL
final_summary_supplementary_table = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[(j+1)]
}


differences_supplementary_table = supplementary_table

differences_supplementary_table[,1] = as.factor(differences_supplementary_table$t2_Group.ID)
colnames(differences_supplementary_table)[1] = 'Group_ID'

for(i in 2:(k+1)){
  differences_supplementary_table[, i] = supplementary_table[(i-1)] - supplementary_table[,i+k] 
  colnames(differences_supplementary_table)[i] = paste("t1_minus",colnames(supplementary_table)[(i+k)],sep="_")
  
}

differences_supplementary_table = differences_supplementary_table[,1:(k+1)]
differences_supplementary_table_summarised = differences_supplementary_table %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


differences_supplementary_table_cohen = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_supplementary_table_summarised[i,(2*j)],"(", differences_supplementary_table_summarised[i,(2*j)] - 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_supplementary_table_summarised[i,(2*j)] + 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, a)
  cohen_d = differences_supplementary_table_summarised[,(2*j)]/differences_supplementary_table_summarised[,(2*j+1)]
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, cohen_d)
  colnames(differences_supplementary_table_cohen)[2*j] = colnames(differences_supplementary_table_summarised[(j+1)])
  colnames(differences_supplementary_table_cohen)[2*j+1] = paste(colnames(differences_supplementary_table_summarised)[(j+1)], "cohen_d", sep = "_")
}

final_summary_supplementary_table = merge(final_summary_supplementary_table, differences_supplementary_table_cohen, by = "Group_ID")



differences_comparison_between_groups_supplementary_table = matrix(0,3,k)
p_values_supplementary_table = matrix(0,3,k)
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[1,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[1]+1/n[3])/(n[1]+n[3]-2)))
#   statistics = mean/sdv
#   pval = 1-pnorm(abs(mean/sdv))
#   differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }
# 
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[2]+1/n[3])/(n[2] + n[3] - 2)))
#   pval = 1-pnorm(abs(mean/sdv))
#   statistics = mean/sdv
#   differences_comparison_between_groups_supplementary_table[2,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }

for(i in 1:k){
  mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2)*(1/n[2]+1/n[1])/(n[2]+n[1]-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
  p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_supplementary_table = data.frame(differences_comparison_between_groups_supplementary_table)

colnames(differences_comparison_between_groups_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]  
rownames(differences_comparison_between_groups_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_supplementary_table = data.frame(p_values_supplementary_table)
colnames(p_values_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]
rownames(p_values_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")




supplementary_table = data %>% select(matches('Group.ID|total.time'))
supplementary_table = na.omit(supplementary_table)
for(j in 1:ncol(supplementary_table)){
  supplementary_table[,j] = as.numeric(supplementary_table[,j])
}

#supplementary_table$t2_Group.ID = ifelse(supplementary_table$t2_Group.ID == 1, 2, supplementary_table$t2_Group.ID)
supplementary_table$t2_Group.ID = as.factor(supplementary_table$t2_Group.ID)

dim(supplementary_table)

summary_supplementary_table = supplementary_table %>% group_by(t2_Group.ID) 
k = (ncol(summary_supplementary_table) - 1)/2
names_working = colnames(summary_supplementary_table)
summary_supplementary_table = summary_supplementary_table %>% summarise(across(everything(), list(mean = mean, sd = sd)))

#head(summary_supplementary_table)

## group sizes
n = as.numeric(table(supplementary_table$t2_Group.ID))

n

final_summary_supplementary_table = NULL
final_summary_supplementary_table = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[(j+1)]
}


differences_supplementary_table = supplementary_table

differences_supplementary_table[,1] = as.factor(differences_supplementary_table$t2_Group.ID)
colnames(differences_supplementary_table)[1] = 'Group_ID'

for(i in 2:(k+1)){
  differences_supplementary_table[, i] = supplementary_table[(i-1)] - supplementary_table[,i+k] 
  colnames(differences_supplementary_table)[i] = paste("t1_minus",colnames(supplementary_table)[(i+k)],sep="_")
  
}

differences_supplementary_table = differences_supplementary_table[,1:(k+1)]
differences_supplementary_table_summarised = differences_supplementary_table %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


differences_supplementary_table_cohen = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_supplementary_table_summarised[i,(2*j)],"(", differences_supplementary_table_summarised[i,(2*j)] - 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_supplementary_table_summarised[i,(2*j)] + 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, a)
  cohen_d = differences_supplementary_table_summarised[,(2*j)]/differences_supplementary_table_summarised[,(2*j+1)]
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, cohen_d)
  colnames(differences_supplementary_table_cohen)[2*j] = colnames(differences_supplementary_table_summarised[(j+1)])
  colnames(differences_supplementary_table_cohen)[2*j+1] = paste(colnames(differences_supplementary_table_summarised)[(j+1)], "cohen_d", sep = "_")
}

final_summary_supplementary_table = merge(final_summary_supplementary_table, differences_supplementary_table_cohen, by = "Group_ID")



differences_comparison_between_groups_supplementary_table = matrix(0,3,k)
p_values_supplementary_table = matrix(0,3,k)
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[1,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[1]+1/n[3])/(n[1]+n[3]-2)))
#   statistics = mean/sdv
#   pval = 1-pnorm(abs(mean/sdv))
#   differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }
# 
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[2]+1/n[3])/(n[2] + n[3] - 2)))
#   pval = 1-pnorm(abs(mean/sdv))
#   statistics = mean/sdv
#   differences_comparison_between_groups_supplementary_table[2,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }

for(i in 1:k){
  mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2)*(1/n[2]+1/n[1])/(n[2]+n[1]-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
  p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_supplementary_table = data.frame(differences_comparison_between_groups_supplementary_table)

colnames(differences_comparison_between_groups_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]  
rownames(differences_comparison_between_groups_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_supplementary_table = data.frame(p_values_supplementary_table)
colnames(p_values_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]
rownames(p_values_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")



supplementary_table = data %>% select(matches('Group.ID|acceleration'))
supplementary_table = na.omit(supplementary_table)
for(j in 1:ncol(supplementary_table)){
  supplementary_table[,j] = as.numeric(supplementary_table[,j])
}

#supplementary_table$t2_Group.ID = ifelse(supplementary_table$t2_Group.ID == 1, 2, supplementary_table$t2_Group.ID)
supplementary_table$t2_Group.ID = as.factor(supplementary_table$t2_Group.ID)

dim(supplementary_table)

summary_supplementary_table = supplementary_table %>% group_by(t2_Group.ID) 
k = (ncol(summary_supplementary_table) - 1)/2
names_working = colnames(summary_supplementary_table)
summary_supplementary_table = summary_supplementary_table %>% summarise(across(everything(), list(mean = mean, sd = sd)))

#head(summary_supplementary_table)

## group sizes
n = as.numeric(table(supplementary_table$t2_Group.ID))

n

final_summary_supplementary_table = NULL
final_summary_supplementary_table = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[(j+1)]
}


differences_supplementary_table = supplementary_table

differences_supplementary_table[,1] = as.factor(differences_supplementary_table$t2_Group.ID)
colnames(differences_supplementary_table)[1] = 'Group_ID'

for(i in 2:(k+1)){
  differences_supplementary_table[, i] = supplementary_table[(i-1)] - supplementary_table[,i+k] 
  colnames(differences_supplementary_table)[i] = paste("t1_minus",colnames(supplementary_table)[(i+k)],sep="_")
  
}

differences_supplementary_table = differences_supplementary_table[,1:(k+1)]
differences_supplementary_table_summarised = differences_supplementary_table %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


differences_supplementary_table_cohen = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_supplementary_table_summarised[i,(2*j)],"(", differences_supplementary_table_summarised[i,(2*j)] - 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_supplementary_table_summarised[i,(2*j)] + 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, a)
  cohen_d = differences_supplementary_table_summarised[,(2*j)]/differences_supplementary_table_summarised[,(2*j+1)]
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, cohen_d)
  colnames(differences_supplementary_table_cohen)[2*j] = colnames(differences_supplementary_table_summarised[(j+1)])
  colnames(differences_supplementary_table_cohen)[2*j+1] = paste(colnames(differences_supplementary_table_summarised)[(j+1)], "cohen_d", sep = "_")
}

final_summary_supplementary_table = merge(final_summary_supplementary_table, differences_supplementary_table_cohen, by = "Group_ID")



differences_comparison_between_groups_supplementary_table = matrix(0,3,k)
p_values_supplementary_table = matrix(0,3,k)
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[1,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[1]+1/n[3])/(n[1]+n[3]-2)))
#   statistics = mean/sdv
#   pval = 1-pnorm(abs(mean/sdv))
#   differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }
# 
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[2]+1/n[3])/(n[2] + n[3] - 2)))
#   pval = 1-pnorm(abs(mean/sdv))
#   statistics = mean/sdv
#   differences_comparison_between_groups_supplementary_table[2,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }

for(i in 1:k){
  mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2)*(1/n[2]+1/n[1])/(n[2]+n[1]-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
  p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_supplementary_table = data.frame(differences_comparison_between_groups_supplementary_table)

colnames(differences_comparison_between_groups_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]  
rownames(differences_comparison_between_groups_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_supplementary_table = data.frame(p_values_supplementary_table)
colnames(p_values_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]
rownames(p_values_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")


supplementary_table = data %>% select(matches('Group.ID|flexion'))
supplementary_table = na.omit(supplementary_table)
for(j in 1:ncol(supplementary_table)){
  supplementary_table[,j] = as.numeric(supplementary_table[,j])
}

#supplementary_table$t2_Group.ID = ifelse(supplementary_table$t2_Group.ID == 1, 2, supplementary_table$t2_Group.ID)
supplementary_table$t2_Group.ID = as.factor(supplementary_table$t2_Group.ID)

dim(supplementary_table)

summary_supplementary_table = supplementary_table %>% group_by(t2_Group.ID) 
k = (ncol(summary_supplementary_table) - 1)/2
names_working = colnames(summary_supplementary_table)
summary_supplementary_table = summary_supplementary_table %>% summarise(across(everything(), list(mean = mean, sd = sd)))

#head(summary_supplementary_table)

## group sizes
n = as.numeric(table(supplementary_table$t2_Group.ID))

n

final_summary_supplementary_table = NULL
final_summary_supplementary_table = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[j]
}

for(j in (k+1):(2*k)){
  a = c()
  for(i in 1:3){
    a = c(a, paste(summary_supplementary_table[i,(2*j)], summary_supplementary_table[i,(2*j+1)], sep= ","))
  }
  final_summary_supplementary_table = cbind(final_summary_supplementary_table, a)
  colnames(final_summary_supplementary_table)[(j+1)] = names_working[(j+1)]
}


differences_supplementary_table = supplementary_table

differences_supplementary_table[,1] = as.factor(differences_supplementary_table$t2_Group.ID)
colnames(differences_supplementary_table)[1] = 'Group_ID'

for(i in 2:(k+1)){
  differences_supplementary_table[, i] = supplementary_table[(i-1)] - supplementary_table[,i+k] 
  colnames(differences_supplementary_table)[i] = paste("t1_minus",colnames(supplementary_table)[(i+k)],sep="_")
  
}

differences_supplementary_table = differences_supplementary_table[,1:(k+1)]
differences_supplementary_table_summarised = differences_supplementary_table %>% group_by(Group_ID) %>% summarise(across(everything(), list(mean = mean, sd = sd)))


differences_supplementary_table_cohen = data.frame(Group_ID = summary_supplementary_table$t2_Group.ID)

for(j in 1:k){
  a = c()
  for(i in 1:3){
    a = c(a, paste(differences_supplementary_table_summarised[i,(2*j)],"(", differences_supplementary_table_summarised[i,(2*j)] - 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]),",", differences_supplementary_table_summarised[i,(2*j)] + 1.96* differences_supplementary_table_summarised[i,(2*j+1)]/sqrt(n[i]), ")"))
  }
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, a)
  cohen_d = differences_supplementary_table_summarised[,(2*j)]/differences_supplementary_table_summarised[,(2*j+1)]
  differences_supplementary_table_cohen = cbind(differences_supplementary_table_cohen, cohen_d)
  colnames(differences_supplementary_table_cohen)[2*j] = colnames(differences_supplementary_table_summarised[(j+1)])
  colnames(differences_supplementary_table_cohen)[2*j+1] = paste(colnames(differences_supplementary_table_summarised)[(j+1)], "cohen_d", sep = "_")
}

final_summary_supplementary_table = merge(final_summary_supplementary_table, differences_supplementary_table_cohen, by = "Group_ID")



differences_comparison_between_groups_supplementary_table = matrix(0,3,k)
p_values_supplementary_table = matrix(0,3,k)
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[1,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[1]+1/n[3])/(n[1]+n[3]-2)))
#   statistics = mean/sdv
#   pval = 1-pnorm(abs(mean/sdv))
#   differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }
# 
# for(i in 1:k){
#   mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[3,2*i])
#   sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[3]-1)*(differences_supplementary_table_summarised[3,2*i+1])^2)*(1/n[2]+1/n[3])/(n[2] + n[3] - 2)))
#   pval = 1-pnorm(abs(mean/sdv))
#   statistics = mean/sdv
#   differences_comparison_between_groups_supplementary_table[2,i] = paste(mean,sdv,sep=",")
#   p_values_supplementary_table[2,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
# }

for(i in 1:k){
  mean = as.numeric(differences_supplementary_table_summarised[2,2*i] - differences_supplementary_table_summarised[1,2*i])
  sdv = as.numeric(sqrt(((n[2]-1)*(differences_supplementary_table_summarised[2,2*i+1])^2 + (n[1]-1)*(differences_supplementary_table_summarised[1,2*i+1])^2)*(1/n[2]+1/n[1])/(n[2]+n[1]-2)))
  pval = 1-pnorm(abs(mean/sdv))
  statistics = mean/sdv
  differences_comparison_between_groups_supplementary_table[1,i] = paste(mean,sdv,sep=",")
  p_values_supplementary_table[1,i] = paste(pval, "(", mean - 1.96*sdv, mean + 1.96*sdv,")")
}




differences_comparison_between_groups_supplementary_table = data.frame(differences_comparison_between_groups_supplementary_table)

colnames(differences_comparison_between_groups_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]  
rownames(differences_comparison_between_groups_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

p_values_supplementary_table = data.frame(p_values_supplementary_table)
colnames(p_values_supplementary_table) = colnames(differences_supplementary_table)[2:(k+1)]
rownames(p_values_supplementary_table) = c("Group 1 v/s Group 3", "Group 2 v/s Group 3","Group 2 v/s Group 1")

write.csv(p_values_supplementary_table, "p_values_supplementary_table.csv", row.names = FALSE)
