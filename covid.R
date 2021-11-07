library(tidyverse)
library(dplyr)
library(ggplot2)

covid <- read.csv("covidkab.csv")
ls(covid)

#data cleaning
coviddata <- covid
coviddata$sex[coviddata$sex %in% "หญิง" ] <- "F"
coviddata$sex[coviddata$sex %in% "ชาย" ] <- "M"
coviddata$birth[coviddata$birth %in% "พ.ศ.2523-2540" ] <- "genY"
coviddata$birth[coviddata$birth %in% "พ.ศ.2508-2522" ] <- "genX"
coviddata$birth[coviddata$birth %in% "พ.ศ. 2489-2507" ] <- "bbbm"
coviddata$education[coviddata$education %in% "ต่ำกว่าปริญญาตรี" ] <- "undergraduate"
coviddata$education[coviddata$education %in% "ปริญญาตรี" ] <- "bachelor"
coviddata$education[coviddata$education %in% "สูงกว่าปริญญาตรี" ] <- "higher"

                    
#data manipulation
babyboomer_data <- coviddata %>% filter(birth %in% 'bbbm')
genX_data <- coviddata %>% filter(birth %in% 'genX')
genY_data <- coviddata %>% filter(birth %in% 'genY')
female_data <- coviddata %>% filter(sex %in% 'F')
male_data <- coviddata %>% filter(sex %in% 'M')

#part1
coviddata %>% group_by(sex) %>% summarise(n=n()) %>% mutate(percent = n/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=sex,fill=sex))

coviddata %>% group_by(education) %>% summarise(n=n()) %>% mutate(percent = n/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=education,fill=education)) 

coviddata %>% group_by(birth) %>% summarise(n=n()) %>% mutate(percent = n/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=birth,fill=birth))


#part2
#wear_mask
coviddata %>% group_by(wear_mask) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wear_mask*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=wear_mask))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(wear_mask) %>% summarise(n=n())%>%
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wear_mask*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(wear_mask) %>% summarise(n=n())%>%
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wear_mask*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(wear_mask) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wear_mask*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=wear_mask,fill=birth)) + facet_wrap(~birth)


#have_alcohol_gel
coviddata %>% group_by(have_alcohol_gel) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(have_alcohol_gel*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=have_alcohol_gel))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(have_alcohol_gel) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(have_alcohol_gel*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(have_alcohol_gel) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(have_alcohol_gel*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(have_alcohol_gel) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(have_alcohol_gel*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=have_alcohol_gel,fill=birth)) + facet_wrap(~birth)


#wash_hand
coviddata %>% group_by(wash_hand) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wash_hand*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=wash_hand))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(wash_hand) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wash_hand*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(wash_hand) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wash_hand*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(wash_hand) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(wash_hand*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=wash_hand,fill=birth)) + facet_wrap(~birth)


#distancing
coviddata %>% group_by(distancing) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(distancing*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=distancing))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(distancing) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(distancing*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(distancing) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(distancing*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(distancing) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(distancing*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=distancing,fill=birth)) + facet_wrap(~birth)


#eat_alone
coviddata %>% group_by(eat_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(eat_alone*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=eat_alone))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(eat_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(eat_alone*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(eat_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(eat_alone*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(eat_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(eat_alone*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=eat_alone,fill=birth)) + facet_wrap(~birth)


#no_activity
coviddata %>% group_by(no_activity) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(no_activity*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=no_activity))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(no_activity) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(no_activity*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(no_activity) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(no_activity*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(no_activity) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(no_activity*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=no_activity,fill=birth)) + facet_wrap(~birth)


#online_payment
coviddata %>% group_by(online_payment) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_payment*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_payment))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(online_payment) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_payment*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(online_payment) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_payment*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(online_payment) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_payment*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_payment,fill=birth)) + facet_wrap(~birth)


#online_shoping
coviddata %>% group_by(online_shoping) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_shoping*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_shoping))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(online_shoping) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_shoping*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(online_shoping) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_shoping*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(online_shoping) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_shoping*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_shoping,fill=birth)) + facet_wrap(~birth)


#delivery
coviddata %>% group_by(delivery) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(delivery*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=delivery))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(delivery) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(delivery*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(delivery) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(delivery*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(delivery) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(delivery*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=delivery,fill=birth)) + facet_wrap(~birth)

#online_communicate
coviddata %>% group_by(online_communicate) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_communicate*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_communicate))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(online_communicate) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_communicate*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(online_communicate) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_communicate*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(online_communicate) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(online_communicate*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=online_communicate,fill=birth)) + facet_wrap(~birth)


#social_media
coviddata %>% group_by(social_media) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(social_media*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=social_media))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(social_media) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(social_media*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(social_media) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(social_media*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(social_media) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(social_media*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=social_media,fill=birth)) + facet_wrap(~birth)


#application_regis
coviddata %>% group_by(application_regis) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(application_regis*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=application_regis))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(application_regis) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(application_regis*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(application_regis) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(application_regis*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(application_regis) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(application_regis*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=application_regis,fill=birth)) + facet_wrap(~birth)

#workout_alone
coviddata %>% group_by(workout_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(workout_alone*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=workout_alone))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(workout_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(workout_alone*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(workout_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(workout_alone*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(workout_alone) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(workout_alone*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=workout_alone,fill=birth)) + facet_wrap(~birth)


#healt_care
coviddata %>% group_by(healt_care) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(healt_care*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=healt_care))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(healt_care) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(healt_care*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(healt_care) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(healt_care*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(healt_care) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(healt_care*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=healt_care,fill=birth)) + facet_wrap(~birth)


#WFH
coviddata %>% group_by(WFH) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(WFH*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=WFH))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(WFH) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(WFH*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(WFH) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(WFH*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(WFH) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(WFH*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=WFH,fill=birth)) + facet_wrap(~birth)

#expenditure_increase
coviddata %>% group_by(expenditure_increase) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(expenditure_increase*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=expenditure_increase))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(expenditure_increase) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(expenditure_increase*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(expenditure_increase) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(expenditure_increase*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(expenditure_increase) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(expenditure_increase*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=expenditure_increase,fill=birth)) + facet_wrap(~birth)

#enough_money
coviddata %>% group_by(enough_money) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(enough_money*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(enough_money))

coviddata %>% filter(birth %in% 'bbbm') %>% group_by(enough_money) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(enough_money*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genX') %>% group_by(enough_money) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(enough_money*n)/sum(n))%>% view()
coviddata %>% filter(birth %in% 'genY') %>% group_by(enough_money) %>% summarise(n=n())%>% 
  mutate(percent = n/sum(n))%>%
  mutate(avg_rate = sum(enough_money*n)/sum(n))%>% view()
ggplot(coviddata) + geom_bar(aes(x=enough_money,fill=birth)) + facet_wrap(~birth)
