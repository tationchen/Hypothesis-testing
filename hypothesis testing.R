d20<-read.csv("C:/Users/Tation'chan/Desktop/pstat 120c/polls_election/data/president_polls_2020.csv",header = T)
#View(d20)
library(lubridate) ##this is to change the format of date
date_2020= mdy(d20$end_date)
date_2020_latest_day=date_2020[1]
index_selected=which(date_2020>='2020-08-01') 
d20=d20[index_selected,]  ###only work on the poll after Aug 1
##get  values with sample size unknown
index_na=which(is.na(d20$sample_size)==T)
index_na
##only look at state Michigan
##d20=d20[which(d20$state=='Michigan'),]
View(d20)
##only look at Biden or Trump 
d20=d20[which(d20$answer=='Biden'|d20$answer=='Trump'),]
# ##you may delete USC Dornsife/Los Angeles Times and Survey Monkey as their polls seem not disjoint
d20=d20[which(d20$pollster_id!=1610&d20$pollster_id!=1193),]
##they do not match. Some poll has mistakes 
length(which(d20$answer=='Biden'))
length(which(d20$answer=='Trump'))
##now let's delete those poll that only contains one candidate
polls_data_2020_question_id_num=unique(d20$question_id)

for(i in 1:length(unique(d20$question_id)) ){
  index_set=which(d20$question_id==polls_data_2020_question_id_num[i])
  if(length(index_set)!=2){
    d20=d20[-index_set,]
  }
}
##now they match 
length(which(d20$answer=='Biden'))
length(which(d20$answer=='Trump'))

###delete sample size NA
index_NA=which(is.na(d20$sample_size)==T)
index_NA
#d20=d20[-index_NA,]
View(d20)
###
index_trump=which(d20$answer=='Trump')
index_biden=which(d20$answer=='Biden')
##total counts to trump
total_count_trump=sum(d20$pct[index_trump]*d20$sample_size[index_trump])
total_count_biden
length(d20$pct[index_trump]*d20$sample_size[index_trump])
##total counts to biden 
total_count_biden=sum(d20$pct[index_biden]*d20$sample_size[index_biden])
total_count_trump
length(d20$pct[index_biden]*d20$sample_size[index_biden])
#b
t.test(d20$pct[index_biden]*d20$sample_size[index_biden],d20$pct[index_trump]*d20$sample_size[index_trump],Pair=T)
#t.test(d20$pct[index_biden],d20$pct[index_trump],pair=T )
#c
wilcox.test(d20$pct[index_biden]*d20$sample_size[index_biden],d20$pct[index_trump]*d20$sample_size[index_trump],Pair=T)
#d
