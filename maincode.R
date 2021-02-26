package_version(R.version)
#install.packages("haven")
#install.packages("dplyr")
#install.packages("tidyr")
# install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caret")

library(haven)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(caret)
setwd("G:/My Drive/DATA_GD/KMP/raw")
p18<-read_dta("p18v26_KMP_stata.dta",encoding='euc-kr')
p19<-read_dta("p19v26_KMP_stata.dta",encoding='euc-kr')
p20<-read_dta("p20v26_KMP_stata.dta",encoding='euc-kr')
d18<-read_dta("d18v26_KMP_stata.dta",encoding='euc-kr')
d19<-read_dta("d19v26_KMP_stata.dta",encoding='euc-kr')
d20<-read_dta("d20v26_KMP_stata.dta",encoding='euc-kr')
setwd("~/Rproject/firstR")

table(p18$p18j01001) #있음 98명 / 없음 9328명
table(p19$p19j01001) #있음 342명 / 없음 10522명
table(p20$p20j01001) #있음 835명 / 없음 9467명

p18 %>% select(pid,p18j01001) %>% as.data.frame -> d18.1
p19 %>% select(pid,p19j01001) %>% as.data.frame -> d19.1
p20 %>% select(pid,p20j01001) %>% as.data.frame -> d20.1

#2018년에는 없다가 2019년에는 소지한 경우 = 157명
d18.1 %>% inner_join(d19.1) %>% filter(p18j01001==2) %>% 
  mutate(wearable.chg=ifelse(p18j01001==2 & p19j01001==1,1,0)) %>% 
  select(pid, wearable.chg) -> d1819
table(d1819$wearable.chg)

#2019년에는 없다가 2020년에는 소지한 경우 = 535명
d19.1 %>% inner_join(d20.1) %>% filter(p19j01001==2) %>% 
  mutate(wearable.chg=ifelse(p19j01001==2 & p20j01001==1,1,0)) %>% 
  select(pid, wearable.chg) -> d1920
table(d1920$wearable.chg)

#########################
# 프라이버시 우려


p18 %>% select(pid,p18d23001) %>% filter(p18d23001!=8) %>% 
  as.data.frame -> p18.1 #SNS사용안하는 사람 제거
p19 %>% select(pid,p19d23001) %>% filter(p19d23001!=8) %>% 
  as.data.frame -> p19.1 #SNS사용안하는 사람 제거
p20 %>% select(pid,p20d23001) %>% filter(p20d23001!=8) %>% 
  as.data.frame -> p20.1 #SNS사용안하는 사람 제거

# 2018 프라이버시 우려도 그래프  
table(p18.1$p18d23001) %>% as.data.frame %>% setNames(c("pid","concern")) -> p18.1.1
p18.1.1

ggplot(p18.1.1, aes(x=pid, y=concern)) + geom_bar(stat="identity") + 
  labs(title="2018 privacy",x="Concern", y="Respondents")

# 2019 프라이버시 우려도 그래프  
table(p19.1$p19d23001) %>% as.data.frame %>% setNames(c("pid","concern")) -> p19.1.1
p19.1.1

ggplot(p19.1.1, aes(x=pid, y=concern)) + geom_bar(stat="identity") + 
  labs(title="2019 privacy",x="Concern", y="Respondents")

# 2020 프라이버시 우려도 그래프  
table(p20.1$p20d23001) %>% as.data.frame %>% setNames(c("pid","concern")) -> p20.1.1
p20.1.1

ggplot(p20.1.1, aes(x=pid, y=concern)) + geom_bar(stat="identity") + 
  labs(title="2020 privacy",x="Concern", y="Respondents")


p1819 <- p18.1 %>% full_join(p19.1)
p1819.chg <- p1819 %>% mutate(privacy.chg = (p1819[,3]) - (p1819[,2])) %>% 
  filter(!is.na(privacy.chg)) %>% select(pid, privacy.chg)
table(p1819.chg$privacy.chg)

p1920 <- p19.1 %>% full_join(p20.1)
p1920.chg <- p1920 %>% mutate(privacy.chg = (p1920[,3]) - (p1920[,2])) %>% 
  filter(!is.na(privacy.chg)) %>% select(pid, privacy.chg)
table(p1920.chg$privacy.chg)

####################################33
# 성별 / 연령 / 학력

d19 %>% select(pid,d19gender, d19age, d19school) %>% unique -> d19.ctrl
d19.ctrl %>% head
str(d19.ctrl)
d19.ctrl$d19gender %>% table
d19.ctrl$d19age %>% table
d19.ctrl$d19school %>% table

d19.ctrl %>% mutate(
  male=as.factor(ifelse(d19gender==1,1,0)),
  age.gr=as.factor(ifelse(d19age==2, "10s",
                   ifelse(d19age==3, "20s",
                   ifelse(d19age==4, "30s",
                   ifelse(d19age==5, "40s",
                   ifelse(d19age==6, "50s",
                   ifelse(d19age>=7, "60+",NA))))))),
  edu.gr=as.factor(ifelse(d19school>=1 & d19school<=3, "low",
                   ifelse(d19school==4, "mid",
                   ifelse(d19school>=5 & d19school<=6, "high",NA))))
) %>% filter(!is.na(age.gr) & !is.na(edu.gr)) %>% 
  select(pid, male, age.gr, edu.gr) %>% unique -> d19.ctrl

head(d19.ctrl)


d20 %>% select(pid,d20gender, d20age, d20school) %>% unique -> d20.ctrl
d20.ctrl %>% mutate(
  male=as.factor(ifelse(d20gender==1,1,0)),
  age.gr=as.factor(ifelse(d20age==2, "10s",
                          ifelse(d20age==3, "20s",
                                 ifelse(d20age==4, "30s",
                                        ifelse(d20age==5, "40s",
                                               ifelse(d20age==6, "50s",
                                                      ifelse(d20age>=7, "60+",NA))))))),
  edu.gr=as.factor(ifelse(d20school>=1 & d20school<=3, "low",
                          ifelse(d20school==4, "mid",
                                 ifelse(d20school>=5 & d20school<=6, "high",NA))))
) %>% filter(!is.na(age.gr) & !is.na(edu.gr)) %>% 
  select(pid, male, age.gr, edu.gr) %>% unique -> d20.ctrl

head(d20.ctrl)

#2018~2020 디바이스 보유 변화 전체 데이터
head(d1819)
head(d1920)

d1819$year=2019
d1920$year=2020

wearable.sample<-rbind(d1819, d1920)
head(wearable.sample)
table(wearable.sample$wearable.chg)


head(p1819.chg)
head(p1920.chg)
p1819.chg$year=2019
p1920.chg$year=2020

privacy.sample<-rbind(p1819.chg, p1920.chg)
head(privacy.sample)

d19.ctrl$year=2019
d20.ctrl$year=2020

ctrl.sample<-rbind(d19.ctrl, d20.ctrl)

# Final sample
wearable.sample %>% 
  inner_join(privacy.sample) %>% 
  inner_join(ctrl.sample) %>% 
  arrange(pid, year)-> final.sample

str(final.sample)

head(final.sample)

final.sample %>% 
  lm(formula = "privacy.chg~wearable.chg+male+age.gr+edu.gr+factor(year)") %>% 
  summary

final.sample %>% filter(year==2020) %>% 
  lm(formula = "privacy.chg~wearable.chg+male+age.gr+edu.gr") %>% 
  summary


final.sample %>% filter(year==2020) %>% 
  mutate(Y=ifelse(privacy.chg>=1,1,0)) %>% 
  glm(formula = "Y~wearable.chg+male+age.gr+edu.gr", family="binomial") %>% 
  summary

final.sample %>% filter(year==2020) %>% 
  mutate(Y=ifelse(privacy.chg>=1,1,0)) %>% 
  select(wearable.chg) -> test
table(test$wearable.chg)
