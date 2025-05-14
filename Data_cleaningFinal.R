##### Data cleaning ####
##### Gamachis and Gezahegn

### set your working directory
getwd()
setwd("D:/SESA_training")


## download necessary packages

install.packages("tidyverse",dependencies = T)
install.packages("readxl")


##load packages

library("tidyverse")
library("readxl")
library("foreign")
library("haven")



### data import ...

dat=read_excel("Ind.xlsx")

##  understand how data are stored

str(dat) # data structure 
glimpse(dat) # data structure 

names(dat) 
head(dat)
tail(dat)

# excel spreadsheet form

View(dat) 

dim(dat)

## labeling variables 
label(dat$lit2)="litracy of respondent"

## data cleaning

## duplication

sum(duplicated(dat$id))
dat[duplicated(dat$id),]

dat %>% 
  group_by(id) %>%
               mutate(n=n()) %>% 
                      arrange(id) %>% 
                                  filter(n>1)


### missing data

sum(is.na(dat$gender))


table(dat$litracy)
table(dat$litracy,useNA = "ifany")
dat[is.na(dat$litracy),c("id","litracy")]

dat=dat %>% mutate(enddate_num=ymd(enddate),dob=ymd(dob),year_dob=year(dob), month_dob=month(dob,label=T,abbr = FALSE), age=as.numeric(curr_date-dob)/365.25)

cdat$curr_date=as.Date(dat$curr_date)
dat %>% 
  mutate(all_date=case_when(is.na(enddate_num)~ymd("2025-5-13"),
                                  TRUE~enddate_num)) %>%
  select(all_date,enddate_num) %>% 
  sample_n(100)

names(dat)
view(head(dat))
glimpse(dat)

dat[is.na(dat$litracy),c("id","litracy","age")]




### inconsistency 

dat %>% count(litracy)
dat$lit2=factor(dat$litracy,levels=c(1,2,3,4),labels = c("litrate","read and write","read only","neither read or write"))

dat %>% count(litracy,lit2)

table(dat$grad_comp,useNA = "ifany")
## inconsistency between litracy and grade completed
dat %>% 
  filter(as.numeric(lit2)==4 & grad_comp>0) %>% 
      select(id,lit2,grad_comp)


## coding error

dat %>% count(gender)


dat =dat%>% mutate(sex=case_when(gender=="Fmale"~"F",
                            gender=="female"~"F",
                            TRUE~gender))


table(dat$sex)

dat$sex2=factor(dat$sex,labels = c("Female","Male"))

table(dat$sex2,dat$sex)



##date variable

library(lubridate)


## recode continous variables 

dat %>% mutate(age_cat=case_when(age<20~"adlocent",
                                 between(age,20,24.999)~"young",
               age>25~"old")

) %>% count(age_cat)


### detecting outlair
summary(dat$age)



dat %>% 
   mutate(age_atentry=as.numeric(ymd(startdate)-dob)/365.25)  %>% 
     select(id,dob,startdate,age_atentry) %>% filter(age_atentry<0)

dat %>% 
  mutate(age_atentry=as.numeric(ymd(startdate)-dob)/365.25)  %>% 
  select(age_atentry) %>% summary(age_atentry)
dat %>% 
  mutate(age_atentry=as.numeric(ymd(startdate)-dob)/365.25)  %>% 
  select(age_atentry) %>% ggplot(aes(y=age_atentry))+geom_boxplot()














