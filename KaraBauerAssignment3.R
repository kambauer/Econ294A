##Kara Bauer
##Econ 294A
##5 February 2016

##################################################################################################
###############################Assignment 3######################################################

#Question 0
print("Kara Bauer")
print("Sid: 1269109")
print("kambauer@ucsc.edu")

 #Question 1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
class(df.ex)
#Question 2
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(year == 2013 & month == 12)

print(nrow(df.ex.2))
print("There are 13261 obs in month 12 of 2013")
df.ex.2 <- df.ex %>%
  dplyr::filter(year == 2013 & (month == 7 | month == 8 | month == 9))

print(nrow(df.ex.2))
print("There are 39657 in the summer of 2013")

#Question 3
df.ex.3a<-df.ex %>% arrange(year, month)

#Question 4
df.ex.4a<-select(df.ex, year:age)
df.ex.4b<-select(df.ex, year, month, starts_with("i"))
distinct(select(df.ex, state))

#Qestion 5
stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

dplyr::mutate

df.ex.5a<-
  df.ex %>% mutate(
    rw.stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw)
  )

df.ex.5b<-
  df.ex %>% 
  group_by(year, month)%>%
  mutate(
    rw.stndz=stndz(rw),
    rw_nrmlz=nrmlz(rw),
    count= n()
  )

#Question 6
df.ex.6<-df.ex %>% 
  group_by(year, month, state)%>%
  summarise(
    rw_min=min(rw, na.rm=T),
    rw_1stq=quantile(rw,0.25, na.rm=T),
    rw_mean=mean(rw, na.rm=T),
    rw_median=median(rw, na.rm=T),
    rw_3rdq=quantile(rw,0.75, na.rm=T),
    rw_max=max(rw, na.rm=T),
    count= n()
  )%>%
select(state,starts_with("rw_"), count)
df.ex.6max<-filter(df.ex.6, rw_mean==max(df.ex.6$rw_mean))
print(df.ex.6max)

#Question 7
df.ex.7<-df.ex %>% arrange(year, month, desc(as.character(df$state)))

