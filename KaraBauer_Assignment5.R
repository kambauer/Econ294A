#Kara Bauer
#Econ 294A
#26 February 2016

#################################################################################################
#####################################Assignment 5################################################

install.packages("ggplot2")
library(ggplot2)
library(scales)
library(dplyr)

#Question 1
#Part a#

q1a<-ggplot(
  diamonds, aes(x=x*y*z, y=price))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(aes(color = clarity), alpha = 0.2) + 
  aes(size = carat) + 
  scale_size(range=c(5,10)) 
q1a

#Part b#
q1b<- ggplot(
  diamonds, aes(x=carat, y=..density.., fill=clarity))+
  geom_histogram(binwidth = 0.2) + 
  facet_grid(cut ~ .) 
q1b

#Part c#
q1c<- ggplot(
  data = diamonds, aes(y = price, 
  x = cut)) + 
  geom_violin() + 
  geom_jitter(alpha = 0.01) 
q1c

#Question 2#

#Part a#
library(foreign)
orgdata<-read.dta(file="/Users/karabauer/Desktop/org_example.dta")
orgdata2<-subset(orgdata, !is.na(rw))
bymonth<-orgdata2 %>% 
  group_by(year,month) %>% 
  mutate(
    educ=educ,
    median.rm=median(rw),
    quart1=quantile(rw, probs = 0.25, na.rm = T, names = TRUE, type = 7),
    quart3=quantile(rw, probs = 0.75 , na.rm = T, names = TRUE, type = 7), 
    dec1=quantile(rw, probs = 0.1, na.rm = T, names = TRUE, type = 5) ,
    dec9=quantile(rw, probs = 0.9, na.rm = T, names = TRUE, type = 5),
    total = n()
  ) %>% select(year, month, median.rm, quart1, quart3, dec1, dec9, educ) %>%
  arrange(year,month)

bymonth<-bymonth %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )
q2a<-ggplot(
  data=bymonth,
  aes(date)) + 
  geom_line(aes(y = median.rm)) + 
  geom_ribbon(aes(ymin = quart1, ymax = quart3), alpha=0.4) + 
  geom_ribbon(aes(ymin = dec1, ymax = dec9), alpha=0.2) + 
  ylim(0, 50) 
q2a


#Part b#

ymonth2<-subset(orgdata2, !is.na(educ))
bymonth2<-orgdata2 %>% 
  group_by(year,month,educ) %>% 
  mutate(
    median.rm=median(rw),
    total = n()
  ) %>% select(year, month, median.rm, educ) %>%
  arrange(year,month)

bymonth2<-bymonth2 %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )


q2b<-ggplot(
  data=bymonth2,
  aes(x=date,y=median.rm)) +  
  geom_line(aes(color=educ)) 
q2b
