#Kara Bauer
#Econ 294A
#12 February 2016

############################################################
######################Assignment 4#########################

#Question 0

print("Name: Kara Bauer
      Sid:  1269109
      email: kambauer@ucsc.edu")

#Question 1
# load data sets
library(foreign)
flights<-read.csv(file="/Users/karabauer/Desktop/flights.csv",
                  stringsAsFactors = FALSE, header=TRUE)
planes<-read.csv(file="/Users/karabauer/Desktop/planes.csv",
                 stringsAsFactors=F, header=T)
weather<-read.csv(file="/Users/karabauer/Desktop/weather.csv",
                  stringsAsFactors=F, header=T)
airports<-read.csv(file="/Users/karabauer/Desktop/airports.csv",
                   stringsAsFactors=F, header=T)
#Question 2
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)

#Question 3
flights.2a<-subset(flights, dest=="SFO" | dest=="OAK" )
nrow(flights.2a)
flights.2b<-subset(flights, dep_delay>=60)
nrow(flights.2b)
flights.2c<-subset(flights, arr_delay>2*dep_delay)
nrow(flights.2c)

#Question 4
library(dplyr)
select(flights, dep_delay)

#Question 5
#A
arrange(flights, -dep_delay)
head(flights,n=5)
#B
flights.5b<-flights
flights$delaydiff<-(flights.5b$dep_delay-flights.5b$arr_delay)
flights.5b<-arrange(flights.5b, delaydiff)
head(abs(flights.5b$delaydiff), n=5)

#Question 6
#A
flights<-mutate(flights, speed=dist/(time/60))
flights.6a<-flights[order(desc(flights$speed)), ]
head(flights.6a, 5)

#B
flights<-mutate(flights, delta=dep_delay-arr_delay)
flights.6b<-flights[order(flights$delta),]
head(abs(flights.6b$delta),n=5)

#C
flights<-mutate(flights, lost=arr_delay-dep_delay)
flights.6c<-flights[order(flights$lost),]
head(flights.6c,n=5)

#Question 7
library(dplyr)

groupc<- group_by(flights, carrier)

flights.7a<-groupc %>%
  summarise(
    cancelledflights=sum(cancelled, na.rm=T),
    totalflights= n(),
    cancelpercent= ((cancelledflights/totalflights)*100),
    mindelta=min(delta,na.rm=T),
    maxdelta= max(delta, na.rm=T),
    meandelta= mean(delta, na.rm=T),
    quantile1=quantile(delta, 0.25, na.rm=T),
    quantile3= quantile(delta, 0.75, na.rm=T),
    quantile90=quantile(delta, 0.90, na.rm=T)
    )

flights.7b<-arrange(flights.7a, desc(cancelpercent))
summary(flights.7a)

day_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n=n()
  )

# 8
arrange(day_delay, date)
delay.8a <- day_delay %>%
  mutate(delay_today = delay,
         delay_yesterday = lag(delay, 1),
         delay_inc = delay_today - delay_yesterday)

delay.8a <- arrange(delay.8a, -delay_inc)
head(delay.8a, n=5)

# 9
dest_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(dest)%>%
  summarize(
    arr_delay = mean(arr_delay),
    n = n()
  )
library(dplyr)
airports <- airports %>%
  rename(dest=iata, name=airport)


df.9a <- left_join(dest_delay, airports, by=c("dest"="dest"))
df.9a <- arrange(df.9a, -arr_delay)
head(df.9a, n=5)

df.9b <- inner_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9a)
nrow(df.9b)
print("The number of observations are close, but are off by 2.")

df.9c <- right_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9c)
print("There are NAs in in the arr_delay column because aiports have more observations")

df.9d <- full_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9d)
print("There are NAs again because airports has a different number of observations")

# 10
hourlydelay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarise(
    delay = mean(dep_delay)
  )

hourlydelay$date <- as.Date(hourlydelay$date)
df.10a <- left_join(hourlydelay, weather, by=c("date"="date"))

arrange(df.10a, delay)
head(df.10a$conditions,n=20)

install.packages(data.table)
library(data.table)

weather.delays=data.table(df.10a$delay,df.10a$conditions)

head(weather_delays, n=18)

