#Kara Bauer
#Econ 294A
#March 15th 2016


############################Final Assingment###############################

#query data
library(dplyr)
library(DBI)
library(RSQLite)
library("nycflights13")
library(ggplot2)
library(knitr)
my_db<- src_sqlite("nycflights13", create=T)

flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))

airlines_sqlite<-copy_to(my_db, airlines, temporary=FALSE,
    indexes=list("carrier")
)

airports_sqlite <- copy_to(
  my_db, airports, temporary = FALSE, 
  indexes = list("faa")
)

planes_sqlite <- copy_to(
  my_db, planes, temporary = FALSE, 
  indexes = list("tailnum")
)

weather_sqlite <- copy_to(
  my_db, weather, temporary = FALSE, 
  indexes = list(
    c("year", "month","day","hour"),
    "origin")
)


flights_sqlite <- tbl(my_db, "flights")
weather_sqlite <- tbl(my_db, "weather")
airports_sqlite <- tbl(my_db, "airports")
planes_sqlite <- tbl(my_db, "planes")
airlines_sqlite <-tbl(my_db, "airlines")

flights=flights
weather=weather
airports=airports
planes=planes
airlines=airlines


 flights_weather<-inner_join(
  tbl(my_db,"flights"),
  tbl(my_db,"weather"),
  by = c("year","month","day","hour"))%>%
  collect()%>%
  mutate(canceled=(is.na(arr_time)),
         delayed= ifelse(dep_delay>0,1,0))     

flights_weather$month<-as.factor(flights_weather$month)


flights_planes<-inner_join(
  tbl(my_db,"flights"),
  tbl(my_db,"planes"),
  by = c("tailnum"))%>%
  collect()%>%
  mutate(canceled=(is.na(arr_time)),
         delayed= ifelse(dep_delay>0,1,0)) 


#A.)
probit_w<-glm(canceled~ temp+wind_speed+precip+pressure+visib, 
              family=binomial(link="probit"), data=flights_weather)
summary(probit_w)
##plot temp pressure visib
weather2<-subset(flights_weather, canceled==1)
#plot distribution of pressure of cancelations
qplot(pressure, data=weather2, geom="density", fill=pressure, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Pressure",
      ylab="Density")

qplot(temp, data=weather2, geom="density", fill=temp, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Temperature",
      ylab="Density")
qplot(visib, data=weather2, geom="density", fill=visib, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Visibility",
      ylab="Density")
can<-ggplot(
  data=weather2,
  aes(x=temp,y=pressure)) +  
  geom_line(aes(color=visib)) 
can

########################################################################

probit_w_delayed<-glm(delayed~ temp+wind_speed+precip+pressure+visib, 
              family=binomial(link="probit"), data=flights_weather)
summary(probit_w_delayed)

weather3<-subset(flights_weather, delayed==1)

qplot(visib, data=weather3, geom="density", fill=visib, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Visibility",
      ylab="Density")
qplot(temp, data=weather3, geom="density", fill=temp, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Temperature",
      ylab="Density")
qplot(pressure, data=weather3, geom="density", fill=pressure, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Pressure",
      ylab="Density")

del<-ggplot(
  data=weather3,
  aes(x=temp,y=pressure)) +  
  geom_line(aes(color=visib)) 
del

#all sig


#B.)
probit_time<-glm(canceled~ day+month+hour, family=binomial(link="probit"), 
                data=flights_weather)
summary(probit_time)
#plot hour and by month

qplot(hour, data=weather2, geom="density", fill=hour, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Hour",
      ylab="Density")
qplot(month, data=weather2, geom="density", fill=month, alpha=I(.5),
      main="Distribution of Cancelations", xlab="month",
      ylab="Density")


probit_time_delay<-glm(delayed~ day+month+hour, family=binomial(link="probit"), 
                 data=flights_weather)
summary(probit_time_delay)

#all sig
qplot(hour, data=weather3, geom="density", fill=hour, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Hour",
      ylab="Density")
qplot(month, data=weather3, geom="density", fill=month, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="month",
      ylab="Density")


#C.)
probit_desit<-glm(canceled~dest+distance+air_time,
                  family=binomial(link="probit"), data=flights_weather)
by_desit<-group_by(weather2, dest)




#D.)
probit_plane<-glm(canceled~type+manufacturer+engines+seats+engine,
                  family=binomial(link="probit"), data=flights_planes)
summary(probit_plane)
planes2<-subset(flights_planes, canceled==1)
qplot(canceled, data=planes2, geom="density", fill=engine, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Canceled Flights",
      ylab="Density")
qplot(canceled, data=planes2, geom="density", fill=manufacturer, alpha=I(.5),
      main="Distribution of Cancelations", xlab="Canceled Flights",
      ylab="Density")


probit_plane<-glm(delayed~type+manufacturer+engines+seats+engine,
                  family=binomial(link="probit"), data=flights_planes)
summary(probit_plane)

qplot(delayed, data=planes2, geom="density", fill=engine, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Canceled Flights",
      ylab="Density")
qplot(delayed, data=planes2, geom="density", fill=manufacturer, alpha=I(.5),
      main="Distribution of Departure Delays", xlab="Canceled Flights",
      ylab="Density")

