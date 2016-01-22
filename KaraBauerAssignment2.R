#Kara Bauer
#Econ 294A
#January 21 2016

######################################################################################
############################Assignment 2#############################################

#Question 0
KaraBauerAssignment2<- list(
  firstName = "Kara",
  lastName  = "Bauer",
  email     = "kambauer@ucsc.edu",
  studentID = 1269109
)

#Question 1
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/3104186b0c5a0bf7f156bf86eecbe018cc5a578c/data/diamonds.CSV")
diamonds <- read.csv(text = x)

KaraBauerAssignment2$s1a<-nrow(diamonds)
KaraBauerAssignment2$s1b<-ncol(diamonds)
KaraBauerAssignment2$s1c<-names(diamonds)
KaraBauerAssignment2$s1d<-summary(diamonds$prices)

#Quesiton 2
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt")
td <- read.table(text = x, header=T)


#Quesiton 2
KaraBauerAssignment2$s2a<- nrow(td)
KaraBauerAssignment2$s2b<- ncol(td)
KaraBauerAssignment2$s2c<- names(td)
KaraBauerAssignment2$s2d<- mean(td$weight)
KaraBauerAssignment2$s2e<- median(td$weight)
histtd<- hist(td$weight)
 td$weight<- ifelse(test= td$weight>= 996 & td$weight<=999,
                yes= NA,
                no=td$weight)
KaraBauerAssignment2$s2f<-mean(td$weight, na.rm=T)
KaraBauerAssignment2$s2g<-median(td$weight, na.rm=T)

summarym<- subset(td, weight<996 & SEX==1)
summaryw<- subset(td, wweight<996 & SEX==2)

KaraBauerAssignment2$s2h<- summary(summaryw$weight, na.rm=T)
KaraBauerAssignment2$s2i<- summary(summarym$weight, na.rm=T)

#Question 3
V<-c(letters, LETTERS)
as.factor(V)
KaraBauerAssignment2$s3a<- even<-V[seq(2, length(V),2)]
KaraBauerAssignment2$s3b<- name<-paste(V[c(37,1,18)], collapse="")
arr<-array(c(letters, LETTERS), dim=c(3,3,3))
print(arr)
KaraBauerAssignment2$s3c<- arr[,1,2]
print(KaraBauerAssignment2$s3c)
KaraBauerAssignment2$s3d<- arr[2,2,]
print(KaraBauerAssignment2$s3d)
KaraBauerAssignment2$s3e<- paste(arr[2,1,2], arr[1,1,1], arr[3,3,2])
print(KaraBauerAssignment2$s3e)

