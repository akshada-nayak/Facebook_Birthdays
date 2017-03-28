# Facebook_Birthdays
Analysis of friends' birthdays
title: "Facebook Birthdays"
author: "akshada"
date: "January 24, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Friends' Birthday analysis

## Step 1: Import calendar from FB to Outlook
## Step 2: Export the calendar from outlook in ICS format and then convert to CSV format
## Step 3: Import the csv in R
```{r}

getwd()
fb<-read.csv("birthdays.csv")
library(date)
library(tidyr)
library(dplyr)
library(ggplot2)

head(fb)
?head

```
## Step 4: Extract only names of friends and their birthdays from the dataset
```{r}
names<-filter(fb,grepl('SUMMARY',BEGIN.VCALENDAR))
start<-filter(fb,grepl('DTSTART',BEGIN.VCALENDAR))
final<-data.frame(names,start)
colnames(final)<-c("Names","birthdays")
head(final)

```
## Step 5: Clean the extracted data
```{r}
final$birthdays<-gsub("DTSTART;VALUE=DATE:","",final$birthdays)
final$Names<-gsub("SUMMARY:","",final$Names)
final$Names = substr(final$Names,1,nchar(final$Names)-11)
head(final)


```
## Step 5: Extract months and day
```{r}
library(lubridate)
final$bmonth<-month(as.POSIXlt(final$birthdays, format="%Y%m%d"))

final$bday<-day(as.POSIXlt(final$birthdays, format="%Y%m%d"))
head(final)
```

## Step 6: Data is FINALly ready to be analysed
## Step 7: Summary of overall data
```{r}

by(final$bday,final$bmonth,summary)
by(final$bday,final$bmonth,max)
by(final$bday,final$bmonth,min)

```
## Every 1st of the month someone has a birthday except in Feb and Oct

## Step 8: People with whom I share my bday
```{r}
mybday<-subset(final,bmonth==01 & bday==01)$Names
mybday

```
##Step 9: #day with maximum bdays
```{r}
names(which(table(final$bday)==max(table(final$bday))))

```
## Step 10: #date with maximum bdays
```{r}
library(plyr)
SUB<-subset(final,select=c("bmonth","bday"))
SUB$count<-1
census=aggregate(count~ .,SUB, FUN = sum)#means all variables except count
subset(census,count==max(count))

qplot(data=final,x=bday, binwidth=1, color=I("black"), 
      fill=I("blue"), xlab="days", ylab="frequency")+
      scale_x_continuous(breaks=seq(1,31,1))+
      scale_y_continuous(breaks=seq(0,40,1))

```
## Step 11: month with most number of bdays
```{r}
mon<-subset(final,select="bmonth")
mon$count<-1
census2=aggregate(count~ .,mon, FUN=sum)
subset(census2,count==max(count))


qplot(data=final,x=bmonth, binwidth=1, color=I("black"), 
      xlab="month", ylab="frequency", geom='freqpoly')+
      scale_x_continuous(breaks=seq(1,12,1))

```
