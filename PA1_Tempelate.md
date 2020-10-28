---
title: 'Reproducible Research: Peer Assessment 1'
author: "kaivalya"
date: "10/28/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Reproducible Research: Peer Assessment 1

## Loading & Pre proccessing the data

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)

## Mean total number of steps taken per day

##33 1. Calculate the number of steps taken each day
library(magrittr)
library(dplyr)

## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union

databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

#### 2. Calculate and report the mean and median of the total number of steps taken per day
mean(databydate$tsteps)
median(databydate$tsteps)

## Average daily pattern
#### 1. Time series plot for average 5 minute interval
library(ggplot2)
databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

#### 2.The 5-minute interval that, on average, contains the maximum number of steps

databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

## Missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# generate listing of NA's
missingVals <- sum(is.na(data))
missingVals

#### 2 & 3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
# Summary of new data : mean & median

summary(FullSummedDataByDay)
# Making Histogram

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
#### 4C Compare the mean and median of Old and New data

oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
# Old mean and New mean
oldmean

newmean

oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
# Old median and New median
oldmedian

newmedian

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")

