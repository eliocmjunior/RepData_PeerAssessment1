---
title: "Reproducible Research - Assignment Week 2"
author: "Elio Moreira"
date: "08/May/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
```

## Getting data
```{r}
data <- read.csv("activity.csv")
```

```{r echo=FALSE}
library(dplyr)
library(ggplot2)
```

Peek through
```{r}
str(data)
```
## Transform
Aggregating steps by day and tidying dataframe
```{r}
daily <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=T)
colnames(daily) <- c("Day", "Steps")
daily$Day <- as.Date(daily$Day)
```
## Processing and plotting

**1. Histogram of the total number of steps taken each day**
```{r}
hist(daily$Steps, main = "Histogram of daily Steps", xlab = "Steps")
```

**2. Mean and median number of steps taken each day**
```{r}
avg <- mean(daily$Steps, na.rm = T)
med <- median(daily$Steps, na.rm = T)
```
In average (mean) `r as.integer(avg)` steps has taken. The median of steps taken per day is `r as.integer(med)`. 

**3. Time series plot of the average number of steps taken**
```{r}
intsteps <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=T)
colnames(intsteps) <- c("Interval", "Steps")
plot(y=intsteps$Steps,
     x=intsteps$Interval,
     type="l", 
     main = "Steps taken in Average by Interval", 
     xlab = "Interval", 
     ylab = "Steps")

```

**4. The 5-minute interval that, on average, contains the maximum number of steps**
```{r}
intsteps[which.max(intsteps$Steps),1]

```
**5. Code to describe and show a strategy for imputing missing data**

Missing data in the source (Na) switched by mean of interval

```{r}
missing <- data[is.na(data$steps),]
colnames(missing) <- c("Steps","Date","Interval")
tbl <- merge(missing,intsteps, by="Interval")
fillmissing <- select(tbl,c(steps = Steps.y, date = Date, interval = Interval))
complete <- rbind(subset(data,!is.na(data$steps)),fillmissing)
```

**6. Histogram of the total number of steps taken each day after missing values are imputed**
```{r}
dailycomp <- aggregate(complete$steps, by=list(complete$date), FUN=sum, na.rm=T)
colnames(dailycomp) <- c("Day", "Steps")
dailycomp$Day <- as.Date(dailycomp$Day)
hist(dailycomp$Steps, main = "Histogram of daily Steps", xlab = "Steps")
```
**7. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**
```{r}
complete$wd <- ifelse(weekdays(as.Date(complete$date))=="Saturday"|weekdays(as.Date(complete$date))=="Sunday",yes = "weekend", no = "weekday")
head(complete)
compweekday <- complete %>% group_by(wd,interval) %>% summarise(steps = mean(steps))
p <- ggplot(data = compweekday, 
                   aes(x = compweekday$interval, 
                       y = compweekday$steps, 
                       col = compweekday$wd) ) + geom_line()
p + labs(title = "Average Steps Weekdays x Weekends", x = "Interval", y = "Steps",colour = "")
```




