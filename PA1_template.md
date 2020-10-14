---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

```r
data <- read.csv('activity.csv')
print(head(data))
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

```r
pasos <- data %>% group_by(date) %>% summarise(pasos_diarios = sum(steps, na.rm = TRUE))
promedio = mean(pasos$pasos_diarios, na.rm=TRUE)
print(promedio)
```

```
## [1] 9354.23
```

```r
ggplot(pasos, aes(x = pasos_diarios)) + geom_histogram(fill = "blue", binwidth = 2000) +
    labs(title="Histograma de pasos por día",
         x = "Pasos por día",
         y = "Total")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
promedio <- mean(pasos$pasos_diarios, na.rm=TRUE)
mediana <- median(pasos$pasos_diarios, na.rm=TRUE)
print(promedio)
```

```
## [1] 9354.23
```

```r
print(mediana)
```

```
## [1] 10395
```


## What is the average daily data pattern?

```r
actividad_diaria <- data %>% group_by(interval) %>% summarise(pasos_diarios = mean(steps, na.rm = TRUE))
print(head(actividad_diaria))
```

```
## # A tibble: 6 x 2
##   interval pasos_diarios
##      <int>         <dbl>
## 1        0        1.72  
## 2        5        0.340 
## 3       10        0.132 
## 4       15        0.151 
## 5       20        0.0755
## 6       25        2.09
```

```r
ggplot(actividad_diaria, aes(x=interval, y=pasos_diarios)) + 
        geom_line(color="blue", size=2) +
        labs(title="Actividad diaria promedio", x="Intervalo", y="Pasos")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maximo <- actividad_diaria[which.max(actividad_diaria$pasos_diarios),]
print(maximo)
```

```
## # A tibble: 1 x 2
##   interval pasos_diarios
##      <int>         <dbl>
## 1      835          206.
```

## Imputing missing values

```r
pasos_imputados <- actividad_diaria$pasos_diarios[match(data$interval, actividad_diaria$interval)]
data_imputada <- transform(data, steps = ifelse(is.na(data$steps), yes = pasos_imputados, no = data$steps))
total_imputados <- aggregate(steps ~ date, data_imputada, sum)
names(total_imputados) <- c("fecha", "pasos")
ggplot(total_imputados, aes(x = pasos)) + geom_histogram(fill = "blue", binwidth = 2000) +
    labs(title="Histograma de pasos por día",
         x = "Pasos por día",
         y = "Total")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
promedio <- mean(total_imputados$pasos)
print(promedio)
```

```
## [1] 10766.19
```

```r
mediana <- median(total_imputados$pasos)
print(mediana)
```

```
## [1] 10766.19
```

## Are there differences in data patterns between weekdays and weekends?

```r
data$date <- as.Date(strptime(data$date, format="%Y-%m-%d"))
data$datetype <- sapply(data$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
actividad_dia <- aggregate(steps~interval + datetype, data, mean, na.rm = TRUE)
ggplot(actividad_dia, aes(x=interval, y=steps, color = datetype)) + 
        geom_line(color="blue", size=2) +
        labs(title="Actividad diaria promedio", x="Intervalo", y="Pasos") +
    facet_wrap(~datetype, nrow=1, ncol = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
