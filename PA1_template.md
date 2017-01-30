# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
We are going to assume that the dataset is in the working folder. Step 1 will be to load the data into R and see a quick preview. 


```r
activitydata <- read.csv('activity.csv')
head(activitydata)
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

Lets take a look at the structure of the dataset.

```r
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We see that date is stored as a factor. Lets convert it to the correct format.

```r
activitydata$date <- as.Date(activitydata$date)
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The date now is correctly formatted. Let's proceed to answer the questions.


## What is mean total number of steps taken per day?
**Part 1**

The goal is to make a histogram of the total steps taken per day. Using the dplyr package we can group and sum the activity data by date. Then we can plot a histogram with that data. 

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

```r
activitydata_date <- group_by(activitydata,date)
stepsperday<-summarise(activitydata_date,total=sum(steps))
stepsperday
```

```
## # A tibble: 61 × 2
##          date total
##        <date> <int>
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```

Now we plot the histogram.

```r
hist(stepsperday$total,main="Histogram of steps per day",xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

**Part 2**

The mean and median steps per day can be easily calculated . Ignore NA to get the correct responses. The mean is :

```r
mean(stepsperday$total,na.rm = TRUE)
```

```
## [1] 10766.19
```

The median is:

```r
median(stepsperday$total,na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

**Part 1**

We need a time series depicting average activity (y axis) by the time interval (x axis). To do this we can group the data by time interval and summaraise it by mean. 


```r
library(dplyr)
activitydata_interval <- group_by(activitydata,interval)
stepsperinterval<-summarise(activitydata_interval,average=mean(steps,na.rm = TRUE))
stepsperinterval
```

```
## # A tibble: 288 × 2
##    interval   average
##       <int>     <dbl>
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## # ... with 278 more rows
```

Now we can plot the time series using ggplot. 


```r
library(ggplot2)
g<-ggplot(stepsperinterval,aes(interval,average))
g + geom_point() + geom_line(group='1')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

**Part 2**

Based on this we observe that the interval with the maximum average steps is somewhere in the 800's . Lets find out the exact interval using which.max.


```r
maxinterval <- stepsperinterval[which.max(stepsperinterval$average),]
maxinterval
```

```
## # A tibble: 1 × 2
##   interval  average
##      <int>    <dbl>
## 1      835 206.1698
```

So we can see that it is 835 



## Imputing missing values
**Part 1**

We need to find the total number of missing values in the dataset. This can be seen using the summary function. 


```r
summary(activitydata)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

We can see that there are 2304 NA's in the steps column. 

**Part 2**

The strategy we use to replace these values is to calculate the mean for that interval and use it to replace the missing value. 

The idea behind this is that on an average a person is expected to maintain a similar activity level throughout the day. This will let us replicate that. 

We first right a function to replace an NA value with a mean value.


```r
impute.mean<-function(x) replace(x,is.na(x),mean(x,na.rm=TRUE))
```

**Part 3**

We can now replicate the original dataset and use dplyr to apply this function to all the records


```r
activitydata_imputed <- activitydata %>% group_by(interval) %>% mutate(steps=impute.mean(steps))
head(activitydata_imputed)
```

```
## Source: local data frame [6 x 3]
## Groups: interval [6]
## 
##       steps       date interval
##       <dbl>     <date>    <int>
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

As we can observe, the previously missing values have been replaced by the interval averages. 

**Part 4**

We can now recreate the histogram for the new dataset.

```r
activitydata_imputed_date <-group_by(activitydata_imputed,date)
stepsperday_imputed<-summarise(activitydata_imputed_date,total=sum(steps))
hist(stepsperday_imputed$total,main="Histogram of steps per day",xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Notice that compared to the original histogram the frequencies are much higher in the new one. 

Let's see the mean and median for the imputed dataset. Notice that na.rm is not needed this time as there are no NA values in the dataset.

The mean is:

```r
mean(stepsperday_imputed$total)
```

```
## [1] 10766.19
```

The median is:

```r
median(stepsperday_imputed$total)
```

```
## [1] 10766.19
```

The mean remains the same but the median is now equal to the mean in the imputed dataset.


## Are there differences in activity patterns between weekdays and weekends?
We can use a convenient function called is.weekend in the chron package along with the mutate function to add a weekday,weekend column. 


```r
library(chron)
activitydata_imputed<- activitydata_imputed %>% mutate(daytype=ifelse(is.weekend(date),'weekend','weekday'))
```

We then convert the daytype column to a factor and verify the results.


```r
activitydata_imputed$daytype<-as.factor(activitydata_imputed$daytype)
str(activitydata_imputed)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol interval
##  - attr(*, "labels")='data.frame':	288 obs. of  1 variable:
##   ..$ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##   ..- attr(*, "vars")=List of 1
##   .. ..$ : symbol interval
##   ..- attr(*, "drop")= logi TRUE
##  - attr(*, "indices")=List of 288
##   ..$ : int  0 288 576 864 1152 1440 1728 2016 2304 2592 ...
##   ..$ : int  1 289 577 865 1153 1441 1729 2017 2305 2593 ...
##   ..$ : int  2 290 578 866 1154 1442 1730 2018 2306 2594 ...
##   ..$ : int  3 291 579 867 1155 1443 1731 2019 2307 2595 ...
##   ..$ : int  4 292 580 868 1156 1444 1732 2020 2308 2596 ...
##   ..$ : int  5 293 581 869 1157 1445 1733 2021 2309 2597 ...
##   ..$ : int  6 294 582 870 1158 1446 1734 2022 2310 2598 ...
##   ..$ : int  7 295 583 871 1159 1447 1735 2023 2311 2599 ...
##   ..$ : int  8 296 584 872 1160 1448 1736 2024 2312 2600 ...
##   ..$ : int  9 297 585 873 1161 1449 1737 2025 2313 2601 ...
##   ..$ : int  10 298 586 874 1162 1450 1738 2026 2314 2602 ...
##   ..$ : int  11 299 587 875 1163 1451 1739 2027 2315 2603 ...
##   ..$ : int  12 300 588 876 1164 1452 1740 2028 2316 2604 ...
##   ..$ : int  13 301 589 877 1165 1453 1741 2029 2317 2605 ...
##   ..$ : int  14 302 590 878 1166 1454 1742 2030 2318 2606 ...
##   ..$ : int  15 303 591 879 1167 1455 1743 2031 2319 2607 ...
##   ..$ : int  16 304 592 880 1168 1456 1744 2032 2320 2608 ...
##   ..$ : int  17 305 593 881 1169 1457 1745 2033 2321 2609 ...
##   ..$ : int  18 306 594 882 1170 1458 1746 2034 2322 2610 ...
##   ..$ : int  19 307 595 883 1171 1459 1747 2035 2323 2611 ...
##   ..$ : int  20 308 596 884 1172 1460 1748 2036 2324 2612 ...
##   ..$ : int  21 309 597 885 1173 1461 1749 2037 2325 2613 ...
##   ..$ : int  22 310 598 886 1174 1462 1750 2038 2326 2614 ...
##   ..$ : int  23 311 599 887 1175 1463 1751 2039 2327 2615 ...
##   ..$ : int  24 312 600 888 1176 1464 1752 2040 2328 2616 ...
##   ..$ : int  25 313 601 889 1177 1465 1753 2041 2329 2617 ...
##   ..$ : int  26 314 602 890 1178 1466 1754 2042 2330 2618 ...
##   ..$ : int  27 315 603 891 1179 1467 1755 2043 2331 2619 ...
##   ..$ : int  28 316 604 892 1180 1468 1756 2044 2332 2620 ...
##   ..$ : int  29 317 605 893 1181 1469 1757 2045 2333 2621 ...
##   ..$ : int  30 318 606 894 1182 1470 1758 2046 2334 2622 ...
##   ..$ : int  31 319 607 895 1183 1471 1759 2047 2335 2623 ...
##   ..$ : int  32 320 608 896 1184 1472 1760 2048 2336 2624 ...
##   ..$ : int  33 321 609 897 1185 1473 1761 2049 2337 2625 ...
##   ..$ : int  34 322 610 898 1186 1474 1762 2050 2338 2626 ...
##   ..$ : int  35 323 611 899 1187 1475 1763 2051 2339 2627 ...
##   ..$ : int  36 324 612 900 1188 1476 1764 2052 2340 2628 ...
##   ..$ : int  37 325 613 901 1189 1477 1765 2053 2341 2629 ...
##   ..$ : int  38 326 614 902 1190 1478 1766 2054 2342 2630 ...
##   ..$ : int  39 327 615 903 1191 1479 1767 2055 2343 2631 ...
##   ..$ : int  40 328 616 904 1192 1480 1768 2056 2344 2632 ...
##   ..$ : int  41 329 617 905 1193 1481 1769 2057 2345 2633 ...
##   ..$ : int  42 330 618 906 1194 1482 1770 2058 2346 2634 ...
##   ..$ : int  43 331 619 907 1195 1483 1771 2059 2347 2635 ...
##   ..$ : int  44 332 620 908 1196 1484 1772 2060 2348 2636 ...
##   ..$ : int  45 333 621 909 1197 1485 1773 2061 2349 2637 ...
##   ..$ : int  46 334 622 910 1198 1486 1774 2062 2350 2638 ...
##   ..$ : int  47 335 623 911 1199 1487 1775 2063 2351 2639 ...
##   ..$ : int  48 336 624 912 1200 1488 1776 2064 2352 2640 ...
##   ..$ : int  49 337 625 913 1201 1489 1777 2065 2353 2641 ...
##   ..$ : int  50 338 626 914 1202 1490 1778 2066 2354 2642 ...
##   ..$ : int  51 339 627 915 1203 1491 1779 2067 2355 2643 ...
##   ..$ : int  52 340 628 916 1204 1492 1780 2068 2356 2644 ...
##   ..$ : int  53 341 629 917 1205 1493 1781 2069 2357 2645 ...
##   ..$ : int  54 342 630 918 1206 1494 1782 2070 2358 2646 ...
##   ..$ : int  55 343 631 919 1207 1495 1783 2071 2359 2647 ...
##   ..$ : int  56 344 632 920 1208 1496 1784 2072 2360 2648 ...
##   ..$ : int  57 345 633 921 1209 1497 1785 2073 2361 2649 ...
##   ..$ : int  58 346 634 922 1210 1498 1786 2074 2362 2650 ...
##   ..$ : int  59 347 635 923 1211 1499 1787 2075 2363 2651 ...
##   ..$ : int  60 348 636 924 1212 1500 1788 2076 2364 2652 ...
##   ..$ : int  61 349 637 925 1213 1501 1789 2077 2365 2653 ...
##   ..$ : int  62 350 638 926 1214 1502 1790 2078 2366 2654 ...
##   ..$ : int  63 351 639 927 1215 1503 1791 2079 2367 2655 ...
##   ..$ : int  64 352 640 928 1216 1504 1792 2080 2368 2656 ...
##   ..$ : int  65 353 641 929 1217 1505 1793 2081 2369 2657 ...
##   ..$ : int  66 354 642 930 1218 1506 1794 2082 2370 2658 ...
##   ..$ : int  67 355 643 931 1219 1507 1795 2083 2371 2659 ...
##   ..$ : int  68 356 644 932 1220 1508 1796 2084 2372 2660 ...
##   ..$ : int  69 357 645 933 1221 1509 1797 2085 2373 2661 ...
##   ..$ : int  70 358 646 934 1222 1510 1798 2086 2374 2662 ...
##   ..$ : int  71 359 647 935 1223 1511 1799 2087 2375 2663 ...
##   ..$ : int  72 360 648 936 1224 1512 1800 2088 2376 2664 ...
##   ..$ : int  73 361 649 937 1225 1513 1801 2089 2377 2665 ...
##   ..$ : int  74 362 650 938 1226 1514 1802 2090 2378 2666 ...
##   ..$ : int  75 363 651 939 1227 1515 1803 2091 2379 2667 ...
##   ..$ : int  76 364 652 940 1228 1516 1804 2092 2380 2668 ...
##   ..$ : int  77 365 653 941 1229 1517 1805 2093 2381 2669 ...
##   ..$ : int  78 366 654 942 1230 1518 1806 2094 2382 2670 ...
##   ..$ : int  79 367 655 943 1231 1519 1807 2095 2383 2671 ...
##   ..$ : int  80 368 656 944 1232 1520 1808 2096 2384 2672 ...
##   ..$ : int  81 369 657 945 1233 1521 1809 2097 2385 2673 ...
##   ..$ : int  82 370 658 946 1234 1522 1810 2098 2386 2674 ...
##   ..$ : int  83 371 659 947 1235 1523 1811 2099 2387 2675 ...
##   ..$ : int  84 372 660 948 1236 1524 1812 2100 2388 2676 ...
##   ..$ : int  85 373 661 949 1237 1525 1813 2101 2389 2677 ...
##   ..$ : int  86 374 662 950 1238 1526 1814 2102 2390 2678 ...
##   ..$ : int  87 375 663 951 1239 1527 1815 2103 2391 2679 ...
##   ..$ : int  88 376 664 952 1240 1528 1816 2104 2392 2680 ...
##   ..$ : int  89 377 665 953 1241 1529 1817 2105 2393 2681 ...
##   ..$ : int  90 378 666 954 1242 1530 1818 2106 2394 2682 ...
##   ..$ : int  91 379 667 955 1243 1531 1819 2107 2395 2683 ...
##   ..$ : int  92 380 668 956 1244 1532 1820 2108 2396 2684 ...
##   ..$ : int  93 381 669 957 1245 1533 1821 2109 2397 2685 ...
##   ..$ : int  94 382 670 958 1246 1534 1822 2110 2398 2686 ...
##   ..$ : int  95 383 671 959 1247 1535 1823 2111 2399 2687 ...
##   ..$ : int  96 384 672 960 1248 1536 1824 2112 2400 2688 ...
##   ..$ : int  97 385 673 961 1249 1537 1825 2113 2401 2689 ...
##   ..$ : int  98 386 674 962 1250 1538 1826 2114 2402 2690 ...
##   .. [list output truncated]
##  - attr(*, "drop")= logi TRUE
##  - attr(*, "group_sizes")= int  61 61 61 61 61 61 61 61 61 61 ...
##  - attr(*, "biggest_group_size")= int 61
```

```r
head(activitydata_imputed)
```

```
## Source: local data frame [6 x 4]
## Groups: interval [6]
## 
##       steps       date interval daytype
##       <dbl>     <date>    <int>  <fctr>
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

**Part 2**

We can now create a panel time series plot to see differences in activity across weekdays and weekends. 


```r
activitydata_imputed_interval <- group_by(activitydata_imputed,daytype,interval)
stepsperinterval_imputed<-summarise(activitydata_imputed_interval,average=mean(steps,na.rm = TRUE))
g<-ggplot(stepsperinterval_imputed,aes(interval,average))
g + facet_grid(daytype ~.) + geom_point() + geom_line(group='1')
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

