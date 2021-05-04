---
title: "Solution to Question 1"
author: "Sumayyah Musa"
date: "4/28/2021"
output: 
        html_document:
                keep_md: true
---



### On Shopify, we have exactly 100 sneaker shops, and each of these shops sells only one model of shoe. We want to do some analysis of the average order value (AOV). When we look at orders data over a 30 day window, we naively calculate an AOV of $3145.13. Given that we know these shops are selling sneakers, a relatively affordable item, something seems wrong with our analysis. 

* Think about what could be going wrong with our calculation. Think about a better way to evaluate this data. 
* What metric would you report for this dataset?
* What is its value?

#### Read in the data


```r
data <- read.csv("2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv")
glimpse(data) #glimpse the data
```

```
## Rows: 5,000
## Columns: 7
## $ order_id       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
## $ shop_id        <int> 53, 92, 44, 18, 18, 58, 87, 22, 64, 52, 66, 40, 54, 100…
## $ user_id        <int> 746, 925, 861, 935, 883, 882, 915, 761, 914, 788, 848, …
## $ order_amount   <int> 224, 90, 144, 156, 156, 138, 149, 292, 266, 146, 322, 3…
## $ total_items    <int> 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 3, 2000, 1, 1…
## $ payment_method <chr> "cash", "cash", "cash", "credit_card", "credit_card", "…
## $ created_at     <chr> "2017-03-13 12:36:56", "2017-03-03 17:38:52", "2017-03-…
```

On glimpsing the data, the class of order_id, shop_id and user_id were all 'integer', therefore we will change them to 'character' variables.


```r
data$order_id <- as.character(data$order_id)
data$shop_id <- as.character(data$shop_id)
data$user_id <- as.character(data$user_id)

glimpse(data)
```

```
## Rows: 5,000
## Columns: 7
## $ order_id       <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"…
## $ shop_id        <chr> "53", "92", "44", "18", "18", "58", "87", "22", "64", "…
## $ user_id        <chr> "746", "925", "861", "935", "883", "882", "915", "761",…
## $ order_amount   <int> 224, 90, 144, 156, 156, 138, 149, 292, 266, 146, 322, 3…
## $ total_items    <int> 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 3, 2000, 1, 1…
## $ payment_method <chr> "cash", "cash", "cash", "credit_card", "credit_card", "…
## $ created_at     <chr> "2017-03-13 12:36:56", "2017-03-03 17:38:52", "2017-03-…
```


```r
sum(is.na(data))
```

```
## [1] 0
```

No missing values in our data


```r
summary(data$order_amount)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      90     163     284    3145     390  704000
```

Summary statistics of the order amount, showing the mean, minimum, maximum and interquartile values


```r
mean(data$order_amount)
```

```
## [1] 3145.128
```

* From the above mean value calculated, it indicated that the mean value of the total order amount for all the shops was calculated, when in fact we are trying to get the average order value (AOV), which means the average amount spent each time a customer places an order for the sneakers over a 30-day period.

* A better way to evaluate the AOV is to calculate it for each sneaker shop by dividing the total revenue made by the total number of orders for each shop.


```r
data_aov <- data %>%
        group_by(shop_id) %>%
        summarize(aov_per_shop = sum(order_amount)/sum(total_items)) #%>%
        #arrange(desc(aov_per_shop))
```



```r
#arrange our output in ascending orders
data_aov %>%
        arrange(aov_per_shop)
```

```
## # A tibble: 100 x 2
##    shop_id aov_per_shop
##    <chr>          <dbl>
##  1 92                90
##  2 2                 94
##  3 32               101
##  4 100              111
##  5 53               112
##  6 7                112
##  7 93               114
##  8 14               116
##  9 48               117
## 10 56               117
## # … with 90 more rows
```

This showed no extreme values.


```r
#Arrange our output in descending orders
data_aov %>%
        arrange(desc(aov_per_shop))
```

```
## # A tibble: 100 x 2
##    shop_id aov_per_shop
##    <chr>          <dbl>
##  1 78             25725
##  2 42               352
##  3 12               201
##  4 89               196
##  5 99               195
##  6 50               193
##  7 38               190
##  8 51               187
##  9 6                187
## 10 11               184
## # … with 90 more rows
```

The AOV for shop 78 seems very high and extreme, given that the sneakers are all rlatively affordable, we will filter out the values for Shop 78 to further examine the issue.


```r
filter(data, shop_id == 78) 
```

```
##    order_id shop_id user_id order_amount total_items payment_method
## 1       161      78     990        25725           1    credit_card
## 2       491      78     936        51450           2          debit
## 3       494      78     983        51450           2           cash
## 4       512      78     967        51450           2           cash
## 5       618      78     760        51450           2           cash
## 6       692      78     878       154350           6          debit
## 7      1057      78     800        25725           1          debit
## 8      1194      78     944        25725           1          debit
## 9      1205      78     970        25725           1    credit_card
## 10     1260      78     775        77175           3    credit_card
## 11     1385      78     867        25725           1           cash
## 12     1420      78     912        25725           1           cash
## 13     1453      78     812        25725           1    credit_card
## 14     1530      78     810        51450           2           cash
## 15     2271      78     855        25725           1    credit_card
## 16     2453      78     709        51450           2           cash
## 17     2493      78     834       102900           4          debit
## 18     2496      78     707        51450           2           cash
## 19     2513      78     935        51450           2          debit
## 20     2549      78     861        25725           1           cash
## 21     2565      78     915        77175           3          debit
## 22     2691      78     962        77175           3          debit
## 23     2774      78     890        25725           1           cash
## 24     2819      78     869        51450           2          debit
## 25     2822      78     814        51450           2           cash
## 26     2907      78     817        77175           3          debit
## 27     2923      78     740        25725           1          debit
## 28     3086      78     910        25725           1           cash
## 29     3102      78     855        51450           2    credit_card
## 30     3152      78     745        25725           1    credit_card
## 31     3168      78     927        51450           2           cash
## 32     3404      78     928        77175           3          debit
## 33     3441      78     982        25725           1          debit
## 34     3706      78     828        51450           2    credit_card
## 35     3725      78     766        77175           3    credit_card
## 36     3781      78     889        25725           1           cash
## 37     4041      78     852        25725           1           cash
## 38     4080      78     946        51450           2           cash
## 39     4193      78     787        77175           3    credit_card
## 40     4312      78     960        51450           2          debit
## 41     4413      78     756        51450           2          debit
## 42     4421      78     969        77175           3          debit
## 43     4506      78     866        25725           1          debit
## 44     4585      78     997        25725           1           cash
## 45     4716      78     818        77175           3          debit
## 46     4919      78     823        25725           1           cash
##             created_at
## 1   2017-03-12 5:56:57
## 2  2017-03-26 17:08:19
## 3  2017-03-16 21:39:35
## 4   2017-03-09 7:23:14
## 5  2017-03-18 11:18:42
## 6  2017-03-27 22:51:43
## 7  2017-03-15 10:16:45
## 8  2017-03-16 16:38:26
## 9  2017-03-17 22:32:21
## 10  2017-03-27 9:27:20
## 11 2017-03-17 16:38:06
## 12 2017-03-30 12:23:43
## 13 2017-03-17 18:09:54
## 14  2017-03-29 7:12:01
## 15 2017-03-14 23:58:22
## 16 2017-03-27 11:04:04
## 17  2017-03-04 4:37:34
## 18  2017-03-26 4:38:52
## 19 2017-03-18 18:57:13
## 20 2017-03-17 19:36:00
## 21  2017-03-25 1:19:35
## 22  2017-03-22 7:33:25
## 23 2017-03-26 10:36:43
## 24  2017-03-17 6:25:51
## 25 2017-03-02 17:13:25
## 26  2017-03-16 3:45:46
## 27 2017-03-12 20:10:58
## 28  2017-03-26 1:59:27
## 29  2017-03-21 5:10:34
## 30 2017-03-18 13:13:07
## 31 2017-03-12 12:23:08
## 32  2017-03-16 9:45:05
## 33 2017-03-19 19:02:54
## 34 2017-03-14 20:43:15
## 35 2017-03-16 14:13:26
## 36 2017-03-11 21:14:50
## 37 2017-03-02 14:31:12
## 38 2017-03-20 21:14:00
## 39  2017-03-18 9:25:32
## 40  2017-03-01 3:02:10
## 41  2017-03-02 4:13:39
## 42 2017-03-09 15:21:35
## 43 2017-03-22 22:06:01
## 44 2017-03-25 21:48:44
## 45  2017-03-05 5:10:44
## 46 2017-03-15 13:26:46
```

The order amount for 1 sneaker seems to have been inputed as $25725, which is plausible. We will assume that the order amount was mistakenly inputed in cents instead of dollars. We will therefore convert the AOV of shop 78 into dollars, by dividing its aov_per_shop value by 100. 25725/100 = 257.25


```r
data_aov <- data_aov %>% 
                mutate(aov_per_shop = case_when(
                        aov_per_shop == 25725 ~ 257.25,
                        TRUE ~ aov_per_shop
                ))
```



```r
data_aov %>%
        arrange(aov_per_shop)
```

```
## # A tibble: 100 x 2
##    shop_id aov_per_shop
##    <chr>          <dbl>
##  1 92                90
##  2 2                 94
##  3 32               101
##  4 100              111
##  5 53               112
##  6 7                112
##  7 93               114
##  8 14               116
##  9 48               117
## 10 56               117
## # … with 90 more rows
```

Write our output into a csv file name AOV.csv


```r
write.csv(data_aov, "AOV.csv")
```













