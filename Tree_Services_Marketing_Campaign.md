Tree Services Marketing Analysis - New Homeowners in Yard Market
================
Olivia Chen
2023-09-17

## Introduction

An anonymous company located in Long Island, NY, provides professional
tree services, such as pruning, trimming, removal, tree disease
diagnosis, and plant healthcare services. In this study, I am trying to
provide digital marketing campaign strategies for new homeowner in yard
market in Long Island by understanding the company’s existing customer
pool and identifying the market potential.

## Data

This project is utilizing the following data sets: 1) Housecall Pro:
Contains existing customers’ data between March 2023 to August 2023. 2)
Esri Market Potential: Esri Location Data (2023) regarding new
homeowners moved in: 2019 or later, and the estimated market potential
index for used services for property/garden maintenance in the past 12
months in an area. 3) Esri Social Media: Esri Location Data (2023)
regarding use of social media platforms, including meta and yelp, in the
last 30 days in an area. 4) Esri Yard Owner: Esri Location Data (2023)
for the average lawn/garden care spending and the percentage of house
have a yard.

``` r
library(tidyverse)
library(dplyr)
library(skimr)
library(lubridate)
library(ggplot2)
library(sf)
library(tigris)
library(ggcorrplot)
```

### Data Preprocessing

``` r
housecall_pro = read_csv("estimate_export_20230805111259.csv")
mk_potential = read_csv("People in the Yard Market.csv")
social_media = read_csv("esri_socialmedia.csv") 
yard_owner = read.csv("yardowner.csv")
#summary(housecall_pro)
#summary(mk_potential)
#summary(social_media)
#summary(yard_owner)

#Extract zipcode
mk_potential <- mk_potential %>% 
  mutate(zipcode = str_sub(string = mk_potential$'ZIP Code', start = 1, end = 5))
social_media <- social_media %>% 
  mutate(zipcode = str_sub(string = social_media$'ZIP Code', start = 1, end = 5))
yard_owner <- yard_owner %>% 
  mutate(zipcode = str_sub(string = social_media$'ZIP Code', start = 1, end = 5))

#Merge data sets
esri_data <- merge(x=mk_potential, y=social_media, by = 'zipcode', all = TRUE) 
esri_data <- select(esri_data,c("zipcode", "2023 Median Age",
                                 "2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)",
                                 "2023 HH Used Svc for Property/Garden Maint/12 Mo (%)",
                                 "2023 Social Media: Used Yelp/30 Days",
                                 "2023 Social Media: Used Facebook/30 Days"))
esri_data <- merge(x=esri_data, y=yard_owner, by = 'zipcode', all = TRUE)

#summary(esri_data)
esri_data <- esri_data %>% 
  mutate(
    mpi_garden_maint 
    = str_sub(string = esri_data$'2023 HH Used Svc for Property/Garden Maint/12 Mo (%)', 
              start = 1, end = -2))

esri_data <- subset(esri_data, mpi_garden_maint != 0) 
esri_data$mpi_garden_maint <- as.numeric(esri_data$mpi_garden_maint)

esri_data <- esri_data %>% 
  mutate(
    perc_of_house_with_garden 
    = str_sub(string = esri_data$'X2023.Have.a.Garden....', 
              start = 1, end = -2))
esri_data$perc_of_house_with_garden <- as.numeric(esri_data$perc_of_house_with_garden)

esri_data$avg_lawn_spending <- str_replace_all(esri_data$'X2023.Lawn.Garden..Avg.', "\\$", "")

esri_data$avg_lawn_spending <- str_replace_all(esri_data$avg_lawn_spending, "\\,", "")

esri_data$avg_lawn_spending <- as.numeric(esri_data$avg_lawn_spending)

#remove records(zipcodes) doesn't have any yard/garden
esri_data <- subset(esri_data, perc_of_house_with_garden != 0) 

esri_data <- select(esri_data,c("zipcode", "2023 Median Age",
                                "2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)",
                                "2023 Social Media: Used Yelp/30 Days",
                                "2023 Social Media: Used Facebook/30 Days",
                                "mpi_garden_maint",
                                "perc_of_house_with_garden",
                                "avg_lawn_spending"
                                ))

#skim_without_charts(esri_data)

#unique(housecall_pro$'Job Status')

#Extract zipcode from address
housecall_pro$zipcode <- str_sub(string = housecall_pro$'Address', start = -5)

#Re-categorize job status
housecall_pro$'Job Status'[housecall_pro$'Job Status' == "DONE"] <- "completed"
housecall_pro$'Job Status'[housecall_pro$'Job Status' %in% 
                             c("Awaiting Approval", "approved", "pro approved", "SCHEDULED")] <-"pending"
housecall_pro$'Job Status'[housecall_pro$'Job Status' %in% 
                             c("declined", "expired", "UNSCHEDULED")] <- "issue"    

#Assign an unique customer_id to customer with same name and phone number
housecall_pro <- 
  housecall_pro %>%
  mutate(customer_key = paste(housecall_pro$'Customer', housecall_pro$'Mobile Phone', sep="_")) %>% 
  group_by(customer_key) %>% 
  mutate(customer_id = cur_group_id() + 1000) %>% 
  ungroup() %>% 
  select(-customer_key)

#Convert the datetime to date
housecall_pro$Date <- format(strptime(housecall_pro$Date, format = "%m/%d/%y %H:%M"),format = "%Y-%m-%d")

#Select the needed columns 
housecall_pro <-
  housecall_pro %>% 
  select("HCP Id", "Date", "Job Status", "Subtotal", "customer_id", "zipcode")
```

## Exisiting Customers

``` r
length(unique(housecall_pro$'HCP Id'))
```

    ## [1] 363

``` r
length(unique(housecall_pro$customer_id))
```

    ## [1] 312

``` r
ggplot(housecall_pro, aes(x = housecall_pro$'Job Status', fill = housecall_pro$'Job Status')) +
  geom_bar() +
  geom_text(stat='count', aes(label = after_stat(count), vjust = -0.5)) +
  labs(title = "Job Status Distribution", x = 'Job Status', y = "Count") +
  guides(fill=guide_legend(title='Job Status'))
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

According to the above job status distribution chart, only 9.64% of the
jobs has been completed, while over 73% of the job are either pending
for approval or schedule.

``` r
#Filtered out records with insufficient job status
filtered_data <- housecall_pro %>%
  filter(`Job Status` != 'issue')

#The repeat customer distribution
distribution <- filtered_data %>% 
  count(customer_id, 'HCP Id') %>%
  group_by(n) %>%
  summarize(number_of_customers = n())

distribution <- distribution %>% 
  mutate(perc_of_total = number_of_customers/sum(number_of_customers)*100)

print(distribution)
```

    ## # A tibble: 3 × 3
    ##       n number_of_customers perc_of_total
    ##   <int>               <int>         <dbl>
    ## 1     1                 240         90.2 
    ## 2     2                  18          6.77
    ## 3     3                   8          3.01

Currently there are 363 transactions associated with belongs to 312
customers recorded in the data set between March 2023 and August 2023.
According to the above the percentage of repeat customers (customers
with more than one sufficient job) is only 9.77%. Since most of the
customer are either one-time customers or low-frequency customers, it’s
important to identify the market potentials and increase the customer
base. Consider the size of the housecall pro data set is too small, I
decided to move forward and focus on the eris data.

## Market Potential

### Target Age Range

``` r
#Weighted Average of median age --> HH moved in 2019/later
weighted.mean(
  esri_data$'2023 Median Age',
  w = esri_data$'2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)',
  na.rm = TRUE)
```

    ## [1] 43.42291

``` r
age_boxplot = boxplot(esri_data$'2023 Median Age')
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
age_boxplot$stats
```

    ##      [,1]
    ## [1,] 34.2
    ## [2,] 41.5
    ## [3,] 44.7
    ## [4,] 46.7
    ## [5,] 53.9

After excluding the outliers of the median age, I was able to narrow the
target age range between 34 and 54.

### Market Potential in Garden Maintenance

``` r
weighted.mean(
  esri_data$mpi_garden_maint,
  w = esri_data$'2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)',
  na.rm = TRUE)
```

    ## [1] 23.13711

The weighted average of market potential in garden maintenance is 23.14%

### Average New Homeowner move in: 2019/later

``` r
mean(esri_data$'2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)')
```

    ## [1] 183.0678

The average of New Homeowner move in: 2019/later is 183.

### Lawn/garden care spending

``` r
weighted.mean(
  esri_data$avg_lawn_spending,
  w = esri_data$'2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)',
  na.rm = TRUE)
```

    ## [1] 1066.122

The weighted average of law/garden care spending is \$1066.12.

### Correlation Matrix of Esri data

``` r
numeric_data <- esri_data[, sapply(esri_data, is.numeric)]
corr_matrix <- round(cor(numeric_data), 1)
corr_matrix <- na.omit(corr_matrix)
ggcorrplot(cor(numeric_data),type = 'lower')
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Noticed that there is a strong positive correlation between the used of
Facebook(Meta) and the used of Yelp in the last 30 days according to the
correlation matrix. I decided to take both Facebook(Meta) and Yelp into
consideration.

### Target Location

``` r
sorted_meta_data <- esri_data[order(esri_data$"2023 Social Media: Used Facebook/30 Days", decreasing=TRUE),]
top_20_meta <- head(sorted_meta_data,20)
top_20_meta_zc <- top_20_meta$zipcode

sorted_yelp_data <- esri_data[order(esri_data$"2023 Social Media: Used Yelp/30 Days", decreasing=TRUE),]
top_20_yelp <- head(sorted_yelp_data,20)
top_20_yelp_zc <- top_20_yelp$zipcode

new_hh <- subset(esri_data, "2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)" >183)
new_hh_zc <- new_hh$zipcode

avg_law_spending <- subset(esri_data, avg_lawn_spending >1066.12)
avg_law_spending_zc <- avg_law_spending$zipcode

avg_house_with_garden <- subset(esri_data, perc_of_house_with_garden >23.14)
avg_house_with_garden_zc <- avg_house_with_garden$zipcode

list_of_lists <- list(top_20_meta_zc,top_20_yelp_zc,new_hh_zc,avg_law_spending_zc, avg_house_with_garden_zc)
target_location <- Reduce(intersect, list_of_lists)

# Download zip code geographies for New York, transform CRS
# zip code tabulation areas; tigris function, last update 2010
nyz = zctas(year = 2010, state = "New York")
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
nyz1 = st_transform(nyz, 2263)

# Download county geographies for Long Island, transform CRS
lic = counties(state = "New York") %>%
  filter(NAME %in% c("Nassau", "Suffolk"))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
lic1 = st_transform(lic, 2263)

# Filter zip codes for relevant counties; st_ from sf package -> spatial filter
liz1 = nyz1 %>%
  st_filter(lic1)

liz2 = liz1
liz2$interest = ifelse(liz1$ZCTA5CE10 %in% target_location, "Zip Code of Interest", "Lower Interest")

ggplot(liz2)+
  geom_sf(aes(fill = interest))+
  labs(title = "Target Location Distribution")
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

After Taking the number of Yelp & Meta users in the past 30 days, the
number of new homeowner, market potential in garden maintenance and the
average spending in lawn care in consideration, I found four zip code
which is the target location, including 11040 (Garden City Park), 11743
(Huntington), 11746 (Huntington Station), 11758 (North Massapequa).

## Recommendation

Based on the above results, I would recommend the client to launch a
“Stress-Free Holiday” campaign. It’s a 75-day digital ads campaign
encouraging potential customer to contact the tree service company now
so that they could enjoy a stress-free holiday. This campaign will
target audiences between age 34 and 54, and targeted zip codes (11746,
11758, 11040, and 11743).

------------------------------------------------------------------------

## Further Study: Using Radius Approach

I’m also interested in finding the center point of those four zip codes
and using the centroid to locate the area cover all targeted zip codes
with the minimum radius.

``` r
## First filter the zipcodes of interest to find their centroid
cents = liz2 %>%
  filter(interest == "Zip Code of Interest") %>%
  st_centroid()

## Create a new object with the coordinate geography for the centroid, assign CRS
center = data.frame(midpoint = "midpoint", longitude = NA, latitude = NA)
center[,2] = mean(st_coordinates(cents)[,1]) ; center[,3] = mean(st_coordinates(cents)[,2]) 
center1 = st_as_sf(center, coords = c("longitude", "latitude"), crs = st_crs(liz2))
center1 = st_transform(center1, 2263) 

## Check
ggplot(liz2)+
  geom_sf(aes(fill = interest))+
  geom_sf(data = center1, size = 5, shape = 17)
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Find distances of centroids within each zip code from center point - find length of radius
distances = st_distance(cents,center1)


# Create a buffer with radius = max distance around centerpoint
buffer = st_as_sf(st_buffer(center1, max(distances)))

## Check
ggplot()+
  geom_sf(data = liz2, aes(fill = interest))+
  geom_sf(data = buffer)+
  geom_sf(data = center1, size = 5, shape = 17)
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Determine which zip codes lie within our radius
zips.within.radius = liz2 %>%
  st_filter(buffer, .predicate = st_intersects) %>%
  select(ZCTA5CE10)

# Add column to liz2 for being within radius
liz2$within.radius = ifelse(liz2$ZCTA5CE10 %in% zips.within.radius$ZCTA5CE10, "Within Radius", "Outside Radius")

# Plot
ggplot(liz2)+
  geom_sf(aes(fill = within.radius))
```

![](Tree_Services_Marketing_Campaign_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
# centroid 
centroid <- liz2 %>% st_filter(center1) %>% select(ZCTA5CE10)

# radius (max distances)
radius_in_mile <- max(distances)/5280
```

In case the client is interested in launch a campaign using the market
radius approach. We could set the centroid zip code as 11803 with the
radius of 10.63 miles. This has not been included in the final
recommendation as this approach will include a lot of zip codes which
were filtered out in an earlier stage. This approach will be helpful if
the target zip codes locate near each other.
