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

This project is utilizing three data sets: 1) Housecall Pro: Contains
existing customers’ data between March 2023 to August 2023. 2) Esri
Market Potential: Esri Location Data (2023) regarding new homeowners
moved in: 2019 or later, and the estimated market potential index for
used services for property/garden maintenance in the past 12 months in
an area. 3) Esri Social Media: Esri Location Data (2023) regarding use
of social media platforms, including meta and yelp, in the last 30 days
in an area.

``` r
library(tidyverse)
library(dplyr)
library(skimr)
library(lubridate)
library(ggplot2)
library(sf)
library(tigris)
```

### Data Preprocessing

``` r
housecall_pro <- read_csv("estimate_export_20230805111259.csv")
mk_potential <- read_csv("People in the Yard Market.csv")
social_media <- read_csv("esri_socialmedia.csv") 
#summary(housecall_pro)
#summary(mk_potential)
#summary(social_media)

#Extract zipcode
mk_potential <- mk_potential %>% 
  mutate(zipcode = str_sub(string = mk_potential$'ZIP Code', start = 1, end = 5))
social_media <- social_media %>% 
  mutate(zipcode = str_sub(string = social_media$'ZIP Code', start = 1, end = 5))

#Merge data sets
esri_data <- merge(x=mk_potential, y=social_media, by = 'zipcode', all = TRUE) 
esri_data <- select(esri_data,c("zipcode","2021 OHHs/Moved In: 2019/Later (ACS 5-Yr)",
                                 "2023 HH Used Svc for Property/Garden Maint/12 Mo (%)",
                                 "2023 Social Media: Used Yelp/30 Days",
                                 "2023 Social Media: Used Facebook/30 Days"))
#summary(esri_data)
esri_data <- esri_data %>% 
  mutate(
    mpi_garden_maint 
    = str_sub(string = esri_data$'2023 HH Used Svc for Property/Garden Maint/12 Mo (%)', 
              start = 1, end = -2))

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
base.
