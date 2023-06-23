---
title: "Mini-Project 01"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 01


```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.2.2
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.4 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2
```

```
## Warning: package 'ggplot2' was built under R version 4.2.2
```

```
## Warning: package 'readr' was built under R version 4.2.2
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```


```r
#source the data
rats_raw <- read_csv("../data/project_1/rats_nyc.csv")
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 41845 Columns: 56
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (33): closed_date, agency, agency_name, complaint_type, descriptor, loc...
## dbl   (9): unique_key, incident_zip, x_coordinate_state_plane, y_coordinate_...
## lgl  (13): landmark, facility_type, school_or_citywide_complaint, vehicle_ty...
## dttm  (1): created_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
#select the data that we're interested in
rats_nyc <- rats_raw %>% 
  select(created_date, closed_date, city)
```


```r
#clean the data in a basic way

#issue 1: some cities have different capitalizations. Use stringr's to_title()
rats_nyc <- rats_nyc %>%  
  drop_na() %>%
  mutate(
      city = str_to_title(city)
  )

rats_nyc
```

```
## # A tibble: 36,849 × 3
##    created_date        closed_date            city         
##    <dttm>              <chr>                  <chr>        
##  1 2015-09-04 00:00:00 09/18/2015 12:00:00 AM New York     
##  2 2015-09-04 00:00:00 10/28/2015 12:00:00 AM Staten Island
##  3 2015-09-04 00:00:00 09/14/2015 12:00:00 AM Brooklyn     
##  4 2015-09-04 00:00:00 09/22/2015 12:00:00 AM Bronx        
##  5 2015-09-04 00:00:00 09/22/2015 12:00:00 AM Brooklyn     
##  6 2015-09-04 00:00:00 09/22/2015 04:26:36 PM Flushing     
##  7 2015-09-04 00:00:00 08/19/2015 12:00:00 AM New York     
##  8 2015-09-04 00:00:00 09/25/2015 12:00:00 AM Staten Island
##  9 2015-09-04 00:00:00 07/30/2015 12:00:00 AM Brooklyn     
## 10 2015-09-04 00:00:00 09/15/2015 04:03:54 PM Brooklyn     
## # … with 36,839 more rows
```

```r
#the data is already in long format when pivoted by year
```


# What are some questions I think this data can answer? 

## What are some months with the most rats? Does season play into this? 

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.2.2
```

```
## Loading required package: timechange
```

```
## Warning: package 'timechange' was built under R version 4.2.2
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
#used to manipulate date columns... 
```


```r
# a helpful function sourced from a thread: https://stackoverflow.com/questions/36502140/determine-season-from-date-using-lubridate-in-r 

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}
```


```r
# translating months to strings, creating a season column

#issue 1: the created date doesn't have a time but the closed date does. Remove the time. 
#issue 2: there is no standard function for extracting seasons, must be mapped manually.

rats_season <- rats_nyc %>%
  mutate(
    closed_date = str_extract(closed_date, '.{10}'), #gets only the date, not the time
    closed_date = mdy(closed_date),
    created_date = as_date(created_date),
    created_month = month(created_date, label=TRUE),
    season = getSeason(created_date)
  )
```




```r
# data visualization 1

#I want to show a distribution of each month given the season. 
months_distribution <- ggplot(data = rats_season) +
  geom_bar(mapping=aes(x=created_month, fill=season), stat="count") +
  scale_fill_manual(values = c("Winter" = "#0033cc", "Spring"="#009933", "Summer"="#ffff00", "Fall"="#ff9900")) +
  labs(x="", y="cases by month", title="Seasonality of rat cases") +
  theme_minimal() + 
  theme(legend.position = "top", axis.title.y = element_text(angle=0, vjust=0.5), legend.title=element_blank())  

#This is a solved issue at this point, but I was having a problem where my theme() layer wasn't being applied. The reason why is because it wasn't the last layer, or was applied by theme_minimal() so it was being overwritten and being ineffective. 
  
#changes to make: remove legend title, change the colors used to represent the seasons, use ggsave() to save the plot
months_distribution
```

![](lastname_project_01_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ggsave("../figures/project_1/rat_cases_seasonality.png", months_distribution, width=6, height=4)
```


## How does the average case completion time vary by location? 

```r
#there are many cities that are sparse or only have a few cases. Filter out cities with less than 150 cases

city_names <- (rats_season %>%
  group_by(city) %>%
  summarize(city_count=n()) %>%
  filter(city_count >= 150))$city 

rats_seasons_cities <- rats_season %>%
  filter(city %in% city_names)  %>% #doesn't make too much of a dent in our total number of rows, which makes sense
  mutate(
    time_to_complete = as.numeric(closed_date - created_date)
  )
rats_seasons_cities
```

```
## # A tibble: 34,761 × 6
##    created_date closed_date city          created_month season time_to_complete
##    <date>       <date>      <chr>         <ord>         <fct>             <dbl>
##  1 2015-09-04   2015-09-18  New York      Sep           Summer               14
##  2 2015-09-04   2015-10-28  Staten Island Sep           Summer               54
##  3 2015-09-04   2015-09-14  Brooklyn      Sep           Summer               10
##  4 2015-09-04   2015-09-22  Bronx         Sep           Summer               18
##  5 2015-09-04   2015-09-22  Brooklyn      Sep           Summer               18
##  6 2015-09-04   2015-09-22  Flushing      Sep           Summer               18
##  7 2015-09-04   2015-08-19  New York      Sep           Summer              -16
##  8 2015-09-04   2015-09-25  Staten Island Sep           Summer               21
##  9 2015-09-04   2015-07-30  Brooklyn      Sep           Summer              -36
## 10 2015-09-04   2015-09-15  Brooklyn      Sep           Summer               11
## # … with 34,751 more rows
```


```r
#some of the times to complete are negative? Some closed dates happen before the created dates. I may be misunderstanding the columns. 

#In my understanding, a case is created, and then closed when the case is done. 

#I'll just remove the rows with negative completion times.

rats_seasons_cities <- rats_seasons_cities %>%
  filter(time_to_complete > 0)
rats_seasons_cities #that's a lot of rows gone... 
```

```
## # A tibble: 25,576 × 6
##    created_date closed_date city          created_month season time_to_complete
##    <date>       <date>      <chr>         <ord>         <fct>             <dbl>
##  1 2015-09-04   2015-09-18  New York      Sep           Summer               14
##  2 2015-09-04   2015-10-28  Staten Island Sep           Summer               54
##  3 2015-09-04   2015-09-14  Brooklyn      Sep           Summer               10
##  4 2015-09-04   2015-09-22  Bronx         Sep           Summer               18
##  5 2015-09-04   2015-09-22  Brooklyn      Sep           Summer               18
##  6 2015-09-04   2015-09-22  Flushing      Sep           Summer               18
##  7 2015-09-04   2015-09-25  Staten Island Sep           Summer               21
##  8 2015-09-04   2015-09-15  Brooklyn      Sep           Summer               11
##  9 2015-09-05   2015-10-08  Brooklyn      Sep           Summer               33
## 10 2015-09-04   2015-09-10  New York      Sep           Summer                6
## # … with 25,566 more rows
```

```r
rats_seasons_cities_avgs <- rats_seasons_cities %>%
  group_by(city) %>%
  summarize(
    avg_completion_time = mean(time_to_complete, na.rm=T),
    var_completion_time = var(time_to_complete, na.rm=T)
  )
```


```r
# data visualization 2
#use a simple geom_bar to show how each city fares.
time_efficiency_plot <- ggplot(data = rats_seasons_cities_avgs) + 
  geom_bar(mapping=aes(x=fct_reorder(city, avg_completion_time, .desc=TRUE), y=avg_completion_time), stat="identity") +
  coord_flip() +
  labs(y="average completion time in days", x="", title="Time efficiency of popular cities", subtitle="where 'popular cities' have had at least 150 cases") +
  theme_minimal()

time_efficiency_plot
```

![](lastname_project_01_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggsave("../figures/project_1/time_efficiency_plot.png", time_efficiency_plot, width=6, height=4)
```


## How do the different months compare in terms of time to complete? Are there any patterns that appear throughout they different months of all years?

```r
#issue 1: there are a few cases with pretty bad outliers past around 110 days. Filter them.
rats_seasons_cities_days <- rats_seasons_cities %>%
  filter(time_to_complete <= 60) %>%
  mutate(
    created_day = mday(created_date) #day in the month
  )
rats_seasons_cities_days
```

```
## # A tibble: 25,365 × 7
##    created_date closed_date city          created_month season time_to…¹ creat…²
##    <date>       <date>      <chr>         <ord>         <fct>      <dbl>   <int>
##  1 2015-09-04   2015-09-18  New York      Sep           Summer        14       4
##  2 2015-09-04   2015-10-28  Staten Island Sep           Summer        54       4
##  3 2015-09-04   2015-09-14  Brooklyn      Sep           Summer        10       4
##  4 2015-09-04   2015-09-22  Bronx         Sep           Summer        18       4
##  5 2015-09-04   2015-09-22  Brooklyn      Sep           Summer        18       4
##  6 2015-09-04   2015-09-22  Flushing      Sep           Summer        18       4
##  7 2015-09-04   2015-09-25  Staten Island Sep           Summer        21       4
##  8 2015-09-04   2015-09-15  Brooklyn      Sep           Summer        11       4
##  9 2015-09-05   2015-10-08  Brooklyn      Sep           Summer        33       5
## 10 2015-09-04   2015-09-10  New York      Sep           Summer         6       4
## # … with 25,355 more rows, and abbreviated variable names ¹​time_to_complete,
## #   ²​created_day
```


```r
# data visualization 3
season_speed_plot <- ggplot(data = rats_seasons_cities_days, mapping=aes(x=created_day, y=time_to_complete)) + 
  geom_point(alpha=0.2, mapping=aes(color=season)) + 
  scale_color_manual(values = c("Winter" = "#0033cc", "Spring"="#009933", "Summer"="#ffff00", "Fall"="#ff9900")) +
  geom_smooth(method="gam", color="black") +
  facet_wrap(vars(created_month), nrow=3, ncol=4) + 
  labs(title="How the date of a season affects completion speed", y="average completion time in days", x="day of month") + 
  theme(legend.position = "top", legend.title=element_blank())

#save the graph
season_speed_plot
```

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

![](lastname_project_01_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
ggsave("../figures/project_1/season_speed_plot.png", season_speed_plot, width=6, height=4)
```

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```
