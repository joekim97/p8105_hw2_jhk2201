p8105\_HW\_Joseph Kim
================
joseph Kim
10/9/2021

##### Homework 2: Data Wrangling

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Read in Excel (Import dataset and datacleaning)

``` r
trashwheel_df=
  read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Mr. Trash Wheel", range="A2:N535") %>%   
  janitor::clean_names() %>% 
  na.omit(dumpster) %>% 
  mutate(sports_balls=round(sports_balls,0)) 
```

``` r
precipitation2018= 
  read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "2018 Precipitation", range = "A2:B14") %>% 
  janitor::clean_names() %>%
  na.omit() %>%
  mutate(year="2018") 
```

``` r
precipitation2019= 
  read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "2019 Precipitation", range = "A2:B14") %>% 
  janitor::clean_names() %>%
  na.omit() %>%
  mutate(year="2019") 
```

``` r
totalprecipitation2018_2019 = 
  rbind(precipitation2018, precipitation2019) %>%
  mutate(month = month.name[month])
```

Make sure to write the paragraph here.
