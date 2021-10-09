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
library(dplyr)
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

### Problem 2:

``` r
pols_month = 
  read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, c("year","month","day"), "-") %>%
  mutate(month = as.integer(month), year = as.integer(year)) %>%
  mutate(month = month.name[month]) %>%
  mutate(president = ifelse(prez_gop == 0,"dem","gop")) %>%
  select(-prez_dem, -prez_gop, -day)
```

    ## Rows: 822 Columns: 9

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_df = 
  read_csv("./data/snp.csv") %>%
  janitor::clean_names() %>%
  separate(date, c("year","month", "day"), "/") %>%
  mutate(month = as.integer(month), year = as.integer(year)) %>%
  mutate(month = month.name[month], year= year + 2000) %>%
  arrange(year, month)
```

    ## Rows: 787 Columns: 2

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_df = 
  read_csv("./data/unemployment.csv") %>%
  pivot_longer(
    2:13,
    names_to = "month",
    values_to = "unemployment_level"
  ) %>%
  mutate(month = match(month, month.abb)) %>%
  mutate(month = month.name[month]) %>%
  janitor::clean_names() %>%
  arrange(year,month) %>% 
  mutate(year = as.integer(year))
```

    ## Rows: 68 Columns: 13

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_pols_merge = 
  left_join(snp_df, pols_month, by=c("year", "month"))

complete_merge = 
  left_join(snp_pols_merge, unemployment_df, by=c("year", "month"))
```

Write a short paragraph about these datasets. Explain briefly what each
dataset contained, and describe the resulting dataset (e.g. give the
dimension, range of years, and names of key variables).

``` r
babyname_data =  
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(ethnicity = ifelse(ethnicity=="ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="WHITE NON HISP", "WHITE NON HISPANIC", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="BLACK NON HISP", "BLACK NON HISPANIC", ethnicity)) %>%
  distinct()
```

    ## Rows: 19418 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Gender, Ethnicity, Child's First Name
    ## dbl (3): Year of Birth, Count, Rank

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
