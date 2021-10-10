p8105\_HW\_Joseph Kim
================
joseph Kim
10/9/2021

# Homework 2: Data Wrangling

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

## Problem 1

### Read in Excel (Import dataset and datacleaning)

``` r
trashwheel_df=
  read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Mr. Trash Wheel", range="A2:N535") %>%   
  janitor::clean_names() %>% 
  na.omit(dumpster) %>% 
  mutate(sports_balls=round(sports_balls,0)) 
```

### Cleaning Precipitation Data from 2018 and 2019

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

### Combinging Precipitation Databsets

``` r
totalprecipitation2018_2019 = 
  rbind(precipitation2018, precipitation2019) %>%
  mutate(month = month.name[month])
```

``` r
sum(precipitation2018$total)
```

    ## [1] 70.33

``` r
median(trashwheel_df$sports_balls)
```

    ## [1] 9

The Trash wheel dataset was large and complex intially. Once cleaned,
there were 453 rows and 14 variables. The sum of precipitation in 2018
was 70.33 inches. The median of sports balls from the Trash wheel
dataset was 9.

When I began, the data in the 2018 and 2019 precipitation datasets were
simply the numbers for the months of the year with precipitation levels
for the corresponding month. The variable “year” was added to specific
which year the row fell in. Once the datasets were combined, the month
values were changed to their actual names, combined, and arranged by
month and year. The combined dataset had 24 rows x 3 columns.

------------------------------------------------------------------------

## Problem 2:

``` r
pols_month = 
  read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, c("year","month","day"), "-") %>%
  mutate(month = as.integer(month), year = as.integer(year)) %>%
  mutate(month = month.name[month]) %>%
  mutate(president = ifelse(prez_gop == 0,"dem","gop")) %>%
  select(-prez_dem, -prez_gop, -day)

snp_df = 
  read_csv("./data/snp.csv") %>%
  janitor::clean_names() %>%
  separate(date, c("year","month", "day"), "/") %>%
  mutate(month = as.integer(month), year = as.integer(year)) %>%
  mutate(month = month.name[month], year= year + 2000) %>%
  arrange(year, month)

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

snp_pols_merge = 
  left_join(snp_df, pols_month, by=c("year", "month"))

complete_merge = 
  left_join(snp_pols_merge, unemployment_df, by=c("year", "month"))
```

The three datasets given were simplier than the trash wheel dataset. The
pols\_month had 822 rows x 9 columns. The snp\_df had 787 rows x 4
columns. The Unemployment\_df had 816 rows x 3 variables. Onces merged
together, the complete\_merge had 787 rows and 12 columns. The years
ranged from 2001 to 2012. Some of the key variables were the
unemployment rate on a given day, the party of the sitting president,
and close value for the snp.

## Problem 3:

### Tidying Initial Baby Name Dataset

``` r
babyname_data =  
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(ethnicity = ifelse(ethnicity=="ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="WHITE NON HISP", "WHITE NON HISPANIC", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="BLACK NON HISP", "BLACK NON HISPANIC", ethnicity)) %>%
  distinct()
```

### Creating Olivia Dataframe

``` r
Oliviatrend_data =  
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  filter(childs_first_name=="Olivia") %>% 
  arrange(year_of_birth, ethnicity) %>% 
  select(-gender, -count) %>%
  mutate(ethnicity = ifelse(ethnicity=="ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="WHITE NON HISP", "WHITE NON HISPANIC", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="BLACK NON HISP", "BLACK NON HISPANIC", ethnicity)) %>%
  distinct() %>%
pivot_wider(
    names_from = "year_of_birth",
    values_from = "rank"
    ) 
```

### Creating Male Name Dataframe:

``` r
malename_data =  
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  filter(gender == "MALE") %>% 
  select(-gender, -count) %>%
  mutate(ethnicity = ifelse(ethnicity=="ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="WHITE NON HISP", "WHITE NON HISPANIC", ethnicity)) %>%
  mutate(ethnicity = ifelse(ethnicity=="BLACK NON HISP", "BLACK NON HISPANIC", ethnicity)) %>%
  distinct() %>%
  arrange(year_of_birth) %>% 
  filter(rank == "1") %>%
pivot_wider(
    names_from = "year_of_birth",
    values_from = "childs_first_name"
    ) 
```

### Creating Scatterplot

``` r
scatterplot_male_df=
 filter(
  babyname_data, gender == "MALE", ethnicity == "WHITE NON HISPANIC",year_of_birth == "2016") %>%
  select(childs_first_name,rank, count)

ggplot(scatterplot_male_df, aes(x=rank, y=count)) + geom_point()
```

![](HW2_Joseph-Kim_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("baby_name_scatterplot.png")
```
