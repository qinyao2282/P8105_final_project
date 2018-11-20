Data\_source
================
Haowei Ni
2018/11/19

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────────── tidyverse 1.2.1 ─

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ─ Conflicts ────────────────────────── tidyverse_conflicts() ─
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(stringr)
library(readr)
library(readxl)
```

Read original data

``` r
heart_disease =  read_csv("data/Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv") %>%
  janitor::clean_names() %>%
  rename(state = location_abbr) %>%
  mutate(state = state.name[match(state, state.abb)]) %>% 
  select(-data_source, -geographic_level, -class, -topic, -data_value_footnote, -data_value_footnote_symbol, -topic_id, -stratification2, -location_id, -stratification1 )
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_integer(),
    ##   LocationAbbr = col_character(),
    ##   LocationDesc = col_character(),
    ##   GeographicLevel = col_character(),
    ##   DataSource = col_character(),
    ##   Class = col_character(),
    ##   Topic = col_character(),
    ##   Data_Value = col_double(),
    ##   Data_Value_Unit = col_character(),
    ##   Data_Value_Type = col_character(),
    ##   Data_Value_Footnote_Symbol = col_character(),
    ##   Data_Value_Footnote = col_character(),
    ##   StratificationCategory1 = col_character(),
    ##   Stratification1 = col_character(),
    ##   StratificationCategory2 = col_character(),
    ##   Stratification2 = col_character(),
    ##   TopicID = col_character(),
    ##   LocationID = col_character(),
    ##   `Location 1` = col_character()
    ## )

Add air quality data

``` r
airquality_2015 = read_csv("data/airquality.csv") %>%
  janitor::clean_names() %>%
  select(state, pm2_5) %>% 
  group_by(state) %>% 
  summarize(sum(pm2_5))
```

    ## Parsed with column specification:
    ## cols(
    ##   State = col_character(),
    ##   County = col_character(),
    ##   Year = col_integer(),
    ##   `Days with AQI` = col_integer(),
    ##   `Good Days` = col_integer(),
    ##   `Moderate Days` = col_integer(),
    ##   `Unhealthy for Sensitive Groups Days` = col_integer(),
    ##   `Unhealthy Days` = col_integer(),
    ##   `Very Unhealthy Days` = col_integer(),
    ##   `Hazardous Days` = col_integer(),
    ##   `Max AQI` = col_integer(),
    ##   `90th Percentile AQI` = col_integer(),
    ##   `Median AQI` = col_integer(),
    ##   `Days CO` = col_integer(),
    ##   `Days NO2` = col_integer(),
    ##   `Days Ozone` = col_integer(),
    ##   `Days SO2` = col_integer(),
    ##   PM2.5 = col_integer(),
    ##   `Days PM10` = col_integer()
    ## )

Add obesity data

``` r
obesity_data = read_csv("data/National_Obesity_By_State.csv") %>%
  janitor::clean_names() %>%
  rename(state = name) %>%
  rename(obesity_percentage = obesity) %>%
  select(state, obesity_percentage) 
```

    ## Parsed with column specification:
    ## cols(
    ##   OBJECTID = col_integer(),
    ##   NAME = col_character(),
    ##   Obesity = col_double(),
    ##   Shape__Area = col_double(),
    ##   Shape__Length = col_double()
    ## )

``` r
data_with_obesity = left_join(heart_disease, obesity_data)
```

    ## Joining, by = "state"

Add stroke data

``` r
stroke_data = read_csv("data/Stroke_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv") %>%
  janitor::clean_names() %>%
  rename(stroke_value=data_value)%>%
  rename(state = location_abbr) %>%
  mutate(state = state.name[match(state, state.abb)])%>%
  select(state,stroke_value)
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_integer(),
    ##   LocationAbbr = col_character(),
    ##   LocationDesc = col_character(),
    ##   GeographicLevel = col_character(),
    ##   DataSource = col_character(),
    ##   Class = col_character(),
    ##   Topic = col_character(),
    ##   Data_Value = col_double(),
    ##   Data_Value_Unit = col_character(),
    ##   Data_Value_Type = col_character(),
    ##   Data_Value_Footnote_Symbol = col_character(),
    ##   Data_Value_Footnote = col_character(),
    ##   StratificationCategory1 = col_character(),
    ##   Stratification1 = col_character(),
    ##   StratificationCategory2 = col_character(),
    ##   Stratification2 = col_character(),
    ##   TopicID = col_character(),
    ##   LocationID = col_character(),
    ##   `Location 1` = col_character()
    ## )

``` r
stroke_data
```

    ## # A tibble: 59,076 x 2
    ##    state  stroke_value
    ##    <chr>         <dbl>
    ##  1 Alaska          3  
    ##  2 Alaska          6.1
    ##  3 Alaska         63.8
    ##  4 Alaska        106. 
    ##  5 Alaska         NA  
    ##  6 Alaska         63.4
    ##  7 Alaska         81.4
    ##  8 Alaska         65.9
    ##  9 Alaska         52.3
    ## 10 Alaska         52.3
    ## # ... with 59,066 more rows

``` r
left_join(heart_disease,stroke_data)
```

    ## Joining, by = "state"

    ## # A tibble: 102,985,344 x 10
    ##     year state location_desc data_value data_value_unit data_value_type
    ##    <int> <chr> <chr>              <dbl> <chr>           <chr>          
    ##  1  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  2  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  3  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  4  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  5  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  6  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  7  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  8  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ##  9  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ## 10  2015 Alas… Aleutians Ea…       110. per 100,000 po… Age-adjusted, …
    ## # ... with 102,985,334 more rows, and 4 more variables:
    ## #   stratification_category1 <chr>, stratification_category2 <chr>,
    ## #   location_1 <chr>, stroke_value <dbl>

Add income

``` r
income_data = read_excel("data/income_2015.xlsx", range = "A4:D55") %>%
  janitor::clean_names() %>%
  rename(state = united_states, median_income = x55117, income_standard_error = x253) 
data_with_income = left_join(heart_disease,income_data, by = "state")
```

``` r
data_income_obesity = left_join(income_data,data_with_obesity, by = "state")
```

``` r
smoking_data = read_csv("data/smoking.csv") %>% 
  filter(YEAR == "2015-2016") %>% 
  mutate(year = 2015) %>% 
  rename(state = LocationDesc) %>% 
  select(-YEAR) %>% 
  select(year, state, Data_Value)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   Data_Value = col_double(),
    ##   Data_Value_Std_Err = col_double(),
    ##   Low_Confidence_Limit = col_double(),
    ##   High_Confidence_Limit = col_double(),
    ##   Sample_Size = col_integer(),
    ##   DisplayOrder = col_integer()
    ## )

    ## See spec(...) for full column specifications.

``` r
data_income_obesity_smoking = left_join(smoking_data, data_income_obesity, by = "state")

data_income_obesity_smoking_air = left_join(airquality_2015, data_income_obesity_smoking, by = "state")



data_income_obesity_smoking = left_join(smoking_data, data_income_obesity, by = "state")
data_income_obesity_smoking_air = left_join(airquality_2015, data_income_obesity_smoking, by = "state")
```
