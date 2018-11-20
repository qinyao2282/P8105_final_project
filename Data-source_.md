Data\_source
================
Haowei Ni
2018/11/19

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

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
  mutate(state = state.name[match(state, state.abb)])
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
  select(state, pm2_5)
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

``` r
heart_data = left_join(airquality_2015, heart_disease) 
```

    ## Joining, by = "state"

``` r
inner_join(airquality_2015, heart_disease)  
```

    ## Joining, by = "state"

    ## # A tibble: 1,493,388 x 20
    ##    state pm2_5  year location_desc geographic_level data_source class topic
    ##    <chr> <int> <int> <chr>         <chr>            <chr>       <chr> <chr>
    ##  1 Alab~    75  2015 Bibb County   County           NVSS        Card~ Hear~
    ##  2 Alab~    75  2015 Autauga Coun~ County           NVSS        Card~ Hear~
    ##  3 Alab~    75  2015 Baldwin Coun~ County           NVSS        Card~ Hear~
    ##  4 Alab~    75  2015 Barbour Coun~ County           NVSS        Card~ Hear~
    ##  5 Alab~    75  2015 Blount County County           NVSS        Card~ Hear~
    ##  6 Alab~    75  2015 Bullock Coun~ County           NVSS        Card~ Hear~
    ##  7 Alab~    75  2015 Butler County County           NVSS        Card~ Hear~
    ##  8 Alab~    75  2015 Calhoun Coun~ County           NVSS        Card~ Hear~
    ##  9 Alab~    75  2015 Chambers Cou~ County           NVSS        Card~ Hear~
    ## 10 Alab~    75  2015 Cherokee Cou~ County           NVSS        Card~ Hear~
    ## # ... with 1,493,378 more rows, and 12 more variables: data_value <dbl>,
    ## #   data_value_unit <chr>, data_value_type <chr>,
    ## #   data_value_footnote_symbol <chr>, data_value_footnote <chr>,
    ## #   stratification_category1 <chr>, stratification1 <chr>,
    ## #   stratification_category2 <chr>, stratification2 <chr>, topic_id <chr>,
    ## #   location_id <chr>, location_1 <chr>

``` r
anti_join(airquality_2015, heart_disease) 
```

    ## Joining, by = "state"

    ## # A tibble: 15 x 2
    ##    state                pm2_5
    ##    <chr>                <int>
    ##  1 Country Of Mexico        0
    ##  2 District Of Columbia   175
    ##  3 Puerto Rico             91
    ##  4 Puerto Rico             64
    ##  5 Puerto Rico              0
    ##  6 Puerto Rico             39
    ##  7 Puerto Rico             63
    ##  8 Puerto Rico            111
    ##  9 Puerto Rico             43
    ## 10 Puerto Rico              0
    ## 11 Puerto Rico             42
    ## 12 Puerto Rico              0
    ## 13 Puerto Rico             17
    ## 14 Virgin Islands          20
    ## 15 Virgin Islands         109

``` r
anti_join(airquality_2015, heart_disease)
```

    ## Joining, by = "state"

    ## # A tibble: 15 x 2
    ##    state                pm2_5
    ##    <chr>                <int>
    ##  1 Country Of Mexico        0
    ##  2 District Of Columbia   175
    ##  3 Puerto Rico             91
    ##  4 Puerto Rico             64
    ##  5 Puerto Rico              0
    ##  6 Puerto Rico             39
    ##  7 Puerto Rico             63
    ##  8 Puerto Rico            111
    ##  9 Puerto Rico             43
    ## 10 Puerto Rico              0
    ## 11 Puerto Rico             42
    ## 12 Puerto Rico              0
    ## 13 Puerto Rico             17
    ## 14 Virgin Islands          20
    ## 15 Virgin Islands         109

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

library(tidyverse) library(data.table) stroke\_data = read\_csv("data/Stroke\_Mortality\_Data\_Among\_US\_Adults\_\_35\_\_\_by\_State\_Territory\_and\_County.csv") %&gt;% janitor::clean\_names() %&gt;% rename(stroke\_value=data\_value)%&gt;% rename(state = location\_abbr) %&gt;% mutate(state = state.name\[match(state, state.abb)\])%&gt;% select(state,stroke\_value)

left\_join(heart\_disease,stroke\_data)

\`\`\`

Add income

``` r
income_data = read_excel("data/income_2015.xlsx", range = "A4:D55") %>%
  janitor::clean_names() %>%
  rename(state = united_states, median_income = x55117, income_standard_error = x253) 

data_with_income = left_join(heart_disease,income_data, by = "state")
```

&lt;&lt;&lt;&lt;&lt;&lt;&lt; HEAD Add smoking

``` r
smoking_data = read_csv("data/smoking.csv")
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

=======

``` r
data_income_obesity = left_join(income_data,data_with_obesity, by = "state")
```
