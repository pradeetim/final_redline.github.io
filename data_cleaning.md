Data Cleaning
================
Polly Wu (rw3031)
2024-11-24

``` r
library(tidyverse)
library(readxl)
```

## SNAP enrollment data

``` r
SNAP = 
  read_csv(file = "./Datasets/ACS_SNAP_ENROLLMENT.csv", skip=1)|>
  janitor::clean_names()|>
  mutate(geoid = str_remove(geography, ".*US"))
```

    ## Rows: 2329 Columns: 458
    ## ── Column specification ────────
    ## Delimiter: ","
    ## chr (458): Geography, Geographic Area Name, Estimate!!Total!!Households, Mar...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## poverty level

``` r
poverty = 
  read_csv(file = "./Datasets/ACS-poverty-level.csv", skip=1)|>
  janitor::clean_names()|>
  mutate(geoid = str_remove(geography, ".*US"))
```

    ## New names:
    ## Rows: 2327 Columns: 375
    ## ── Column specification
    ## ──────── Delimiter: "," chr
    ## (146): Geography, Geographic
    ## Area Name,
    ## Estimate!!Total!!UNRELATED
    ## INDIV... dbl (228):
    ## Estimate!!Total!!Population for
    ## whom poverty status is
    ## determined... lgl (1): ...375
    ## ℹ Use `spec()` to retrieve the
    ## full column specification for
    ## this data. ℹ Specify the column
    ## types or set `show_col_types =
    ## FALSE` to quiet this message.
    ## • `` -> `...375`

## NYC census demographic

``` r
demo = 
  read_excel("./Datasets/NYC_census_core_data.xlsx")|>
  janitor::clean_names()|>
  filter(geo_type == "CT2020")|>
  rename(geoid = geo_id)
```

## healthy store data

``` r
nyc_healthy_store = 
  read_csv("./Datasets/Recognized_Shop_Healthy_Stores.csv")|>
  janitor::clean_names()|>
  distinct(bin, .keep_all = TRUE)|>
  mutate(fipcode = case_when(
    borough == "Bronx" ~ "36005",
    borough == "Brooklyn" ~ "36047",
    borough == "New York" ~ "36061"
  ),
  ct_label = case_when(
    census_tract_2020 < 100 ~ paste0("00", census_tract_2020, "00"),
    census_tract_2020 < 1000 ~ paste0("0", census_tract_2020, "00"),
    census_tract_2020 < 10000 ~ paste0(census_tract_2020, "00"),
    census_tract_2020 < 100000 ~ paste0(census_tract_2020, "0")
  ))|>
  select(store_name,borough,zip_code,latitude,longitude,fipcode, ct_label)|>
  filter(!is.na(ct_label))|>
  mutate(geoid = paste(fipcode, ct_label, sep = ""))|>
  select(-fipcode, -ct_label)
```

    ## Rows: 675 Columns: 15
    ## ── Column specification ────────
    ## Delimiter: ","
    ## chr  (4): Store Name, Street Address, Borough, Neighborhood Tabulation Area ...
    ## dbl (10): Zip 
    ## Code, Year Awarded, Program 
    ## Wave, Latitude, Longitude, Com...
    ## lgl  (1): Address
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## redlining data

``` r
redlining = 
  read_excel("./Datasets/Historic Redlining Score 2020B.xlsx")|>
  janitor::clean_names()|>
  mutate(fip = substr(geoid20, 1, 5))|>
  filter(fip %in% c("36005", "36047", "36061", "36081", "36085"))|>
  rename(geoid = geoid20)
```

## PLACE data

``` r
place =
  read_csv("./Datasets/PLACES.csv")|>
  janitor::clean_names()|>
  rename(geoid = location_id)
```

    ## Rows: 73621 Columns: 25
    ## ── Column specification ────────
    ## Delimiter: ","
    ## chr (13): StateAbbr, StateDesc, CountyName, DataSource, Category, Measure, D...
    ## dbl (10): Year, CountyFIPS, LocationName, Data_Value, Low_Confidence_Limit, ...
    ## lgl  (2): Data_Value_Footnote_Symbol, Data_Value_Footnote
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
