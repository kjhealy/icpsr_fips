FIPS / ICPSR
================

## Setup

``` r
knitr::opts_chunk$set(echo = TRUE)
  
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x readr::edition_get()   masks testthat::edition_get()
    ## x dplyr::filter()        masks stats::filter()
    ## x purrr::is_null()       masks testthat::is_null()
    ## x dplyr::lag()           masks stats::lag()
    ## x readr::local_edition() masks testthat::local_edition()
    ## x dplyr::matches()       masks tidyr::matches(), testthat::matches()

We have two tables, one with ICPSR codes and one with FIPS codes:

### ICPSR Table

``` r
## ICPSR Excel table from https://usa.ipums.org/usa/volii/ICPSR.shtml
icpsr <- read_csv("data/icpsrcnt.csv") %>% 
  janitor::clean_names() %>% 
  rename(icp_county = county, 
         icp_state = state, 
         icp_statefips = statefips, 
         icp_statecode = stateicp, 
         icp_county_cod = county_cod) %>% 
  select(icp_state, icp_county, everything())
```

    ## Rows: 3259 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): State, County
    ## dbl (3): STATEICP, STATEFIPS, County cod
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
## ICPSR Table
icpsr
```

    ## # A tibble: 3,259 × 5
    ##    icp_state icp_county     icp_statecode icp_statefips icp_county_cod
    ##    <chr>     <chr>                  <dbl>         <dbl>          <dbl>
    ##  1 Alabama   Autauga                   41             1             10
    ##  2 Alabama   Baldwin                   41             1             30
    ##  3 Alabama   Barbour                   41             1             50
    ##  4 Alabama   Bibb                      41             1             70
    ##  5 Alabama   Blount                    41             1             90
    ##  6 Alabama   Bullock                   41             1            110
    ##  7 Alabama   Butler                    41             1            130
    ##  8 Alabama   Calhoun/Benton            41             1            150
    ##  9 Alabama   Chambers                  41             1            170
    ## 10 Alabama   Cherokee                  41             1            190
    ## # … with 3,249 more rows

### FIPS table

This is from my own repo. This may not be up to date! Can get more
current list from the Census

``` r
fips <- read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv")
```

    ## Rows: 3146 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): county_name, state_abbr, state_name, long_name, crosswalk, region_n...
    ## dbl (6): fips, sumlev, region, division, state, county
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
## FIPS table
fips
```

    ## # A tibble: 3,146 × 13
    ##     fips county_name     state_abbr state_name long_name  sumlev region division
    ##    <dbl> <chr>           <chr>      <chr>      <chr>       <dbl>  <dbl>    <dbl>
    ##  1  1001 Autauga County  AL         Alabama    Autauga C…     50      3        6
    ##  2  1003 Baldwin County  AL         Alabama    Baldwin C…     50      3        6
    ##  3  1005 Barbour County  AL         Alabama    Barbour C…     50      3        6
    ##  4  1007 Bibb County     AL         Alabama    Bibb Coun…     50      3        6
    ##  5  1009 Blount County   AL         Alabama    Blount Co…     50      3        6
    ##  6  1011 Bullock County  AL         Alabama    Bullock C…     50      3        6
    ##  7  1013 Butler County   AL         Alabama    Butler Co…     50      3        6
    ##  8  1015 Calhoun County  AL         Alabama    Calhoun C…     50      3        6
    ##  9  1017 Chambers County AL         Alabama    Chambers …     50      3        6
    ## 10  1019 Cherokee County AL         Alabama    Cherokee …     50      3        6
    ## # … with 3,136 more rows, and 5 more variables: state <dbl>, county <dbl>,
    ## #   crosswalk <chr>, region_name <chr>, division_name <chr>

## Cleanup

We need to merge the tables but we don’t have fully shared ID codes
between them. We can try using the combination of State name and County
name. The difficulty is that the counties (also e.g. parishes \[in LA\]
and cities \[in VA\] etc) have the name “County” etc appended in one
table but not the other. We’ll strip the last word of the `county_name`
column in the `fips` table to make them roughly comparable. This won’t
be perfect, though.

``` r
end_words <- c("Borough", "County", "Area", "Municipality", 
               "Columbia", "Parish", "city", "City")

## A regular expression to match them
end_words_re <- paste0(end_words, collapse = "|")
```

Now we add a column to `fips` that’s the “county” names but with end
word deleted.

``` r
fips <- fips %>% 
  mutate(county_clipped = str_remove(county_name, end_words_re), 
         county_clipped = str_squish(county_clipped)) %>% 
  select(fips, county_name, state_abbr, state, county_clipped, everything())

## Table
fips
```

    ## # A tibble: 3,146 × 14
    ##     fips county_name state_abbr state county_clipped state_name long_name sumlev
    ##    <dbl> <chr>       <chr>      <dbl> <chr>          <chr>      <chr>      <dbl>
    ##  1  1001 Autauga Co… AL             1 Autauga        Alabama    Autauga …     50
    ##  2  1003 Baldwin Co… AL             1 Baldwin        Alabama    Baldwin …     50
    ##  3  1005 Barbour Co… AL             1 Barbour        Alabama    Barbour …     50
    ##  4  1007 Bibb County AL             1 Bibb           Alabama    Bibb Cou…     50
    ##  5  1009 Blount Cou… AL             1 Blount         Alabama    Blount C…     50
    ##  6  1011 Bullock Co… AL             1 Bullock        Alabama    Bullock …     50
    ##  7  1013 Butler Cou… AL             1 Butler         Alabama    Butler C…     50
    ##  8  1015 Calhoun Co… AL             1 Calhoun        Alabama    Calhoun …     50
    ##  9  1017 Chambers C… AL             1 Chambers       Alabama    Chambers…     50
    ## 10  1019 Cherokee C… AL             1 Cherokee       Alabama    Cherokee…     50
    ## # … with 3,136 more rows, and 6 more variables: region <dbl>, division <dbl>,
    ## #   county <dbl>, crosswalk <chr>, region_name <chr>, division_name <chr>

## Joining the tables

### 1. Left-join `icpsr` table to `fips` table

If we put `icpsr` on the left side of the join, this will preserve all
rows in `icpsr` but any rows in `fips` not matched in `icpsr` won’t be
merged.

``` r
## Result: 3,265 x 19
icpsr_ljoin_fips <- icpsr %>% 
  left_join(fips, by = c("icp_state" = "state_name", 
                         "icp_county" = "county_clipped"), 
            keep = TRUE)

write_csv(icpsr_ljoin_fips, file = "data/icpsr_ljoin_fips.csv")


icpsr_ljoin_fips
```

    ## # A tibble: 3,265 × 19
    ##    icp_state icp_county     icp_statecode icp_statefips icp_county_cod  fips
    ##    <chr>     <chr>                  <dbl>         <dbl>          <dbl> <dbl>
    ##  1 Alabama   Autauga                   41             1             10  1001
    ##  2 Alabama   Baldwin                   41             1             30  1003
    ##  3 Alabama   Barbour                   41             1             50  1005
    ##  4 Alabama   Bibb                      41             1             70  1007
    ##  5 Alabama   Blount                    41             1             90  1009
    ##  6 Alabama   Bullock                   41             1            110  1011
    ##  7 Alabama   Butler                    41             1            130  1013
    ##  8 Alabama   Calhoun/Benton            41             1            150    NA
    ##  9 Alabama   Chambers                  41             1            170  1017
    ## 10 Alabama   Cherokee                  41             1            190  1019
    ## # … with 3,255 more rows, and 13 more variables: county_name <chr>,
    ## #   state_abbr <chr>, state <dbl>, county_clipped <chr>, state_name <chr>,
    ## #   long_name <chr>, sumlev <dbl>, region <dbl>, division <dbl>, county <dbl>,
    ## #   crosswalk <chr>, region_name <chr>, division_name <chr>

What’s missing in this table after the merge? If FIPS is missing, the
county didn’t exist in the original `fips` table.

``` r
## 279 rows missing
icpsr_nofips <- icpsr_ljoin_fips %>% 
  filter(is.na(fips)) 

icpsr_nofips
```

    ## # A tibble: 279 × 19
    ##    icp_state icp_county         icp_statecode icp_statefips icp_county_cod  fips
    ##    <chr>     <chr>                      <dbl>         <dbl>          <dbl> <dbl>
    ##  1 Alabama   Calhoun/Benton                41             1            150    NA
    ##  2 Alabama   Chilton/Baker                 41             1            210    NA
    ##  3 Alabama   Lamar/Sanford                 41             1            750    NA
    ##  4 Alabama   Morgan/Cotaco                 41             1           1030    NA
    ##  5 Alabama   St Clair                      41             1           1150    NA
    ##  6 Alabama   Winston/Hancock               41             1           1330    NA
    ##  7 Alaska    Northern District             81             2           2900    NA
    ##  8 Alaska    Southern District             81             2           2950    NA
    ##  9 Alaska    First Judicial Di…            81             2           3000    NA
    ## 10 Alaska    Second Judicial D…            81             2           3100    NA
    ## # … with 269 more rows, and 13 more variables: county_name <chr>,
    ## #   state_abbr <chr>, state <dbl>, county_clipped <chr>, state_name <chr>,
    ## #   long_name <chr>, sumlev <dbl>, region <dbl>, division <dbl>, county <dbl>,
    ## #   crosswalk <chr>, region_name <chr>, division_name <chr>

``` r
write_csv(file = "data/icpsr_nofips.csv", icpsr_nofips)
```

### 2. Left join `fips` to `icpsr`

If we put `fips` on the left side of the join, this will preserve all
rows in `fips`, but any rows in `icpsr` not matched in `fips` won’t be
merged.

``` r
# Result: 3,146 × 19
fips_ljoin_icpsr <- fips %>% 
  left_join(icpsr, by = c("state_name" = "icp_state", 
                           "county_clipped" = "icp_county"), 
             keep = TRUE)

write_csv(fips_ljoin_icpsr, file = "data/fips_ljoin_icpsr.csv")

fips_ljoin_icpsr
```

    ## # A tibble: 3,146 × 19
    ##     fips county_name state_abbr state county_clipped state_name long_name sumlev
    ##    <dbl> <chr>       <chr>      <dbl> <chr>          <chr>      <chr>      <dbl>
    ##  1  1001 Autauga Co… AL             1 Autauga        Alabama    Autauga …     50
    ##  2  1003 Baldwin Co… AL             1 Baldwin        Alabama    Baldwin …     50
    ##  3  1005 Barbour Co… AL             1 Barbour        Alabama    Barbour …     50
    ##  4  1007 Bibb County AL             1 Bibb           Alabama    Bibb Cou…     50
    ##  5  1009 Blount Cou… AL             1 Blount         Alabama    Blount C…     50
    ##  6  1011 Bullock Co… AL             1 Bullock        Alabama    Bullock …     50
    ##  7  1013 Butler Cou… AL             1 Butler         Alabama    Butler C…     50
    ##  8  1015 Calhoun Co… AL             1 Calhoun        Alabama    Calhoun …     50
    ##  9  1017 Chambers C… AL             1 Chambers       Alabama    Chambers…     50
    ## 10  1019 Cherokee C… AL             1 Cherokee       Alabama    Cherokee…     50
    ## # … with 3,136 more rows, and 11 more variables: region <dbl>, division <dbl>,
    ## #   county <dbl>, crosswalk <chr>, region_name <chr>, division_name <chr>,
    ## #   icp_state <chr>, icp_county <chr>, icp_statecode <dbl>,
    ## #   icp_statefips <dbl>, icp_county_cod <dbl>

What’s missing after merging this way? If the `icp_county_cod` is
missing, the county didn’t exist in the original `icpsr` table.

``` r
### 160 rows missing
missing_icpcod <- fips_ljoin_icpsr %>% 
  filter(is.na(icp_county_cod)) 

missing_icpcod
```

    ## # A tibble: 160 × 19
    ##     fips county_name state_abbr state county_clipped state_name long_name sumlev
    ##    <dbl> <chr>       <chr>      <dbl> <chr>          <chr>      <chr>      <dbl>
    ##  1  1015 Calhoun Co… AL             1 Calhoun        Alabama    Calhoun …     50
    ##  2  1021 Chilton Co… AL             1 Chilton        Alabama    Chilton …     50
    ##  3  1075 Lamar Coun… AL             1 Lamar          Alabama    Lamar Co…     50
    ##  4  1103 Morgan Cou… AL             1 Morgan         Alabama    Morgan C…     50
    ##  5  1115 St. Clair … AL             1 St. Clair      Alabama    St. Clai…     50
    ##  6  1133 Winston Co… AL             1 Winston        Alabama    Winston …     50
    ##  7  2013 Aleutians … AK             2 Aleutians East Alaska     Aleutian…     50
    ##  8  2016 Aleutians … AK             2 Aleutians Wes… Alaska     Aleutian…     50
    ##  9  2020 Anchorage … AK             2 Anchorage      Alaska     Anchorag…     50
    ## 10  2050 Bethel Cen… AK             2 Bethel Census  Alaska     Bethel C…     50
    ## # … with 150 more rows, and 11 more variables: region <dbl>, division <dbl>,
    ## #   county <dbl>, crosswalk <chr>, region_name <chr>, division_name <chr>,
    ## #   icp_state <chr>, icp_county <chr>, icp_statecode <dbl>,
    ## #   icp_statefips <dbl>, icp_county_cod <dbl>

``` r
write_csv(missing_icpcod, file = "data/missing_icp_cod.csv")
```

### 3. Full join

This is the union of the tables, preserving all rows in both tables,
even if one isn’t present in the other table.

``` r
## Result: 3,425 x 19
county_fulljoin <- icpsr %>% 
  full_join(fips, by = c("icp_state" = "state_name", 
                                "icp_county" = "county_clipped"), 
            keep = TRUE)

write_csv(county_fulljoin, file = "data/county_fulljoin.csv")

county_fulljoin
```

    ## # A tibble: 3,425 × 19
    ##    icp_state icp_county     icp_statecode icp_statefips icp_county_cod  fips
    ##    <chr>     <chr>                  <dbl>         <dbl>          <dbl> <dbl>
    ##  1 Alabama   Autauga                   41             1             10  1001
    ##  2 Alabama   Baldwin                   41             1             30  1003
    ##  3 Alabama   Barbour                   41             1             50  1005
    ##  4 Alabama   Bibb                      41             1             70  1007
    ##  5 Alabama   Blount                    41             1             90  1009
    ##  6 Alabama   Bullock                   41             1            110  1011
    ##  7 Alabama   Butler                    41             1            130  1013
    ##  8 Alabama   Calhoun/Benton            41             1            150    NA
    ##  9 Alabama   Chambers                  41             1            170  1017
    ## 10 Alabama   Cherokee                  41             1            190  1019
    ## # … with 3,415 more rows, and 13 more variables: county_name <chr>,
    ## #   state_abbr <chr>, state <dbl>, county_clipped <chr>, state_name <chr>,
    ## #   long_name <chr>, sumlev <dbl>, region <dbl>, division <dbl>, county <dbl>,
    ## #   crosswalk <chr>, region_name <chr>, division_name <chr>

The missings from both the original tables are in this table.
