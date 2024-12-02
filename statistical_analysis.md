Statistical Analysis
================

import the datasets and packages

### Desciptive Characteristics based on redlining grades

``` r
redlining_quartile=
final_analysis |>
  group_by(red_grade) |>
  summarise(
    avg_snap = mean(ph_snap, na.rm = TRUE),
    sd_snap = sd(ph_snap, na.rm = TRUE),
    avg_poverty = mean(ph_below_poverty, na.rm = TRUE),
    sd_poverty = sd(ph_below_poverty, na.rm = TRUE),
    avg_disability = mean(ph_disability, na.rm = TRUE),
    sd_disability = sd(ph_disability, na.rm = TRUE),
    avg_white = mean(ph_white, na.rm = TRUE),
    sd_white = sd(ph_white, na.rm = TRUE),
    avg_black = mean(ph_black, na.rm = TRUE),
    sd_black = sd(ph_black, na.rm = TRUE),
    avg_asian = mean(ph_asian, na.rm = TRUE),
    sd_asian = sd(ph_asian, na.rm = TRUE),
    avg_otherrace = mean(ph_other_race, na.rm = TRUE),
    sd_otherrace = sd(ph_other_race, na.rm = TRUE),
    avg_hispanic = mean(ph_hispanic, na.rm = TRUE),
    sd_hispanic = sd(ph_hispanic, na.rm = TRUE),
    avg_nowork = mean(ph_no_work, na.rm = TRUE),
    sd_nowork = sd(ph_no_work, na.rm = TRUE),
    avg_1work = mean(ph_1_work, na.rm = TRUE),
    sd_1work = sd(ph_1_work, na.rm = TRUE),
    avg_2work = mean(ph_2_work, na.rm = TRUE),
    sd_2work = sd(ph_2_work, na.rm = TRUE),
    avg_obesity = mean(obesity, na.rm = TRUE),
    sd_obesity = sd(obesity, na.rm = TRUE),
    avg_highchol = mean(highchol, na.rm = TRUE),
    sd_highchol = sd(highchol, na.rm = TRUE),
    avg_diabetes = mean(diabetes, na.rm = TRUE),
    sd_diabetes = sd(diabetes, na.rm = TRUE)
  )|> pivot_longer(
    cols = -red_grade,
    names_to = c("stat", "variable"),
    names_sep = "_"
  )|>
  pivot_wider(
    names_from = c(red_grade, stat), 
    values_from = value,              
    names_glue = "{red_grade}_{stat}"
  )

redlining_quartile|>
  knitr::kable(digits = 2)
```

| variable   | A_avg |  A_sd | B_avg |  B_sd | C_avg |  C_sd | D_avg |  D_sd |
|:-----------|------:|------:|------:|------:|------:|------:|------:|------:|
| snap       |  5.72 |  6.95 | 17.25 | 13.61 | 18.63 | 13.40 | 23.69 | 17.85 |
| poverty    |  7.58 |  6.03 | 14.18 |  8.67 | 15.56 | 10.41 | 20.56 | 13.11 |
| disability | 19.56 |  8.20 | 23.13 |  8.85 | 24.37 |  9.72 | 23.86 | 11.45 |
| white      | 74.51 | 23.05 | 46.19 | 28.67 | 36.94 | 27.73 | 36.87 | 28.20 |
| black      |  6.48 | 14.06 | 23.18 | 28.43 | 22.46 | 27.85 | 29.81 | 27.75 |
| asian      |  8.45 |  7.09 | 11.82 | 13.31 | 16.68 | 18.27 |  9.95 | 13.31 |
| otherrace  | 10.56 |  9.86 | 18.81 | 17.89 | 23.92 | 16.65 | 23.36 | 17.18 |
| hispanic   | 12.72 | 12.03 | 21.39 | 22.91 | 27.21 | 21.05 | 28.42 | 23.08 |
| nowork     | 17.45 |  7.60 | 12.46 |  7.18 | 12.22 |  7.23 | 14.00 | 10.12 |
| 1work      | 31.11 |  9.24 | 32.87 |  9.70 | 32.61 | 10.13 | 34.33 | 12.26 |
| 2work      | 51.46 | 12.88 | 54.67 | 10.61 | 55.17 | 11.89 | 51.67 | 15.78 |
| obesity    | 20.74 |  5.61 | 25.92 |  6.18 | 27.51 |  6.37 | 28.71 |  7.61 |
| highchol   | 37.27 |  3.24 | 34.50 |  3.30 | 34.14 |  3.05 | 32.29 |  4.20 |
| diabetes   |  9.25 |  2.48 | 11.11 |  2.91 | 12.36 |  2.92 | 11.58 |  4.42 |

``` r
final_analysis |>
  group_by(red_grade)|>
  summarise(count = n())|>
   knitr::kable()
```

| red_grade | count |
|:----------|------:|
| A         |    36 |
| B         |   352 |
| C         |  1003 |
| D         |   642 |

### Comparing census tracts with HRS in the first and fourth quartile

``` r
q1_vs_q4 = 
final_analysis|>
  filter(red_grade %in%c("A","D"))

q1q4_desc = 
q1_vs_q4|>
  group_by(red_grade)|>
  summarise(
    ph_snap = mean(ph_snap),
    ph_below_poverty = mean(ph_below_poverty),
    ph_disability = mean(ph_disability),
    minority = mean(minority),
    obesity = mean(obesity),
    highchol = mean(highchol),
    diabetes = mean(diabetes)
  )|>
 pivot_longer(cols = -red_grade, names_to = "variable", values_to = "value")|>
 pivot_wider(names_from = red_grade, names_prefix = "grade_", values_from = value)|>
 mutate(
    mean_diff = grade_D - grade_A,
  )|>
  rename(outcome = variable)
```

``` r
uni_reg = function(dataset, outcomes, predictor) {
  results = list() 
  for (outcome in outcomes) {
    formula = as.formula(paste(outcome, "~", predictor))
    model= lm(
      formula,
      data = dataset
    )
    results[[outcome]] = broom::tidy(model)
  }
  
  combined_results = bind_rows(results, .id = "outcome")
  return(combined_results)
}

outcomes = c("ph_snap","ph_below_poverty", "ph_disability", "minority", "obesity", "highchol", "diabetes")


uni_reg(q1_vs_q4, outcomes, "red_grade")|>
  filter(term != "(Intercept)")|>
  right_join(q1q4_desc, by="outcome")|>
  select(outcome, grade_A, grade_D, mean_diff, p.value)|>
  knitr::kable(digits = 3)
```

| outcome          | grade_A | grade_D | mean_diff | p.value |
|:-----------------|--------:|--------:|----------:|--------:|
| ph_snap          |   5.722 |  23.695 |    17.972 |   0.000 |
| ph_below_poverty |   7.581 |  20.559 |    12.978 |   0.000 |
| ph_disability    |  19.556 |  23.863 |     4.307 |   0.026 |
| minority         |  25.494 |  63.126 |    37.632 |   0.000 |
| obesity          |  20.736 |  28.710 |     7.974 |   0.000 |
| highchol         |  37.275 |  32.286 |    -4.989 |   0.000 |
| diabetes         |   9.247 |  11.584 |     2.337 |   0.002 |

### regression analysis for hrs and the relevant variables

``` r
# univariate analysis
lm(ph_snap~red_grade,final_analysis)|>
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     5.72      2.48      2.30 2.14e- 2
    ## 2 red_gradeB     11.5       2.61      4.42 1.04e- 5
    ## 3 red_gradeC     12.9       2.53      5.10 3.65e- 7
    ## 4 red_gradeD     18.0       2.55      7.04 2.62e-12

``` r
## multivariate model 1
lm(ph_snap~red_grade+minority+ph_disability,final_analysis)|>
  broom::tidy()|>
  knitr::kable(digits = 3)
```

| term          | estimate | std.error | statistic | p.value |
|:--------------|---------:|----------:|----------:|--------:|
| (Intercept)   |  -11.575 |     2.004 |    -5.776 |   0.000 |
| red_gradeB    |    4.778 |     2.053 |     2.327 |   0.020 |
| red_gradeC    |    3.904 |     2.005 |     1.948 |   0.052 |
| red_gradeD    |    9.313 |     2.024 |     4.602 |   0.000 |
| minority      |    0.151 |     0.010 |    15.386 |   0.000 |
| ph_disability |    0.687 |     0.027 |    25.294 |   0.000 |

``` r
##multivariate model 2
lm(ph_snap~red_grade+ph_disability+ph_white+ph_black+ph_asian+ph_1_work+ph_2_work,final_analysis)|>
  broom::tidy()|>
  knitr::kable(digits = 3)
```

| term          | estimate | std.error | statistic | p.value |
|:--------------|---------:|----------:|----------:|--------:|
| (Intercept)   |   42.463 |     3.602 |    11.789 |   0.000 |
| red_gradeB    |    6.636 |     1.832 |     3.622 |   0.000 |
| red_gradeC    |    5.801 |     1.794 |     3.233 |   0.001 |
| red_gradeD    |    9.792 |     1.802 |     5.434 |   0.000 |
| ph_disability |    0.438 |     0.027 |    16.170 |   0.000 |
| ph_white      |   -0.326 |     0.015 |   -21.816 |   0.000 |
| ph_black      |   -0.234 |     0.016 |   -14.755 |   0.000 |
| ph_asian      |   -0.279 |     0.020 |   -13.848 |   0.000 |
| ph_1_work     |   -0.011 |     0.035 |    -0.314 |   0.754 |
| ph_2_work     |   -0.326 |     0.030 |   -10.891 |   0.000 |
