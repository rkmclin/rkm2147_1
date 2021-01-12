ML assignment 1
================
Ronae McLin rkm2147
1/12/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(haven)
library(table1)
```

    ## 
    ## Attaching package: 'table1'

    ## The following objects are masked from 'package:base':
    ## 
    ##     units, units<-

read in the file

``` r
data_df = read_csv("data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Age = col_double(),
    ##   BMI = col_double(),
    ##   Glucose = col_double(),
    ##   Insulin = col_double(),
    ##   HOMA = col_double(),
    ##   Leptin = col_double(),
    ##   Adiponectin = col_double(),
    ##   Resistin = col_double(),
    ##   MCP.1 = col_double(),
    ##   Classification = col_double()
    ## )

clean data

``` r
data_df = data_df %>% janitor::clean_names()
```

1.  Construct a table providing summaries of the quantitative features
    of the dataset.Summaries should include the mean, median, minimum
    value, and maximum value. If you are unable to construct a formatted
    table within R, you can print raw output, but then comment the
    output to identify the answer that was requested.

**classification variable was not included in this table** \#no missing
data or values to omit

``` r
table_data = 
table1(~ age + bmi + glucose + insulin + homa + leptin + adiponectin + resistin + mcp_1, data = data_df)
```

``` r
table_data
```

    ## [1] "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=116)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>age</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>57.3 (16.1)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>56.0 [24.0, 89.0]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>bmi</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>27.6 (5.02)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>27.7 [18.4, 38.6]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>glucose</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>97.8 (22.5)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>92.0 [60.0, 201]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>insulin</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>10.0 (10.1)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>5.92 [2.43, 58.5]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>homa</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>2.69 (3.64)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>1.38 [0.467, 25.1]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>leptin</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>26.6 (19.2)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>20.3 [4.31, 90.3]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>adiponectin</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>10.2 (6.84)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>8.35 [1.66, 38.0]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>resistin</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>14.7 (12.4)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>10.8 [3.21, 82.1]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>mcp_1</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>535 (346)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>471 [45.8, 1700]</td>\n</tr>\n</tbody>\n</table>\n"

2.  Recode BMI into the WHO-defined categories below Severely
    underweight - BMI less than 16.5kg/m^2 Underweight - BMI under 18.5
    kg/m^2 Normal weight - BMI greater than or equal to 18.5 to 24.9
    kg/m^2 Overweight – BMI greater than or equal to 25 to 29.9 kg/m^2
    Obesity class I – BMI 30 to 34.9 kg/m^2 Obesity class II – BMI 35 to
    39.9 kg/m^2 Obesity class III – BMI greater than or equal to 40
    kg/m^2

3.Create a bar chart showing the proportion of breast cancer cases and
controls within each BMI category

4.  Construct a logistic regression model using breast cancer
    classification as the outcome and glucose, HOMA, leptin, BMI
    (continuous) and age as the independent variables. Fill in the beta
    estimate and 95% confidence interval associated with a 1-unit change
    in HOMA

5.Construct a linear regression model using insulin as the outcome and
BMI (continuous), age, and glucose as the independent variables. Fill in
the beta estimate and 95% confidence interval associated with a 1-unit
change in age.
