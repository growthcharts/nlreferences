
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlreferences

<!-- badges: start -->

[![R-CMD-check](https://github.com/growthcharts/nlreferences/workflows/R-CMD-check/badge.svg)](https://github.com/growthcharts/nlreferences/actions)
[![R-CMD-check](https://github.com/growthcharts/nlreferences/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/growthcharts/nlreferences/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The nlreferences package provides Dutch reference values.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("growthcharts/nlreferences")
```

## Example

Suppose you want to calculate Z-scores for waist circumference for Dutch
boys and girls using the references published in Fredriks AM et al
(2005).

The calculation requires two R packages: `nlreferences` (with the
reference data) and `centile` (with the calculation method). Install
both. You only need to do this once.

``` r
install.packages("remotes")
remotes::install_github("growthcharts/nlreferences")
remotes::install_github("growthcharts/centile")
```

The raw reference coordinates for boys and girls can be found in the
`nlreferences` package as files
`data-raw/data/nl1997/nl_1997_wst_male_.txt` and
`data-raw/data/nl1997/nl_1997_wst_female_.txt`. See
<https://github.com/growthcharts/nlreferences/tree/master/data-raw/data/nl1997>.
To calculate Z-score for waist circumference, you need their file names
without the path and extension.

``` r
refs <- c("nl_1997_wst_male_", "nl_1997_wst_female_")
```

Next you need your data, in the long format, and

``` r
data <- data.frame(
  age = c(0.5, 10, 18, 14, 12),
  sex = c("male", "male", "female", "female", "male"),
  y = c(42.037, 60, 70, NA, 70)
)
data
#>    age    sex  y
#> 1  0.5   male 42
#> 2 10.0   male 60
#> 3 18.0 female 70
#> 4 14.0 female NA
#> 5 12.0   male 70
```

Load the packages, and the run the `y2z()` function. Specify for each
record the reference that you want to use, and convert measurement (`y`)
into Z-score (`z`).

``` r
library(centile)
library(nlreferences)

data$ref <- ifelse(data$sex == "male", refs[1], refs[2])
data$z <- y2z(y = data$y, x = data$age, refcode = data$ref, pkg = "nlreferences", verbose = TRUE)
```

And presto, your data with two extra columns `ref` and `z`.

``` r
data
#>    age    sex  y                 ref     z
#> 1  0.5   male 42   nl_1997_wst_male_ 0.000
#> 2 10.0   male 60   nl_1997_wst_male_ 0.019
#> 3 18.0 female 70 nl_1997_wst_female_ 0.080
#> 4 14.0 female NA nl_1997_wst_female_    NA
#> 5 12.0   male 70   nl_1997_wst_male_ 0.953
```

## Literature

- Fredriks AM, van Buuren S, Fekkes M, Verloove-Vanhorick SP & Wit
  JM (2005) Are age references for waist circumference, hip
  circumference and waist-hip ratio in Dutch children useful in clinical
  practice? European Journal of Pediatrics, 164, 216-222; Fredriks AM,
  van Buuren S, van Heel WJM, Dijkman-Neerincx RHM, Verloove-Vanhorick
  SP & Wit JM (2005) Nationwide age references for sitting height, leg
  length, and sitting height/height ratio, and their diagnostic value
  for disproportionate growth disorders. Arch. Dis. Child., 90, 807-812.
