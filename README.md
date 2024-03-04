
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIAModuleIRR2FPR

<!-- badges: start -->
<!-- badges: end -->

The goal of SIAModuleIRR2FPR is to …

## Installation

You can install the development version of SIAModuleIRR2FPR like so:

``` r
devtools::install_github("FBartos/SIAModuleIRR2FPR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SIAModuleIRR2FPR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
IRR <- spearman_brown_formula(0.34, 2.79)
compute_true_positive_rate(IRR, 0.18)
#> [1] 0.6027441
compute_false_positive_rate(IRR, 0.18)
#> [1] 0.3972559
compute_false_negative_rate(IRR, 0.18)
#> [1] 0.08720251
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
