
# snvtyping

<!-- badges: start -->
[![R-CMD-check](https://github.com/MorfeoRenai/snvtyping/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MorfeoRenai/snvtyping/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `snvtyping` is to preprocess and analyze VCF files containing SNV
mutations, by getting the genomic context of all of its Single Nucleotide
Variants mutations. It can also classify group them by type of SNV, by type of
context and then plot their frequency.

## Installation

You can install the development version of snvtyping from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MorfeoRenai/snvtyping")
```

## Usage

Check out the [vignette in GitHub](vignettes/snvtyping-basicUsage.Rmd).

You can also read and run the vignette with:

```r
vignette("snvtyping")
```
