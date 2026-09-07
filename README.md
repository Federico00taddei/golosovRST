# golosovRST

<!-- Optional logo -->
<!-- ![logo](man/figures/logo.png) -->

## Overview

`golosovRST` is an R package for **Golosov Relative Size Triangle (RST)** classification and plotting.  
It computes RST coordinates, classifies party-system observations into Golosov subtypes using polygon mapping, and produces publication-ready RST plots.

## Contents

- `.validate_rst_input()`
- `plot_rst()`
- `rst_classify()`
- `rst_coords()`

## Package metadata

- **Type:** Package  
- **Title:** Golosov Relative Size Triangle Classification and Plotting  
- **Version:** 0.0.1  
- **License:** MIT + file LICENSE  
- **Encoding:** UTF-8  
- **Imports:** ggplot2, rlang  
- **Suggests:** sf, testthat (>= 3.0.0), dplyr  
- **NeedsCompilation:** no  
- **Author/Maintainer:** Federico Taddei (<federico.taddei@unimi.it>)  
- **Repository:** CRAN  
- **Date/Publication:** 2026-09-03 11:50:37 UTC  

## Installation

```r
install.packages("golosovRST")
```

## Functions

### `.validate_rst_input(data)`

Validate input for Golosov RST functions.

### `plot_rst(data, label_col = NULL)`

Plot Golosov Relative Size Triangle.

**Arguments**
- `data`: A `data.frame` containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.
- `label_col`: Optional character scalar: name of a column used as point labels.

**Value**
- A `ggplot2` object.

### `rst_classify(data, offset = 5e-16)`

Classify observations in Golosov subtypes using RST polygons.

**Arguments**
- `data`: A `data.frame` containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.
- `offset`: Small numeric offset used to avoid polygon-edge ambiguity.

**Value**
- A `data.frame` with coordinates and `golosov_subtype`.

### `rst_coords(data)`

Compute Golosov relative size coordinates.

**Arguments**
- `data`: A `data.frame` containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.

**Value**
- A `data.frame` with original columns plus `sr`, `x`, and `y`.

## Example

```r
library(golosovRST)

mydata <- data.frame(
  country = c("uk 2015", "ita 2013", "spa 2015"),
  s1 = c(0.508, 0.4714, 0.3514),
  s2 = c(0.357, 0.1730, 0.2571),
  s3 = c(0.086, 0.1556, 0.1971)
)

plot_rst(mydata)
plot_rst(mydata, label_col = "country")
rst_classify(mydata)
rst_coords(mydata)
```

## References

- Golosov, G. V. (2011). *Party system classification: A methodological inquiry*. Party Politics, 17(5), 539–560. https://doi.org/10.1177/1354068810377189  
- Golosov, G. V. (2014). *Towards a classification of the world's democratic party systems, step 2: Placing the units into categories*. Party Politics, 20(1), 3–15.

## Documentation

- CRAN package page: https://CRAN.R-project.org/package=golosovRST  
- Full reference manual (HTML): https://cran.r-project.org/web/packages/golosovRST/refman/golosovRST.html  
- Reference manual (PDF): https://cran.r-project.org/web/packages/golosovRST/golosovRST.pdf  

## DOI

https://doi.org/10.32614/CRAN.package.golosovRST
