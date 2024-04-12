
<!-- README.md is generated from README.Rmd. Please edit that file. -->
<!-- The code to render this README is stored in .github/workflows/render-readme.yaml -->
<!-- Variables marked with double curly braces will be transformed beforehand: -->
<!-- `packagename` is extracted from the DESCRIPTION file -->
<!-- `gh_repo` is extracted via a special environment variable in GitHub Actions -->

# contactmatrix <img src="man/figures/logo.svg" align="right" width="120" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![R-CMD-check](https://github.com/socialcontactdata/contactmatrix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/socialcontactdata/contactmatrix/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/socialcontactdata/contactmatrix/branch/main/graph/badge.svg)](https://app.codecov.io/gh/socialcontactdata/contactmatrix?branch=main)
[![lifecycle-concept](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

contactmatrix provides standard classes and methods to work with social
contact matrices in R.

This is an attempt at providing a common framework for social contact
data in R, to facilitate interoperability between different data sources
and analysis tools.

It results from the collaboration between multiple stakeholders:

- the [‘Social Contact Data’ Zenodo community maintainers and
  curators](https://zenodo.org/communities/social_contact_data/)
- the [conmat R package](https://idem-lab.github.io/conmat/) maintainers
- the [contactdata R
  package](https://cran.r-project.org/package=contactdata) maintainers
- the [socialmixr R
  package](https://cran.r-project.org/package=socialmixr) maintainers

## Installation

You can install the development version of contactmatrix from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("socialcontactdata/contactmatrix")
```

## Development

### Lifecycle

This package is currently *experimental*, as defined by the [tidyverse
lifecycle](https://lifecycle.r-lib.org/articles/stages.html). This means
that it is made available so people can try it out and provide feedback,
but comes with no promises for long term stability.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/socialcontactdata/contactmatrix/pulls).

### Code of Conduct

Please note that the contactmatrix project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
