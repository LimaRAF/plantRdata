
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plantRdata <!-- <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/> -->

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/plantRdata)](https://CRAN.R-project.org/package=plantRdata) -->

[![R CMD
Check](https://github.com/LimaRAF/plantRdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LimaRAF/plantRdata/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/LimaRAF/plantRdata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/LimaRAF/plantRdata/actions/workflows/pkgdown.yaml)
[![Test
coverage](https://github.com/LimaRAF/plantRdata/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/LimaRAF/plantRdata/actions/workflows/test-coverage.yaml)
<!-- [![codecov](https://codecov.io/gh/LimaRAF/plantRdata/branch/master/graph/badge.svg)](https://codecov.io/gh/LimaRAF/plantRdata) -->
[![License: GPL (\>=
3)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%203%29-blue.svg)](https://choosealicense.com/licenses/gpl-3.0/)
<!-- badges: end -->

<p align="left">
• <a href="#overview">Overview</a><br> •
<a href="#installation">Installation</a><br> •
<a href="#references">References</a>
</p>

## Overview

This is a data package that compiles, edits and redistributes relatively
large taxonomic datasets and maps that are currently used by the R
package `plantR` (<https://github.com/LimaRAF/plantR/>). These datasets
are used for the validation of taxonomic and geographic information
associated to species occurrence records.

Currently, `plantRdata` provides processed datasets from the following
sources:

- [World Flora Online](https://www.worldfloraonline.org/)
- [World Checklist of Vascular Plants](https://powo.science.kew.org/)
- [Global Biodiversity Information Facility](https://www.gbif.org/)
  - Only the kingdoms Plantae, Fungi and Animalia are currently provided
- [Flora and Funga of
  Brazil](https://floradobrasil.jbrj.gov.br/consulta/)

Please read the documentation of each dataset for further details (e.g.
`?wfoNames`) on dataset content, processing, orginal source and last
update.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
## Install < remotes > package (if not already installed) ----
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## Install < plantRdata > from GitHub ----
remotes::install_github("LimaRAF/plantRdata")
```

Then you can attach the package `plantRdata`:

``` r
library("plantRdata")
```

## References

This package does not generate new information. It only provide
processed snapshots of the datasets it compiles. So, we kindly ask you
to read and cite the original data providers:

> Borsch, T., Berendsohn, W., Dalcin, E., et al. (2020). World Flora
> Online: Placing taxonomists at the heart of a definitive and
> comprehensive global resource on the world’s plants. Taxon, 69(6):
> 1311-1341. <https://doi.org/10.1002/tax.12373>

> Govaerts, R., Nic Lughadha, E., Black, N. et al. (2021). The World
> Checklist of Vascular Plants, a continuously updated resource for
> exploring global plant diversity. Sci. Data 8: 215.
> <https://doi.org/10.1038/s41597-021-00997-6>

> GBIF Secretariat (2023). GBIF Backbone Taxonomy. Checklist dataset.
> accessed via GBIF.org. <https://doi.org/10.15468/39omei>

> Flora e Funga do Brasil (Constantly updated): Flora e Funga do Brasil
> project. Instituto de Pesquisas Jardim Botânico do Rio de Janeiro.
> Dataset/Checklist. <https://doi.org/10.15468/1mtkaw>
