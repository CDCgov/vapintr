
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/CDCgov/vapintr/workflows/R-CMD-check/badge.svg)](https://github.com/CDCgov/vapintr/actions?workflow=R-CMD-check)
[![CRAN
status](https://www.r-pkg.org/badges/version/sword)](https://CRAN.R-project.org/package=sword)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

<!-- [![Travis-CI Build Status](https://travis-ci.org/cont-limno/LAGOSNE.svg?branch=main)](https://travis-ci.org/cont-limno/LAGOSNE) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/LAGOSNE)](https://cran.r-project.org/package=LAGOSNE) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/LAGOSNE)](https://cran.r-project.org/package=LAGOSNE)
[![Codecov test coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://codecov.io/gh/tidyverse/dplyr?branch=main)-->

# vapintr

## About

This package was developed to enable Monte Carlo simulation of the
[Johnson & Ettinger groundwater vapor intrusion model tool version
6](https://www.epa.gov/vaporintrusion/epa-spreadsheet-modeling-subsurface-vapor-intrusion)
as developed by EPA (EPA 2020).

The motivation was that to analyze multiple chemicals across multiple
buildings, with uncertain parameters, a Monte Carlo simulation would
allow for the effects of multiple uncertain parameters to be considered
simultaneously in probabilistic estimates of exposure.

The vapintr package was built step wise from the original [Johnson and
Ettinger Model Spreadsheet Tool, Version 6.0
(XLS)](https://semspub.epa.gov/src/document/HQ/100000499) set to the
groundwater source catagory. Note, vapintr does not directly support
soil gas source measurements. We anticipate a future enhancement will
add this feature.

### Introduction to vapintr

### Design philosophy

Where possible were initial coded from the official EPA J&E model
spreadsheet tool. Modification were needed to accommodate the `mc2d`
package in some functions. Functions are designed to operate independent
of each other. This allows the maximum amount of granularity in any
simulation.

### Future improvements

TBD.

### Background and Motivation

The purpose of this vignette is to illustrate the use of the vapintr
package with both a deterministic variables and with a Monte Carlo
simulation. For the deterministic analysis, both central tendency
estimates (CTE) and reasonable maximum estimate (RME) inputs are
provided.

Inputs are provided in an example Microsoft Excel workbook. We will
combine the results of the two analysis for comparison purposes.

## Installation

To install vapintr:

``` r
devtools::install_git("https://github.com/CDCgov/vapintr", ref = "main", build_vignettes = TRUE, build_manual = TRUE)
```

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and
is not subject to domestic copyright protection under 17 USC § 105. This
repository is in the public domain within the United States, and
copyright and related rights in the work worldwide are waived through
the [CC0 1.0 Universal public domain
dedication](https://creativecommons.org/publicdomain/zero/1.0/). All
contributions to this repository will be released under the CC0
dedication. By submitting a pull request you are agreeing to comply with
this waiver of copyright interest.

## License Standard Notice

The repository utilizes code licensed under the terms of the Apache
Software License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it
and/or modify it under the terms of the Apache Software License version
2, or (at your option) any later version.

This source code in this repository is distributed in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
Apache Software License for more details.

You should have received a copy of the Apache Software License along
with this program. If not, see
<http://www.apache.org/licenses/LICENSE-2.0.html>

The source code forked from other open source projects will inherit its
license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](https://github.com/CDCgov/vapintr/blob/main/DISCLAIMER.md)
and [Code of
Conduct](https://github.com/CDCgov/vapintr/blob/main/code-of-conduct.md).
For more information about CDC’s privacy policy, please visit
[http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by
[forking](https://help.github.com/articles/fork-a-repo) and submitting a
pull request. (If you are new to GitHub, you might start with a [basic
tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual,
irrevocable, non-exclusive, transferable license to all users under the
terms of the [Apache Software License
v2](http://www.apache.org/licenses/LICENSE-2.0.html) or later.

All comments, messages, pull requests, and other submissions received
through CDC including this GitHub page may be subject to applicable
federal law, including but not limited to the Federal Records Act, and
may be archived. Learn more at <http://www.cdc.gov/other/privacy.html>.

## Records Management Standard Notice

This repository is not a source of government records, but is a copy to
increase collaboration and collaborative potential. All government
records will be published through the [CDC web
site](http://www.cdc.gov).
