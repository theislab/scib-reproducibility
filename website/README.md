# scIB results

This repository holds the code for the scIB results website which displays additional results from the single-cell integration benchmarking project.

You can view the website at https://theislab.github.io/scIB-reproducibility.

## Directory structure

* `_drake.R` - **{drake}** plan for building the website
* `pages` - HTML templates and associated website files
* `R` - R functions
* `renv` - **{renv}** directory containing the package library etc.
* `renv.lock` - **{renv}** snapshot file specifiying package versions
* scIB-results.Rproj - **RStudio** project file

## Environment

The R environment for building the website has been specified using the [**{renv}**][renv] package.
To recreate the environment locally run:

```r
renv::restore()
```

## Building the website

The website can be rebuilt using the [**{drake}**][drake] package.
After you have set up the environment just run:

```r
drake::r_make()
```

[drake]: https://books.ropensci.org/drake/ "drake user manual"
[renv]: https://rstudio.github.io/renv/ "renv project website"

