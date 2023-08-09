# nlreferences 0.16.0

* Add example to the `README.Rmd`

# nlreferences 0.15.2

* Refresh all references so that `length(refcode)` matches the number of imported files

# nlreferences 0.15.1

* Replaces Dutch D-score reference by GSED Phase 1 reference. Affects `transform2y()` and `transform2z()`
* Add test for daz calculation using GSED Phase 1 reference
* Removes `renv` functionality

# nlreferences 0.15.0

* Updates to `tidyselect 1.2.0` grammar for `select()` and `pivot_...()`
* Updates `roxygen``
* Updates GH actions
* Updates `renv` and `renv.lock`

# nlreferences 0.14.0

* `transform2z()` and `transform2y()` now pass down additional arguments to `centile::y2z()` and `centile::z2y()` to allow for calculation of Z-scores outside the age limits of the reference table (e.g. by setting `rule = c(1, 2)` for right hand extrapolation). 

# nlreferences 0.13.1

* Updates `renv` packages to R 4.2.1

# nlreferences 0.13.0

* Adds `renv` package management

# nlreferences 0.12.1

* Adds two spaces at remotes in DESCRIPTION
* Omits LazyData from DESCRIPTION

# nlreferences 0.12.0

* Switches on continuous integration
* Adds Github action `pkgdown`
* Adds Github action `R-CMD-check`
* Adds github-pages branch
* Correct bug in `set_refcodes()` that occurred when `age = NA`

# nlreferences 0.11.0

* Improves error checking in `transform2y()` and `transform2z()` for WFH
* Make `set_refcodes()` accept zero-row data frames 

# nlreferences 0.10.0

* New package name `nlreferences`

# nlreferences 0.9.0

* Adds references for Hindostane subpopulation

# nlreferences 0.8.0

* Adds Dutch 1980 and 2009 references

# nlreferences 0.7.0

* Replace `yzy` by `centile` package

# nlreferences 0.6.0

* Rename as `transform2y()` and `transform2z()` to prevent name clashes with `clopus`
* Include last age of preterm references

# nlreferences 0.5.0

* Added a `NEWS.md` file to track changes to the package.
