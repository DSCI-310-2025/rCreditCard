# scripts/check.R

# Install devtools (this will install to default .libPaths())
install.packages("devtools", repos = "https://cran.rstudio.com")

# Install all dependencies of your package
devtools::install("dataVizModelR", dependencies = TRUE)

# Run all devtools commands in a single session
devtools::document("dataVizModelR")
devtools::test("dataVizModelR")
devtools::build_vignettes("dataVizModelR")
pkgdown::build_site("dataVizModelR")
