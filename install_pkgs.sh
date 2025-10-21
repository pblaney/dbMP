#!/usr/bin/env bash
# Install necessary R packages for deploying dbMP dashboard

echo "Installing necessary R packages for deploying dbMP dashboard ..."

package_deps=(utils methods stats tools grDevices
              graphics cli lifecycle rlang glue
              sass fs htmltools base64enc digest
              ellipsis fastmap R6 rappdirs vctrs
              clipr utf8 timechange pkgconfig memoise
              cachem bit64 bit blob httpuv
              Rcpp later magrittr promises plogr
              DBI anytime stringi RSQLite lubridate
              stringr usethis mime jsonlite xtable
              fontawesome sourcetools crayon withr curl
              covr xml2 brio ps processx
              callr desc rprojroot evaluate pkgload
              praise diffobj fansi pillar tibble
              rematch2 waldo testthat commonmark jquerylib
              bslib shiny miniUI prettyunits pkgbuild
              downlit yaml sys askpass openssl
              httr purrr systemfonts cpp11 textshaping
              xfun highr knitr tinytex rmarkdown
              markdown whisker pkgdown htmlwidgets profvis
              sessioninfo xopen rcmdcheck remotes brew
              roxygen2 rversions urlchecker devtools lazyeval
              crosstalk DT sodium generics tidyselect
              dplyr hms tzdb progress vroom
              readr shinydashboard shinyjs shinyauthr shinyWidgets)

for i in "${package_deps[@]}"
    do
        echo 
        echo "INSTALL: Begin install of R package ... ${i}"
        cmd="R -q -e 'install.packages(\"${i}\", repo = \"https://cloud.r-project.org/\")'"
        (eval $cmd)
        sleep 2
    done

echo 
echo "INSTALL: Begin install of R package ... bh"
R -q -e 'devtools::install_github("eddelbuettel/bh")'
sleep 2

echo "D O N E ..."
