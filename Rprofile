## https://csgillespie.github.io/efficientR/3-3-r-startup.html
## local creates a new, empty environment
## This avoids polluting the global environment with
## the object r
local({
  r = getOption("repos")             
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
})

library(devtools)
