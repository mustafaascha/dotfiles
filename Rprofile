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

## See http://gettinggeneticsdone.blogspot.com/2013/06/customize-rprofile.html

## Load packages
#library(BiocInstaller)

## Don't show those silly significanct stars
options(show.signif.stars=FALSE)

## Do you want to automatically convert strings to factor variables in a data.frame?
## WARNING!!! This makes your code less portable/reproducible.
options(stringsAsFactors=FALSE)

## Get the sqldf package to play nicely on OSX. No longer necessary with R 3.0.0
## From http://stackoverflow.com/questions/8219747/sqldf-package-in-r-querying-a-data-frame
## options(sqldf.driver="SQLite")
# options(gsubfn.engine = "R")

## Don't ask me for my CRAN mirror every time
options("repos" = c(CRAN = "http://cran.rstudio.com/"))

## Create a new invisible environment for all the functions to go in so it doesn't clutter your workspace.
.env <- new.env()

## Returns a logical vector TRUE for elements of X not in Y
.env$"%nin%" <- function(x, y) !(x %in% y) 

## Returns names(df) in single column, numbered matrix format.
.env$n <- function(df) matrix(names(df)) 

## Single character shortcuts for summary() and head().
.env$s <- base::summary
.env$h <- utils::head

## ht==headtail, i.e., show the first and last 10 items of an object
.env$ht <- function(d) rbind(head(d,10),tail(d,10))

## Show the first 5 rows and first 5 columns of a data frame or matrix
.env$hh <- function(d) if(class(d)=="matrix"|class(d)=="data.frame") d[1:5,1:5]

## Read data on clipboard.
.env$read.cb <- function(...) {
  ismac <- Sys.info()[1]=="Darwin"
  if (!ismac) read.table(file="clipboard", ...)
  else read.table(pipe("pbpaste"), ...)
}

## Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x) {
    rownames(x) <- NULL
    x
}

## List objects and classes (from @_inundata, mod by ateucher)
.env$lsa <- function() {
{
    obj_type <- function(x) class(get(x, envir = .GlobalEnv)) # define environment
    foo = data.frame(sapply(ls(envir = .GlobalEnv), obj_type))
    foo$object_name = rownames(foo)
    names(foo)[1] = "class"
    names(foo)[2] = "object"
    return(unrowname(foo))
}

## List all functions in a package (also from @_inundata)
.env$lsp <-function(package, all.names = FALSE, pattern) {
    package <- deparse(substitute(package))
    ls(
        pos = paste("package", package, sep = ":"),
        all.names = all.names,
        pattern = pattern
    )
}

## Open Finder to the current directory on mac
.env$macopen <- function(...) if(Sys.info()[1]=="Darwin") system("open .")
.env$o       <- function(...) if(Sys.info()[1]=="Darwin") system("open .")

  ## Use the *nix wc program to quickly determine the number of lines
  ## of a file (and print the first few lines).
.env$wc <- function(f){
    stopifnot(is.character(f))
    stopifnot(length(f)==1)
    if(file.exists(f)){
      system(paste("head", f))
      cmd <- sprintf("wc -l %s", f)
      wc.txt <- system(cmd, intern=TRUE)
      wc.sub <- sub(" .*","",wc.txt)
      as.integer(wc.sub)
    }else{
      0L
    }
}

  ## Use the *nix ps program to get the memory usage of this R process.
.env$memory.usage <- function(ps.parameter=paste("-p", Sys.getpid())){
    cmd <- sprintf("ps %s -o pid,cmd,rss", ps.parameter)
    ps.lines <- system(cmd, intern=TRUE)
    stopifnot(length(ps.lines) > 1)
    ps.table <- read.table(text=ps.lines, header=TRUE)
    ps.table$megabytes <- ps.table$RSS/1024
    ps.table
}

  ## convert an author list string from an automatically formatted
  ## bibtex entry (Hocking, Toby Dylan and Rigaill, Guillem) to a
  ## human readable abbreviated list (Hocking TD, Rigaill G).
.env$bibAuthors2text <- function(authors.str){
    authors.vec <- strsplit(authors.str, " and ")[[1]]
    authors.mat <- namedCapture::str_match_named(authors.vec, "(?<last>[^,]+), (?<first>[^ ]+)(?: (?<rest>.*))?")
    abbrev.vec <- data.table(authors.mat)[, paste0(last, " ", substr(first, 1, 1), substr(rest, 1, 1))]
    paste(abbrev.vec, collapse=", ")
}

meminfo <- function(){
  meminfo.df <- read.table("/proc/meminfo", sep=":", row.names=1)
  names(meminfo.df) <- c("text")
  meminfo.df$digits <- gsub("[^0-9]", "", meminfo.df$text)
  meminfo.df$unit <- ifelse(grepl("kB", meminfo.df$text), "kB", "pages")
  meminfo.df$value <- as.numeric(meminfo.df$digits)
  meminfo.df$megabytes <- as.integer(meminfo.df$value/1024)
}

free <- function(){
  free.lines <- system("free -m", intern=TRUE)
  df <- read.table(text=free.lines, sep=":", row.names=1)
  names(df) <- "values"
  mb.text <- sub(" +.*", "", sub("^ *", "", df$values))
  data.frame(megabytes=as.numeric(mb.text),
        row.names=c("total", "used", "swap"))
}




## Attach all the variables above
attach(.env)

## .First() run at the start of every R session. 
## Use to load commonly used packages? 
.First <- function() {
	# library(ggplot2)
	cat("\nSuccessfully loaded .Rprofile at", date(), "\n")
}

## .Last() run at the end of the session
.Last <- function() {
	# save command history here?
	cat("\nGoodbye at ", date(), "\n")
}

