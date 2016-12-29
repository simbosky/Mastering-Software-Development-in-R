library(dplyr)
library(readr)

## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date) {
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz",
                 year, date)
  
  ## Construct path for storing local file
  dest <- file.path("data", basename(src))
  
  ## Don't download if the file is already there!
  if(!file.exists(dest)){
    download.file(src, dest, quiet = TRUE)
  }
  
  cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
  cran %>% filter(package == pkgname) %>% nrow
}