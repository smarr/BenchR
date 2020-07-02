# ReBenchDB file access of data


load_data <- function(url) {
  # url <- "https://rebench.stefan-marr.de/rebenchdb/get-exp-data/37"
  safe_name <- str_replace_all(url, "[:/.]", "-")
  cache_file <- paste0(str_replace_all(safe_name, "-+", "-"), ".qs")
  cache_file <- here(".cache", cache_file)
  
  if(!file.exists(cache_file)) {
    download.file(url=url, destfile=cache_file)
  }
  
  tryCatch(
    qread(cache_file),
    error = function(c) {
      file.remove(cache_file)
      Sys.sleep(10)
      load_data(url)
    }
  )
}
