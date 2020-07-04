library(boot)

# used by get_bca
boot_median <- function(data, indices) {
  resampled_data <- data[indices]
  return (median(resampled_data))
}

simple_median <- function(data) {
  list(median=median(data), lower=NA, upper=NA, type="simple-median")  
}

# Calculates the bootstrap interval and returns median,
# and lower and upper bound of the confidence interval.
# The interval is calculated using the adjusted bootstrap percentile (BCa) method.
get_bca <- function(data, num_replicates) {
  if (length(data) < 30) {
    return(simple_median(data))
  }
  if (all(data == data[[1]])) {
    list(median=data[[1]], lower=NA, upper=NA, type="constant")  
  }
  
  b <- boot(data, boot_median, num_replicates, stype = "i") # 1000
  tryCatch({
    
    bb <- boot.ci(b, type="bca")
    # column 4 and 5 contain the lower and upper ends of the interval
    if (is.null(bb$bca[4])) {
      return(simple_median(data))
    } else {
      return(list(median=b$t0, lower=bb$bca[4], upper=bb$bca[5], type = "bca"))
    }
  },
  error = function (cond) {
    print(cond)
    tryCatch({
      bb <- boot.ci(b, type="normal")
      return(list(median=b$t0, lower=bb$normal[2], upper=bb$normal[3], type = "normal"))  
    },
    error = function (cond) {
      print(cond)
      return(simple_median(data))
    })
  })
}
