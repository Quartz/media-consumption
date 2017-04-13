library(lazyeval)
library(dplyr)
library(readr)

# Export 8 from ATUS
# "Media consumption activities by sex (2003-2015) w/ replicate weights"

# Read ATUS export
rw.cols <- paste0(rep("d", 161), collapse = "")
col.types <- paste0("id", rw.cols, "idddddd")

atus <- read_csv("atus_00008.csv", col_types=col.types)

ReplicateVariance <- function(table) {
  result <- data.frame()
  
  totals <- table %>%
    summarise(
      sample.n = n(),
      pop.n = sum(WT06) / 365,
      est.mean = sum(reading * WT06) / sum(WT06)
    )
  
  for (i in c(1:160)) {
    rwt <- paste0("RWT06_", i)
    
    rep.formula <- interp("sum(reading * rwt) / sum(rwt)", rwt = as.name(rwt))
    
    result <- rbind(
      result,
      table %>%
        summarise_(.dots = setNames(list(rep.formula), c("rep.estimate")))
    )
  }
  
  variance <- (4 / 160) * sum((result$rep.estimate - totals$est.mean) ^ 2)
  
  return(
    data.frame(
      sample.n = totals$sample.n,
      pop.n = totals$pop.n,
      est.mean = totals$est.mean,
      variance = variance
    )
  )
}

test <- atus %>%
  group_by(YEAR) %>%
  do(ReplicateVariance(.)) %>%
  mutate(
    stdev = sqrt(est.mean),
    se = stdev / sqrt(sample.n),
    ci.lower = est.mean - (se * 1.96),
    ci.upper = est.mean + (se * 1.96)
  )

# Counts and means for reading by sex
atus.reading.totals <- atus %>%
  group_by(YEAR) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(reading * WT06) / sum(WT06)
  )

