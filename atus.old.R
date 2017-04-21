library(lazyeval)
library(dplyr)
library(readr)
library(tidyr)

# Export 8 from ATUS
# "Media consumption activities by sex (2003-2015) w/ replicate weights"

# Build column types string including replicate weights
col.types <- paste0(
  "id",
  paste0(rep("d", 161), collapse = ""),
  "idddddd"
)

# Read ATUS export
atus.orig <- read_csv("atus_00008.csv", col_types=col.types)

# Working copy
atus <- atus.orig

# Recode sex flag
atus$SEX[atus$SEX == 1] <- "Male"
atus$SEX[atus$SEX == 2] <- "Female"

atus$SEX <- factor(atus$SEX, levels = c("Male", "Female"))

# Calculate estimated variance using replicate weights
SDRVariance <- function(data, est.mean, activity.name) {
  result <- data.frame()
  activity <- data[[activity.name]]
  
  # Calculate a mean estimate using a given replicate weight
  ReplicateEstimate <- function(rwt.index) {
    rwt.name <- paste0("RWT06_", rwt.index)
    rwt <- data[[rwt.name]]
    
    rwt.est <- sum(activity * rwt) / sum(rwt)
    
    return(rwt.est)
  }
  
  rwt.estimates <- sapply(c(1:160), ReplicateEstimate)
  deviations <- rwt.estimates - est.mean
  
  variance <- (4 / 160) * sum(deviations ^ 2)
  
  return(variance)
}

# Average reading time by sex
atus.reading.sex <- atus %>%
  group_by(YEAR) %>%
  summarise(
    sample.n = n(),
    pop.n = sum(WT06) / 365,
    est.mean = sum(tvmovies * WT06) / sum(WT06),
    variance = SDRVariance(., est.mean, "tvmovies")
  ) %>%
  mutate(
    stdev = sqrt(variance),
    se = stdev / sqrt(sample.n),
    ci.lower = est.mean - (se * 1.96),
    ci.upper = est.mean + (se * 1.96)
  )

write_csv(atus.reading.sex, "atus.reading.sex.csv")
