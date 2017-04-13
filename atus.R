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
  
  SquaredDeviation <- function(rwt.index) {
    rwt.name <- paste0("RWT06_", rwt.index)
    rwt <- data[[rwt.name]]
    
    rwt.est <- sum(activity * rwt) / sum(rwt)
    
    dev <- (rwt.est - est.mean) ^ 2
    
    return(dev)
  }
  
  squared.deviations <- sapply(c(1:160), SquaredDeviation)
  
  variance <- (4 / 160) * sum(squared.deviations)
  
  return(variance)
}

# Average reading time by sex
atus.reading.sex <- atus %>%
  group_by(YEAR, SEX) %>%
  summarise(
    sample.n = n(),
    pop.n = sum(WT06) / 365,
    est.mean = sum(reading * WT06) / sum(WT06),
    variance = SDRVariance(., est.mean, "reading")
  ) %>%
  mutate(
    stdev = sqrt(est.mean),
    se = stdev / sqrt(sample.n),
    ci.lower = est.mean - (se * 1.96),
    ci.upper = est.mean + (se * 1.96)
  )

write_csv(atus.reading.sex, "atus.reading.sex.csv")
