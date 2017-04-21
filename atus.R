library(dplyr)
library(ggplot2)
library(readr)
library(survey)

# Create column type hints for replicate weights
col.types <- paste0(
  "id",
  paste0(rep("d", 161), collapse = ""),
  "idddddd"
)

# Load ATUS data
atus.orig <- read_csv("atus_00008.csv", col_types=col.types)

# Working copy
atus <- atus.orig

# Recode sex flag
atus$SEX[atus$SEX == 1] <- "Male"
atus$SEX[atus$SEX == 2] <- "Female"

atus$SEX <- factor(atus$SEX, levels = c("Male", "Female"))

# Add a column of ones for calculating weighted totals
atus$one <- 1

# NB: This analysis uses a jacknife survey design, even though the data was constructed with SDR.
# Per IPUMS, "successive difference replicate weights can be treated as jackknife replicate weights if the options are specified correctly."
# See: https://cps.ipums.org/cps/repwt.shtml
design = svrepdesign(
  data = atus,
  weights = ~WT06,
  repweights = "RWT06_[1-9]+",
  type = "JK1",
  combined.weights = TRUE,
  scale = .025
)

# Calculate population totals
totals <- svyby(~ one, ~ YEAR, design, svytotal) %>%
  mutate(
    one = one / 365,
    se = se / 365
  )

# Compute annual means with error
means <- svyby(~ tvmovies, ~ YEAR + SEX, design, svymean, vartype=c("se", "ci"))

# Convert means to hours
means.hours <- means %>%
  mutate(
    tvmovies = tvmovies / 60,
    se = se / 60,
    ci_l = ci_l / 60,
    ci_u = ci_u / 60
  )

ggplot(data = means.hours, aes(x = YEAR, y = tvmovies, color = SEX)) + 
  geom_line(size = 1.5) + 
  geom_ribbon(aes(ymax = ci_u, ymin = ci_l), alpha = 0.2) +
  ylim(2, 3.5) +
  labs(title = "Time spent watching TV and movies per day", x = "Year", y = "Hours")
