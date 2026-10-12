#audit
#setwd("C:/Users/riley/Desktop/audit stuff/FINAL")

# Packages ------------------------------------
library(tidyverse)

# Data Cleaning -------------------------------

# Import Data
q3_2025 = read_csv("data/2025_q3.csv")

# Examine
# I expect 54 rows, 4 columns
dim(q3_2025)
names(q3_2025)
summary(q3_2025) # shows min of -1, which is in the codebook

# Recode
# codebook says -1 means claim was still open
q3_2025 <- q3_2025 |>
  mutate(
    days_to_close_clean = na_if(days_to_close, -1)
  )

# Confirm
count(q3_2025, days_to_close, days_to_close_clean) # -1 turned to NA


# Analyze -------------------------------------
# claim becomes late after 30 days (30 day limit)
days_to_close_limit <- 30

# add boolean late column to determine if a claim was late
q3_2025 <- q3_2025 |>
  mutate(
    late = days_to_close_clean > days_to_close_limit
  )

# show what claim types are more efficient at closing claims than others
claim_type_data <- q3_2025 |>
  group_by(claim_type) |>
  summarize(
    claims = n(),
    still_open = sum(is.na(days_to_close_clean)),
    avg_days_to_close = mean(days_to_close_clean, na.rm = TRUE),
    pct_late = mean(late, na.rm = TRUE)
  ) |>
  ungroup()
claim_type_data

#save
write_csv(claim_type_data, "output/2025_q3_claim_data.csv")
