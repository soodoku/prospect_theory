## FRISP Recode

library(dplyr)
library(here)
library(readr)
library(haven)
library(car)

# Set directory
here::here()

# Load Data
ffrisp <- read_csv("data/ffrisp/ffrisp1029_1.csv")

# Cleaning up identifier for FFRISP
ffrisp <- ffrisp %>%
  mutate(partid = clean(identifier)) %>%
  filter(complete == "COMPLETED", !is.na(partid)) %>%
  distinct(partid, .keep_all = TRUE) %>%
  mutate(partid = as.character(partid))

# Load Sociodem
sociodem <- read_dta("data/ffrisp/sociodem.dta") %>%
  rename_with(tolower) %>%
  mutate(pid = as.character(abtsrbiid),
         partid = as.character(abtsrbiid))

# Merge Sociodem + Survey
ffrisp2 <- ffrisp %>%
  inner_join(sociodem, by = "partid")

# By Day Issues
ffrisp2 <- ffrisp2 %>%
  mutate(day = as.numeric(substr(start_time, 9, 10)),
         month = as.numeric(substr(start_time, 6, 7)),
         ndays = (month - 9) * 30 + day - 1,
         hour = as.numeric(substr(start_time, 12, 13)),
         hour2 = case_when(
             hour - 6 == -6 ~ 18,
             hour - 6 == -5 ~ 19,
             hour - 6 == -4 ~ 18,
             hour - 6 == -3 ~ 20,
             hour - 6 == -2 ~ 21,
             hour - 6 == -1 ~ 22,
             TRUE ~ NA_real_  # Default value for unmatched cases
           ))

# Rename column
ffrisp2 <- ffrisp2 %>%
  rename(qcond = `Qktcomp) Computational Logic`)

# Certain choice vector
ffrisp2 <- ffrisp2 %>%
  rowwise() %>%
  mutate(certain = if_else(qcond %in% 1:6, eval(parse(text = paste0("qkt", qcond))) == 1, NA_real_))

# Gain Loss
ffrisp2 <- ffrisp2 %>%
  mutate(
    losses = case_when(
      qcond %in% c(2, 4, 6) ~ 1, # Assign 1 for these values
      TRUE ~ 0               # Default to 0 for all other values
    ),
    condition = case_when(
      qcond %in% c(1, 2) ~ "Original Wording",
      qcond %in% c(3, 4) ~ "Internal Flip",
      qcond %in% c(5, 6) ~ "Reworded",
      TRUE ~ NA_character_ # Default to NA for unmatched cases
    )
  )

# Education
ffrisp2 <- ffrisp2 %>%
  mutate(
    ppeduc = if_else(!is.na(ppeduc) & ppeduc < 0, NA_real_, ppeduc),
    highed = as.numeric(ppeduc > 8),
    ed = case_when(
      ppeduc %in% 1:8 ~ "1 lesshs",
      ppeduc == 9 ~ "2 hs",
      ppeduc == 10 ~ "3 somecoll",
      ppeduc == 11 ~ "4 ass",
      ppeduc == 12 ~ "5 bs",
      ppeduc %in% 13:14 ~ "6 grad",
      TRUE ~ NA_character_ # Default for unmatched cases
    )
  )

# Age
ffrisp2 <- ffrisp2 %>%
  mutate(
    ager = case_when(
      ppage <= 24 ~ "1 less24",
      ppage >= 25 & ppage <= 34 ~ "2 2534",
      ppage >= 35 & ppage <= 44 ~ "3 3544",
      ppage >= 45 & ppage <= 54 ~ "4 4554",
      ppage >= 55 & ppage <= 64 ~ "5 5564",
      ppage >= 65 & ppage <= 74 ~ "6 6574",
      ppage >= 75 ~ "7 75more",
      TRUE ~ NA_character_ # Default for unmatched cases
    )
  )

# Gender
# Gender
ffrisp2 <- ffrisp2 %>%
  mutate(
    femaler = case_when(
      ppgender == 2 ~ "female",
      ppgender == 1 ~ "male",
      TRUE ~ NA_character_ # Handle unmatched cases
    )
  )

# Race
ffrisp2 <- ffrisp2 %>%
  mutate(
    race = case_when(
      ppethm == 1 ~ "1 white nh",
      ppethm == 2 ~ "2 black nh",
      ppethm %in% c(3, 5) ~ "4 other nh",
      ppethm == 4 ~ "3 hispanic nh",
      TRUE ~ NA_character_ # Handle unmatched cases
    )
  )

# Census Region
ffrisp2 <- ffrisp2 %>%
  mutate(
    region = case_when(
      ppreg4 == 1 ~ "1 northeast",
      ppreg4 == 2 ~ "2 midwest",
      ppreg4 == 3 ~ "3 south",
      ppreg4 == 4 ~ "4 west",
      TRUE ~ NA_character_ # Handle unmatched cases
    )
  )

save(ffrisp2, file = "data/ffrisp/ffrisp2.rdata")
