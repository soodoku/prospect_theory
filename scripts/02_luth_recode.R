library(dplyr)
library(readr)
library(stringr)
library(haven)
library(car)
library(janitor)

# Load Data
luth <- read_csv("data/luth/luth1024.csv")

# Filter data based on conditions
luth <- luth %>%
  filter(complete == "COMPLETED", qcheck == 5)

# Load Sociodem data and clean identifiers
dem_luth <- read_sav("data/luth/dem.luth.sav") %>%
  mutate(partid = clean(identifier))

# Merge Sociodem + Survey
luth_dem_merge <- luth %>%
  inner_join(dem_luth, by = c("uniq" = "partid"))

# Merge Luth with Party ID
party_luth <- read_sav("data/luth/party.luth.sav") %>%
  mutate(partid = clean(identifier))

luth2 <- luth_dem_merge %>%
  left_join(party_luth, by = c("uniq" = "partid")) %>%
  janitor::clean_names()

# By Day Issues
luth2 <- luth2 %>%
  mutate(
    month = as.numeric(substr(start, 1, 2)),
    day = as.numeric(str_extract(start, "(?<=/)[0-9]+(?= )")),
    ndays = (month - 10) * 30 + day - 1,
    hour = as.numeric(str_extract(start, "(?<= )[0-9]+(?=:)"))
  )

# Sociodem - Education
luth2 <- luth2 %>%
  mutate(
    # Education categories
    ed = case_when(
      ppeduc %in% 1:7 ~ "1 lesshs",
      ppeduc == 8 ~ "2 hs",
      ppeduc == 10 ~ "3 voc",
      ppeduc == 9 ~ "4 somecoll",
      ppeduc == 11 ~ "5 ass",
      ppeduc == 12 ~ "6 bs",
      ppeduc %in% 13:15 ~ "7 grad",
      TRUE ~ NA_character_ # Default for unmatched cases
    ),
    ed2 = case_when(
      ppeduc %in% 1:7 ~ "1 lesshs",
      ppeduc == 8 ~ "2 hs",
      ppeduc == 9 ~ "3 somecoll",
      ppeduc == 11 ~ "4 ass",
      ppeduc == 12 ~ "5 bs",
      ppeduc %in% 13:15 ~ "6 grad",
      ppeduc == 10 ~ NA_character_, # Set to NA for ppeduc == 10
      TRUE ~ NA_character_
    ),
    # Update ppeduc and create high education indicator
    ppeduc = qd11,
    highed = as.numeric(qd11 > 8) # High education: `1` if greater than 8, else `0`
  )

# Age
luth2 <- luth2 %>%
  mutate(
    ppage = qd7y,
    ager = case_when(
      ppage <= 24 ~ "1 less24",
      ppage >= 25 & ppage <= 34 ~ "2 2534",
      ppage >= 35 & ppage <= 44 ~ "3 3544",
      ppage >= 45 & ppage <= 54 ~ "4 4554",
      ppage >= 55 & ppage <= 64 ~ "5 5564",
      ppage >= 65 & ppage <= 74 ~ "6 6574",
      ppage >= 75 ~ "7 75more",
      TRUE ~ NA_character_
    )
  )

# Gender
luth2 <- luth2 %>%
  mutate(
    ppgender = as.numeric(qd8 == 2),
    femaler = case_when(
      ppgender == 1 ~ "female",
      ppgender == 0 ~ "male",
      TRUE ~ NA_character_
    )
  )

# Race
luth2 <- luth2 %>%
  mutate(
    race = case_when(
      str_detect(temp <- paste(qd14_1, qd14_2, qd14_3, qd14_4, qd14_5, qd14_6, sep = ""), "^100000$") ~ "white",
      str_detect(temp, "^010000$") ~ "black",
      str_detect(temp, "^000100$") ~ "asian",
      temp %in% c("000001", "001000", "000010") ~ "other",
      temp == "NANANANANANA" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    race2 = case_when(
      str_detect(temp <- paste(qd14_1, qd14_2, qd14_3, qd14_4, qd14_5, qd14_6, qd12, sep = ""), "^1000002$") ~ "1 white nh",
      str_detect(temp, "^0100002$") ~ "2 black nh",
      temp %in% c("0000011", "0000101", "0001011", "0010001", "0100001", "1000001", "1000011", "1000101", "1001011", "1010101", "1110011", "1100011", "1100001") ~ "3 hispanic",
      temp %in% c("0001002", "0000012", "0010002", "0000102") ~ "other",
      temp == "NANANANANANA" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# State and Census Region
stnum <- read_csv("data/state.num.csv")
luth2 <- luth2 %>%
  mutate(
    state = tolower(stnum$state[match(qd1, stnum$num)]),
    region = tolower(stnum$census[match(qd1, stnum$num)]),
    region2 = case_when(
      region == "northeast" ~ "1 northeast",
      region == "midwest" ~ "2 midwest",
      region == "south" ~ "3 south",
      region == "west" ~ "4 west",
      TRUE ~ NA_character_
    )
  )

# Party Time
luth2 <- luth2 %>%
  mutate(
    # Combine q2a.y, q2b.y, and q2c into a single string for processing
    combined_pid = paste(q2a, q2b, q2c, sep = ""),
    # Map combined_pid to pid7 values using case_when
    pid7 = case_when(
      combined_pid == "11NA" ~ 7,
      combined_pid == "12NA" ~ 6,
      combined_pid == "3NA1" ~ 5,
      combined_pid == "3NA3" ~ 4,
      combined_pid == "3NA2" ~ 3,
      combined_pid == "11NA" ~ 2,
      combined_pid == "22NA" ~ 1,
      combined_pid == "21NA" ~ 0,
      combined_pid == "NANANA" ~ NA_real_,
      TRUE ~ NA_real_ # Default to NA for unmatched cases
    ),
    # Convert pid7 to numeric
    pid = as.numeric(pid7)
  ) %>%
  select(-combined_pid)

# Certain Vector
luth2 <- luth2 %>%
  mutate(
    certain = case_when(
      qra == 1 ~ as.numeric(q1a == 1),
      qra == 2 ~ as.numeric(q2a == 1),
      qra == 3 ~ as.numeric(q3a == 1),
      qra == 4 ~ as.numeric(q4a == 1),
      qra == 5 ~ as.numeric(q5a == 1),
      qra == 6 ~ as.numeric(q6a == 1),
      qra == 7 ~ as.numeric(q7a == 1),
      qra == 8 ~ as.numeric(q8a == 1),
      qra == 9 ~ as.numeric(q9a == 2),
      qra == 10 ~ as.numeric(q10a == 2),
      qra == 11 ~ as.numeric(q11a == 1),
      qra == 12 ~ as.numeric(q12a == 1),
      qra == 13 ~ as.numeric(q13a == 1),
      qra == 14 ~ as.numeric(q14a == 1),
      qra == 15 ~ as.numeric(q15a == 1),
      qra == 16 ~ as.numeric(q16a == 1),
      qra == 17 ~ as.numeric(q17a == 2),
      qra == 18 ~ as.numeric(q18a == 2),
      TRUE ~ NA_real_
    )
  )

# Condition
luth2 <- luth2 %>%
  mutate(
    condition = case_when(
      qra %in% c(1, 2) ~ "Original Wording",
      qra %in% c(3, 4) ~ "Save 199 with certainty",
      qra %in% c(5, 6) ~ "Save 201 with certainty",
      qra %in% c(7, 8) ~ "Refining Risk Save",
      qra %in% c(9, 10) ~ "Response Order Flip",
      qra %in% c(11, 12) ~ "Kuhberger",
      qra %in% c(13, 14) ~ "Kuhberger -Internal Flip",
      qra %in% c(15, 16) ~ "Internal Flip",
      qra %in% c(17, 18) ~ "Internal Flip and Response Flip",
      TRUE ~ NA_character_
    )
  )

# Gain Loss
luth2 <- luth2 %>%
  mutate(
    losses = if_else(qra %in% seq(2, 18, 2), 1, 0)
  )

# First Choice
luth2 <- luth2 %>%
  mutate(
    firstchoice = if_else(qra %in% c(9, 10, 17, 18), 1, 0)
  )

# Political Knowledge
luth2 <- luth2 %>%
  mutate(
    qk1c = clean(qk1),
    qk1r = case_when(
      str_detect(qk1c, "vice|vp") ~ 1,
      is.na(qk1c) ~ NA_real_,
      qk1c == "" ~ 0,
      str_detect(tolower(qk1c), "don") ~ 0,
      TRUE ~ 0
    ),
    qk2r = as.numeric(qk2 == 3),
    qk3r = as.numeric((as.numeric(qk3) > 60 & as.numeric(qk3) < 70) | qk3 == "2/3"),
    qk4r = as.numeric(qk4 == 2),
    qk5r = as.numeric(qk5 == 1),
    meanpk = rowMeans(select(., qk1r, qk2r, qk3r, qk4r, qk5r), na.rm = TRUE)
  )

# Open Ended
luth2 <- luth2 %>%
  mutate(
    caseid = row_number(),
    openend = coalesce(q1b[!is.na(q1a)], q2b.x[!is.na(q2a.x)], q3b[!is.na(q3a)], q4b[!is.na(q4a)],
                       q5b[!is.na(q5a)], q6b[!is.na(q6a)], q7b[!is.na(q7a)], q8b[!is.na(q8a)],
                       q9b[!is.na(q9a)], q10b[!is.na(q10a)], q11b[!is.na(q11a)], q12b[!is.na(q12a)],
                       q13b[!is.na(q13a)], q14b[!is.na(q14a)], q15b[!is.na(q15a)], q16b[!is.na(q16a)],
                       q17b[!is.na(q17a)], q18b[!is.na(q18a)])
  )

# Save final dataset
save(luth2, file = "data/luth/luth2.rdata", ascii = TRUE)

# Save Open Ended
luth_open_end <- luth2 %>%
  select(caseid, condition, choice = certain, openend) %>%
  mutate(code1 = "", code2 = "", comments = "")

write_csv(luth_open_end, "data/luth.open.ended/luth.open.end.csv")

# Merge Ballot Data
ballot <- read_spss("data/merge/ballot.sav") %>%
  rename_all(tolower)
merged_ballot_kt_dem <- ballot %>%
  inner_join(luth, by = c("identifier" = "uniq"))
nrow(merged_ballot_kt_dem)
save(merged_ballot_kt_dem, file = "data/merge/merged.ballot.kt.dem.rdata", ascii = TRUE)