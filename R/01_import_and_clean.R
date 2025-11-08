library(tidyverse)
library(tidytext)
library(textdata)
afinn <- get_sentiments()

# Handy values ####
mslq_valid_responses <- c("Strongly agree","Agree","Somewhat agree","Neither agree nor disagree",
                          "Somewhat disagree","Disagree","Strongly disagree")
likert_map <- c(
  "Strongly agree" = 5,
  "Somewhat agree" = 4,
  "Neither agree nor disagree" = 3,
  "Somewhat disagree" = 2,
  "Strongly disagree" = 1
)

# Functions ####
parse_any <- function(x) {
  suppressWarnings(ymd_hms(x, quiet = TRUE)) %||%
    suppressWarnings(ymd(x, quiet = TRUE)) %||%
    suppressWarnings(mdy_hms(x, quiet = TRUE)) %||%
    suppressWarnings(mdy_hm(x, quiet = TRUE)) %||%
    suppressWarnings(mdy(x, quiet = TRUE))
}

# Load Data ####
mslq$RecordedDate
# load MSLQ
mslq <- read_csv("./data/edited_MSLQ_Data.csv") %>% 
  dplyr::filter(Finished) %>% 
  mutate(
    ts = suppressWarnings(mdy_hm(RecordedDate)) %||% suppressWarnings(mdy(RecordedDate)),
    term = if_else(month(ts) %in% 8:12, "Fall", "Spring"),
    acad_term = str_c(year(ts), term, sep = "_")
  )
# load MCAT
mcat_mentor <- read_csv("./data/edited_MCAT_mentor.csv") %>% 
  dplyr::filter(Finished) %>% 
  mutate(
    ts = suppressWarnings(mdy_hm(RecordedDate)) %||% suppressWarnings(mdy(RecordedDate)),
    term = if_else(month(ts) %in% 8:12, "Fall", "Spring"),
    acad_term = str_c(year(ts), term, sep = "_")
  )
mcat_mentee <- read_csv("./data/edited_MCAT_mentee.csv") %>% 
  dplyr::filter(Finished) %>% 
  mutate(
    ts = suppressWarnings(mdy_hm(RecordedDate)) %||% suppressWarnings(mdy(RecordedDate)),
    term = if_else(month(ts) %in% 8:12, "Fall", "Spring"),
    acad_term = str_c(year(ts), term, sep = "_")
  )
# load impact of relationship
impact <- read_csv("./data/edited_impact_of_relationship.csv") %>% 
  mutate(
    ts = suppressWarnings(mdy_hm(RecordedDate)) %||% suppressWarnings(mdy(RecordedDate)),
    term = if_else(month(ts) %in% 8:12, "Fall", "Spring"),
    acad_term = str_c(year(ts), term, sep = "_")
  )

# load question-to-id dictionaries
dictionary_mslq <- read_csv("./data/mslq_dictionary.csv")
dictionary_mcat <- read_csv("./data/mcat_dictionary.csv") %>% 
  mutate(Question = Question %>% str_split("\\.",n = 2) %>% map_chr(2) %>% str_squish()) %>% 
  dplyr::filter(responder == "mentor")
dictionary_impact <- read_csv("./data/impact_dictionary.csv")



# tidy data ####

## MSLQ ####
# (self reflection)
# colnames look fine and match dictionary
names(mslq); all(dictionary_mslq$ID %in% names(mslq))
# put into long format
mslq <- 
  mslq %>% 
  pivot_longer(starts_with("Q"),
               names_to = "ID", values_to = "Response")
# add actual questions
mslq <- left_join(mslq, dictionary_mslq)
# clean up bad responses
mslq <- 
  mslq %>% 
  dplyr::filter(Response %in% mslq_valid_responses)
# add numeric response column (7 is most agree)
mslq <- 
  mslq %>% 
  mutate(Response_numeric = case_when(Response == mslq_valid_responses[1] ~ 7,
                                      Response == mslq_valid_responses[2] ~ 6,
                                      Response == mslq_valid_responses[3] ~ 5,
                                      Response == mslq_valid_responses[4] ~ 4,
                                      Response == mslq_valid_responses[5] ~ 3,
                                      Response == mslq_valid_responses[6] ~ 2,
                                      Response == mslq_valid_responses[7] ~ 1,))
glimpse(mslq)
mslq <- 
  mslq %>% 
  mutate(
    factor_group = case_when(
      ID %in% paste0("Q", 3:11) ~ "Self_Efficacy",
      ID %in% paste0("Q", 12:20) ~ "Intrinsic_Value",
      ID %in% paste0("Q", 21:28) ~ "Cognitive_Strategy_Use",
      ID %in% paste0("Q", 29:37) ~ "Self_Regulation",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Response_numeric = case_when(
    ID %in% c("Q22", "Q30", "Q33", "Q34") ~ 8 - Response_numeric,  # flip 1↔7, 2↔6, 3↔5
    TRUE ~ Response_numeric
  ))

# make factor for academic term
term_order <- 
  c("2020_Spring","2020_Fall",
  "2021_Spring","2021_Fall",
  "2022_Spring","2022_Fall",
  "2023_Spring","2023_Fall",
  "2024_Spring","2024_Fall",
  "2025_Spring")
mslq$acad_term <- factor(mslq$acad_term,levels = term_order)
mslq$cohort <- as.character(mslq$cohort)
mslq$attempt <- as.character(mslq$attempt)
mslq$Student <- as.character(mslq$Student)
# if cohort id is 'off' then things need to be shifted for those rows,
# starting w/ marital status, shifted 1 column to the right
shift_start_col <- which(names(mslq) == "maritalStatus")
class_levels <- c("FR","SO","JR","SR")
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["Student"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["attempt"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["attempt"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["cohort"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["cohort"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["gender"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["gender"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["majorProgram"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["majorProgram"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["majorDescription"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["majorDescription"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["classLevelCode"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["classLevelCode"]] <- 
  mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["maritalStatus"]]
mslq[mslq$maritalStatus %in% class_levels,][,shift_start_col:(shift_start_col+7)][["maritalStatus"]] <- NA

mslq$cohort %>% unique
## MCAT ####

# pivot longer
mcat_mentee <- 
  mcat_mentee %>% 
  pivot_longer(starts_with("Q"),
               names_to = "ID",
               values_to = "Response")
mcat_mentor <- 
  mcat_mentor %>% 
  pivot_longer(starts_with("Q"),
               names_to = "ID",
               values_to = "Response")

# correct messy values to numeric (1=least skilled, 7=extremely skilled)
mcat_mentee$Response <- 
  mcat_mentee$Response %>% gsub(pattern="[^0-9]", replacement="") %>% as.numeric()
mcat_mentor$Response <- 
  mcat_mentor$Response %>% gsub(pattern="[^0-9]", replacement="") %>% as.numeric()

# join them
identical(unique(mcat_mentee$ID),unique(mcat_mentor$ID)) # all Qs match up perfectly
mcat <- full_join(mcat_mentee,mcat_mentor)
# mcat now has both mentor and mentee responses. Denoted in the `responder_type` column

# add actual question text
# using the mentor versions of the actual questions as they're more generalizable
mcat <- left_join(mcat,dictionary_mcat)

# make factor for term
mcat$acad_term <- factor(mcat$acad_term,levels = term_order)

## Impact

# convert to numeric (except for Q5.1)
# rename Q5.1
impact <- 
  impact %>% 
  rename("Free_response" = "Q5.1")
impact <- 
  impact %>% 
  mutate(across(
    starts_with("Q"),
    ~ recode(.x, !!!likert_map) %>% as.numeric()
  ))

impact %>% 
  dplyr::select(starts_with("Q")) %>% 
  pivot_longer(everything()) %>% 
  pluck("value") %>% 
  unique()


# pivot longer
impact <- 
  impact %>% 
  pivot_longer(starts_with("Q"),
               names_to = "ID",
               values_to = "Response")

# add actual question text
impact <- 
  full_join(impact,dictionary_impact)

# add sentiment analysis values
impact <- impact %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Free_response) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = mean(value, na.rm = TRUE)) %>%
  right_join(impact %>% mutate(id = row_number()), by = "id") %>%
  select(-id)
# sentiment scores roughly range from -5 (most negative) to +5 (most positive)

# make factor for term
impact$acad_term <- factor(impact$acad_term,levels = term_order)

# make grouped stand-in date for each data set
grouped_date_mslq <- 
paste(
  as.Date(mslq$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% year(),
  as.Date(mslq$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% month(),
  1, sep = "-"
) %>% as.Date()

grouped_date_mcat <- 
  paste(
    as.Date(mcat$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% year(),
    as.Date(mcat$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% month(),
    1, sep = "-"
  ) %>% as.Date()

grouped_date_impact <- 
  paste(
    as.Date(impact$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% year(),
    as.Date(impact$RecordedDate %>% str_split(" ") %>% map_chr(1),format='%m/%d/%Y') %>% month(),
    1, sep = "-"
  ) %>% as.Date()

mslq$date <- grouped_date_mslq
mcat$date <- grouped_date_mcat
impact$date <- grouped_date_impact

# make pretty-text for mslq response groups
mslq$factor_group <- mslq$factor_group %>% str_replace_all("_"," ") %>% str_to_sentence()

# make pretty text for mcat questions
mcat$Question <- 
  mcat$Question %>% 
  str_remove_all("\\(.*\\)") %>% 
  str_squish()

# Export cleaned data
saveRDS(mslq,"./data/mslq.RDS")
saveRDS(mcat,"./data/mcat.RDS")
saveRDS(impact,"./data/impact.RDS")

