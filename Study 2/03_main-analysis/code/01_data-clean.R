## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates data wrangling procedure of Study 2
## INPUT: 
##              ./Study 2/03_main-analysis/data/raw-data_study-2.csv
## OUTPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
##              
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  janitor,       # clean_names() function
  tidyverse      # Tidyverse umbrella package
)  


## IMPORT RAW DATA =============================================================
INPUT_FILE_PATH <- "./Study 2/03_main-analysis/data/raw-data_study-2.csv"
df <- read_csv(INPUT_FILE_PATH) %>% clean_names()

# Initial number of participants is 1,030
## > nrow(df)
## [1] 1030


## FILTER OUT SAMPLE ===========================================================
# Leave participants who satisfied minimum criterion
df <- df %>% 
  mutate(q_recaptcha_score = q_recaptcha_score,
         duration_in_seconds = duration_in_seconds) %>% 
  # Filter participants with low reCaptcha score 
  filter(q_recaptcha_score >= 0.5 | is.na(.$q_recaptcha_score)) %>% 
  # Only who agreed to participate in the survey
  filter(q1_2_1 == 1 & q1_2_2 == 1 & q1_2_3 == 1) %>%
  # Those who were not able to pass attention check, filter out
  filter(is.na(attention_1)) %>% 
  # Leave those who responded to every important questions,
  filter(
    !is.na(q2_2), !is.na(q2_3), !is.na(q2_3),
    !is.na(q3_2), !is.na(q3_3), !is.na(q8_2),
    !is.na(q5_2), !is.na(q5_3), !is.na(q5_4), !is.na(q5_5), !is.na(q5_6),
    !is.na(q6_2), !is.na(q6_3), !is.na(q6_4), !is.na(q6_5), !is.na(q6_6),
    !is.na(q16_2), !is.na(q16_3), !is.na(q16_4), !is.na(q16_5), !is.na(q16_6)
    ) 

# 904 participants were left
## > nrow(df)
## [1] 904


## RECODING ====================================================================
# Start recoding the variables
mydata <- df %>% 
  # Factor 1: Valence
  # Biden: fact-checking news debunking the misinformation in Biden's statement
  # Ducey: fact-checking news debunking the misinformation in Ducey's statement
  mutate(valence = case_when(
    (!is.na(.$q10_11))|(!is.na(.$q11_11))|(!is.na(.$q12_11)) ~ "Biden",
    (!is.na(.$q13_11))|(!is.na(.$q14_11))|(!is.na(.$q15_11)) ~ "Ducey"
  ),
  # Factor 2: Political slant of news media
  # Reuter: Neutral, FOX: Conservative, MSNBC: Liberal
  media_slant = case_when(
    (!is.na(.$q10_11))|(!is.na(.$q13_11)) ~ "Reuter",
    (!is.na(.$q11_11))|(!is.na(.$q14_11)) ~ "FOX",
    (!is.na(.$q12_11))|(!is.na(.$q15_11)) ~ "MSNBC"
  ),
  # Dependent variable 1: Persuasive effect of fact-checking news
  # (1) Agreement and (2) belief on the statement (repeated measure)
  agree_t1 = case_when(
    !is.na(.$q10_11) ~ q10_4,
    !is.na(.$q11_11) ~ q11_4,
    !is.na(.$q12_11) ~ q12_4,
    !is.na(.$q13_11) ~ q13_4,
    !is.na(.$q14_11) ~ q14_4,
    !is.na(.$q15_11) ~ q15_4
  ),
  agree_certainty_t1 = case_when(
    !is.na(.$q10_11) ~ q10_5,
    !is.na(.$q11_11) ~ q11_5,
    !is.na(.$q12_11) ~ q12_5,
    !is.na(.$q13_11) ~ q13_5,
    !is.na(.$q14_11) ~ q14_5,
    !is.na(.$q15_11) ~ q15_5
  ),
  belief_t1 = case_when(
    !is.na(.$q10_11) ~ q10_6,
    !is.na(.$q11_11) ~ q11_6,
    !is.na(.$q12_11) ~ q12_6,
    !is.na(.$q13_11) ~ q13_6,
    !is.na(.$q14_11) ~ q14_6,
    !is.na(.$q15_11) ~ q15_6
  ),
  belief_certainty_t1 = case_when(
    !is.na(.$q10_11) ~ q10_7,
    !is.na(.$q11_11) ~ q11_7,
    !is.na(.$q12_11) ~ q12_7,
    !is.na(.$q13_11) ~ q13_7,
    !is.na(.$q14_11) ~ q14_7,
    !is.na(.$q15_11) ~ q15_7
  ),
  agree_t2 = case_when(
    !is.na(.$q10_11) ~ q10_11,
    !is.na(.$q11_11) ~ q11_11,
    !is.na(.$q12_11) ~ q12_11,
    !is.na(.$q13_11) ~ q13_11,
    !is.na(.$q14_11) ~ q14_11,
    !is.na(.$q15_11) ~ q15_11
  ),
  belief_t2 = case_when(
    !is.na(.$q10_11) ~ q10_12,
    !is.na(.$q11_11) ~ q11_12,
    !is.na(.$q12_11) ~ q12_12,
    !is.na(.$q13_11) ~ q13_12,
    !is.na(.$q14_11) ~ q14_12,
    !is.na(.$q15_11) ~ q15_12
  ),
  # Dependent variable 2: Bias perception on fact-checking news
  bias_1 = case_when(
    !is.na(.$q10_11) ~ q10_13,
    !is.na(.$q11_11) ~ q11_13,
    !is.na(.$q12_11) ~ q12_13,
    !is.na(.$q13_11) ~ q13_13,
    !is.na(.$q14_11) ~ q14_13,
    !is.na(.$q15_11) ~ q15_13
  ),
  bias_2 = case_when(
    !is.na(.$q10_11) ~ q10_14,
    !is.na(.$q11_11) ~ q11_14,
    !is.na(.$q12_11) ~ q12_14,
    !is.na(.$q13_11) ~ q13_14,
    !is.na(.$q14_11) ~ q14_14,
    !is.na(.$q15_11) ~ q15_14
  ),
  # Pre-treatment covariate: Party affiliation
  partisanship = case_when(
    q8_2 == 1 ~ "Republican",
    q8_2 == 2 ~ "Democrat",
    (q8_2 == 3)|(q8_2 == 4) ~ "Independent"
  ),
  partisanship_2 = case_when(
    (q8_2 == 1)&(q8_4 == 1) ~ 7,
    (q8_2 == 1)&(q8_4 == 2) ~ 6,
    (q8_2 == 3)&(q8_5 == 1) ~ 5,
    (q8_2 == 4)&(q8_5 == 1) ~ 5,
    (q8_2 == 3)&(q8_5 == 3) ~ 4,
    (q8_2 == 4)&(q8_5 == 3) ~ 4,
    (q8_2 == 3)&(q8_5 == 2) ~ 3,
    (q8_2 == 4)&(q8_5 == 2) ~ 3,
    (q8_2 == 2)&(q8_3 == 2) ~ 2,
    (q8_2 == 2)&(q8_3 == 1) ~ 1,
  ),
  # Pre-treatment covariate: General political knowledge
  gen_knowledge_1 = if_else(q4_2 == 5, 1, 0),
  gen_knowledge_2 = if_else(q4_3 == 3, 1, 0),
  gen_knowledge_3 = if_else(q4_4 == 4, 1, 0),
  gen_knowledge_4 = if_else(q4_5 == 1, 1, 0),
  gen_knowledge_5 = if_else(q4_6 == 2, 1, 0),
  # Pre-treatment covariate: Knowledge on the immigration policy
  immig_knowledge_1 = if_else(q5_2 == 1, 1, 0),
  immig_knowledge_2 = if_else(q5_3 == 2, 1, 0),
  immig_knowledge_3 = if_else(q5_4 == 1, 1, 0),
  immig_knowledge_4 = if_else(q5_5 == 2, 1, 0),
  immig_knowledge_5 = if_else(q5_6 == 2, 1, 0),
  # Attention check questions regarding the partisan media
  attention_media = case_when(
    attention_3 == 6 ~ 'AP',
    attention_3 == 8 ~ 'FOX',
    attention_3 == 9 ~ 'MBNBC',
    attention_3 == 13 ~ 'Reuter',
    attention_3 == 7 ~ 'CNN',
    attention_3 == 5 ~ 'ABC'
  ),
  # Demographic information 
  gender = q16_3, age = q16_2, education = q16_5, income = q16_6
  )

# Reverse code some questions be make consistent with measures in study 1
mydata$agree_t1 <- 8 - mydata$agree_t1
mydata$agree_t2 <- 8 - mydata$agree_t2
mydata$belief_t1 <- 8 - mydata$belief_t1
mydata$belief_t2 <- 8 - mydata$belief_t2

# There is Don't Know (DK) selection in general knowledge question
# DK is recorded as NA value, so convert to 0
mydata$gen_knowledge_1[is.na(mydata$gen_knowledge_1)] <- 0
mydata$gen_knowledge_2[is.na(mydata$gen_knowledge_2)] <- 0
mydata$gen_knowledge_3[is.na(mydata$gen_knowledge_3)] <- 0
mydata$gen_knowledge_4[is.na(mydata$gen_knowledge_4)] <- 0
mydata$gen_knowledge_5[is.na(mydata$gen_knowledge_5)] <- 0

# Sum all correct answer (= 1)
mydata$gen_knowledge <- rowSums(mydata %>% select(c(gen_knowledge_1:gen_knowledge_5)))
mydata$immig_knowledge <- rowSums(mydata %>% select(c(immig_knowledge_1:immig_knowledge_5)))

# Mean value of attitude on immigration
mydata$attitude_immig <- rowMeans(mydata %>% select(c(q2_3:q2_4)))

# Mean value of involvement
mydata$q3_2 <- 6 - mydata$q3_2
mydata$q3_3 <- 6 - mydata$q3_3
mydata$involvement <- rowMeans(mydata %>% select(c(q3_2:q3_3)))

# Mean value of media trust
mydata$q6_2 <- 8 - (mydata$q6_2 - 12)
mydata$q6_3 <- 8 - (mydata$q6_3 - 12)
mydata$q6_4 <- 8 - (mydata$q6_4 - 12)
mydata$q6_5 <- 8 - (mydata$q6_5 - 12)
mydata$q6_6 <- 8 - (mydata$q6_6 - 12)
mydata$media_trust <- rowMeans(mydata %>% select(c(q6_2:q6_6)))

# Difference between t1 and t2
mydata$agree_diff <- mydata$agree_t2 - mydata$agree_t1
mydata$belief_diff <- mydata$belief_t2 - mydata$belief_t1
mydata$bias <- ((mydata$bias_1 + mydata$bias_2)/2 - 6)/10

# Calculate hostile media perception (HMP) score 
# Range: [0, 1]
mydata <- mydata %>% 
  mutate(hmp_score = case_when(
    partisanship == "Democrat" ~ bias + 0.5,
    partisanship == "Republican" ~ 1 - (bias + 0.5),
    TRUE ~ NA_real_
  ))

# Certainty measure 
mydata <- mydata %>% 
  mutate(certainty_agree = case_when(
    agree_certainty_t1 %in% c(1,2) ~ "Certain",
    agree_certainty_t1 %in% c(3,4) ~ "Uncertain"
  ),
  certainty_belief = case_when(
    belief_certainty_t1 %in% c(1,2) ~ "Certain",
    belief_certainty_t1 %in% c(3,4) ~ "Uncertain"
  ))

# Leave only relevant variables for analysis
mydata <- mydata %>% select(valence:belief_t2, partisanship, gender:hmp_score)


## OUTPUT ======================================================================
OUTPUT_FILE_PATH <- "./Study 2/03_main-analysis/data/processed-data_study-2.csv"
mydata %>% write_csv(file = OUTPUT_FILE_PATH, col_names = TRUE)
