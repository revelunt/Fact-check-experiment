## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates data wrangling procedure of Study 1
## INPUT: 
##              ./Study 1/main-analysis/data/raw-data_study-1.xlsx
## OUTPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.csv
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
##
## =============================================================================


## IMPORT PACKAGES==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Tidyverse umbrella package
  rio             # Import data
  )  


## IMPORT DATA==================================================================
df <- import('./Study 1/main-analysis/data/raw-data_study-1.xlsx')


## WRANGLING====================================================================
mydata <- df %>% 
  mutate(
    # Assigned experimental group
    group = gubun,  
    # Political ideology and party affiliation
    ideology = q5, party = q6, 
    # Perceived slant on each news media
    chosun = q21, hani = q22, 
    # Participants judgement on quality of news article
    news_quality = q26,
    # Dependent variables
    agree_t1 = q23, agree_t2 = q24, bias = q25,
    # Demographic variables
    age = q3, education = q27, gender = q1
    ) %>% 
  mutate(
    # Valence of fact-checking news content
    valence = case_when(group %in% c(1, 2) ~ 1, TRUE ~ 2),
    # Political slant of news media delivering the news
    media_slant = case_when(group %in% c(1, 3) ~ 1, TRUE ~ 2),
    # Trichotomize 7-point political ideology measure
    ideology_discrete = case_when(
      ideology %in% c(1, 2, 3) ~ 1,  # Liberal
      ideology %in% 4 ~ 2,           # Moderate
      TRUE ~ 3                       # Conservative
      )
    ) %>% 
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    valence = factor(
      valence, levels = c(1, 2), 
      labels = c("Conservative-consistent", "Liberal-consistent")
      ),
    media_slant = factor(
      media_slant, levels = c(1, 2), 
      labels = c("Conservative Media", "Liberal Media")
      ),
    ideology_discrete = factor(
      ideology_discrete, levels = c(1, 2, 3), 
      labels = c("Liberal", "Moderate", "Conservative")
      ),
    party = factor(
      party, levels = c(1,2,7), 
      labels = c("Minju Party", "Hankuk Party", "Independent")
      )
    ) %>% 
  # Row-wise operation from this point
  rowwise() %>% 
  mutate(
    # Attitude toward economic policy
    economic_policy = mean(c(q8, q9, q10)),
    # Issue involvement
    involvement = mean(c(q11, q12)), 
    # General media trust
    media_trust = mean(c(q13, q14, q15, q16, q17, q18, q19, q20)),
    # Difference score of degree of agreement on the statement
    agree_diff = agree_t2 - agree_t1,
    # Rescale bias perception score from [1, 11] to [-0.5, 0.5]
    bias = (bias - 6)/10
  ) %>% 
  # Select only relevant variables to be used in analysis
  select(no, gubun, group:agree_diff) %>% 
  # Add HMP (Hostile Media Perception) score (range: [0, 1])
  mutate(hmp_score = case_when(
    ideology_discrete == "Liberal" ~ bias + 0.5,
    ideology_discrete == "Conservative" ~ 1 - (bias + 0.5),
    TRUE ~ NA_real_
  )) %>% 
  rowid_to_column("id")


## EXPORT PROCESSED DATA========================================================
export(mydata, './Study 1/main-analysis/data/processed-data_study-1.csv')
save(mydata, file = './Study 1/main-analysis/data/processed-data_study-1.RData')

