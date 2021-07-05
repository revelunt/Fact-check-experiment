##==============================================================================
##
## PROJECT TITLE: Fact-Checking News by Partisan Media
## PROJECT AUTHOR: JE HOON CHAE (YONSEI UNIVERSITY)
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: Preprocessing Data
##
##==============================================================================

## Import Packages==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, conflicted, readxl)
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("ungroup", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

df <- read_xlsx('./data/raw/fact_check.xlsx', col_names = TRUE)
mydata <- df %>% 
  mutate(
    group = gubun, age = q3, education = q27, gender = q1, 
    ideology = q5, chosun = q21, hani = q22, party = q6, news_quality = q26,
    agree_before = q23, agree_after = q24, bias = q25
    ) %>% 
  mutate(
    favor = case_when(group %in% c(1, 2) ~ 1, TRUE ~ 2),
    source = case_when(group %in% c(1, 3) ~ 1, TRUE ~ 2),
    ideology_discrete_1 = case_when(ideology %in% c(1, 2, 3) ~ 1,
                                    ideology %in% 4 ~ 2,
                                    TRUE ~ 3),
    ideology_discrete_2 = case_when(ideology %in% c(1, 2) ~ 1,
                                    ideology %in% c(3, 4, 5) ~ 2,
                                    TRUE ~ 3),
    partisanship_two_group = case_when(ideology %in% c(1, 2, 6, 7) ~ 1, TRUE ~ 2),
    partisanship_three_group = case_when(ideology == 4 ~ 1,
                                         ideology %in% c(3, 5) ~ 2,
                                         TRUE ~ 3),
    bias_intensity = case_when(bias == 6 ~ 0,
                               bias %in% c(5, 7) ~ 1,
                               bias %in% c(4, 8) ~ 2,
                               bias %in% c(3, 9) ~ 3,
                               bias %in% c(2, 10) ~ 4,
                               TRUE ~ 5)
    ) %>% 
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    favor = factor(favor, levels = c(1, 2), labels = c("Conservative-consistent", "Liberal-consistent")),
    source = factor(source, levels = c(1, 2), labels = c("Conservative Media", "Liberal Media")),
    ideology_discrete_1 = factor(ideology_discrete_1, levels = c(1, 2, 3), 
                                 labels = c("Liberal", "Moderate", "Conservative")),
    ideology_discrete_2 = factor(ideology_discrete_2, levels = c(1, 2, 3), 
                                 labels = c("Liberal", "Moderate", "Conservative")),
    partisanship_two_group = factor(partisanship_two_group, levels = c(1, 2),
                                    labels = c("Strong", "Weak")),
    partisanship_three_group = factor(partisanship_three_group, levels = c(1, 2, 3),
                                    labels = c("Moderate", "Weak", "Strong")),
    party = factor(party, levels = c(1,2,7), 
                   labels = c("Minju Party", "Hankuk Party", "Independent"))
    ) %>% 
  rowwise() %>% 
  mutate(
    economic_policy = mean(c(q8, q9, q10)),
    involvement = mean(c(q11, q12)), 
    media_trust = mean(c(q13, q14, q15, q16, q17, q18, q19, q20)),
    agree_difference = agree_after - agree_before,
    bias = bias - 6,
    bias_intensity = bias_intensity/5
  ) %>% 
  select(no, gubun, group:agree_difference) %>% 
  rowid_to_column("id")

# df %>% glimpse()
# df %>% summary()

save(mydata, file = './data/mydata.RData')
write_csv(mydata, file = './data/mydata.csv', col_names = TRUE)

