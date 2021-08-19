## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Appendix Table B3
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a table with the data in Appendix Table B3 of the paper
##
## =============================================================================


## IMPORT PACKAGES==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  lsr,           # CohenD() 
  tidyverse      # Tidyverse umbrella package
)  


## IMPORT DATA==================================================================
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## GENERATE TABLE===============================================================
tab1 <- mydata %>% 
  filter(ideology_discrete != "Moderate") %>% 
  group_by(group, ideology_discrete) %>% 
  summarise(n = n(), mean = round(mean(hmp_score),2), sd = round(sd(hmp_score),2))

tab2 <- mydata %>%
  filter(ideology_discrete != "Moderate") %>% 
  dplyr::select(ideology_discrete, group, hmp_score) %>%
  group_by(ideology_discrete, group) %>%
  group_modify(~tidy(t.test(.$hmp_score, mu = 0.5, alt = "two.sided"))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0") | (p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- mydata %>%
  dplyr::select(ideology_discrete, group, hmp_score) %>%
  group_by(group, ideology_discrete) %>%
  group_modify(~ tidy(cohensD(.$hmp_score, mu = 0.5))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3) 


## OUTPUT ======================================================================
output <- data.frame(summary_df)
output <- output[c(5:8, 1:4), 2:ncol(output)]
rownames(output) <- NULL
col_names <- c("Ideology", "N", "M", "SD", "t", "p", "Cohen's d")
colnames(output) <- col_names

print(output)
##       Ideology  N    M   SD     t      p Cohen's d
## 1      Liberal 31 0.72 0.25  4.95 < .001      0.89
## 2 Conservative 51 0.43 0.20 -2.60   .012      0.36
## 3      Liberal 38 0.58 0.21  2.42   .021      0.39
## 4 Conservative 43 0.53 0.28  0.66   .513      0.10
## 5      Liberal 33 0.66 0.24  3.82 < .001      0.66
## 6 Conservative 41 0.48 0.22 -0.63   .532      0.10
## 7      Liberal 40 0.46 0.23 -1.16   .254      0.18
## 8 Conservative 36 0.64 0.25  3.30   .002      0.55
