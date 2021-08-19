## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Appendix Table B2
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a table with the data in Appendix Table B2 of the paper
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
  group_by(group, ideology_discrete) %>% 
  summarise(n = n(), mean = round(mean(bias),2), sd = round(sd(bias),2))

tab2 <- mydata %>%
  dplyr::select(ideology_discrete, group, bias) %>%
  group_by(ideology_discrete, group) %>%
  group_modify(~tidy(t.test(.$bias, mu = 0, alt = "two.sided"))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0") | (p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- mydata %>%
  dplyr::select(ideology_discrete, group, bias) %>%
  group_by(group, ideology_discrete) %>%
  group_modify(~ tidy(cohensD(.$bias))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)


## OUTPUT ======================================================================
output <- data.frame(summary_df)
output <- output[c(7:12, 1:6), 2:ncol(output)]
rownames(output) <- NULL
col_names <- c("Ideology", "N", "M", "SD", "t", "p", "Cohen's d")
colnames(output) <- col_names

print(output)
##        Ideology  N     M   SD     t      p Cohen's d
## 1       Liberal 31  0.22 0.25  4.95 < .001      0.89
## 2      Moderate 55  0.08 0.22  2.64   .011      0.36
## 3  Conservative 51  0.07 0.20  2.60   .012      0.36
## 4       Liberal 38  0.08 0.21  2.42   .021      0.39
## 5      Moderate 49 -0.03 0.20 -1.12   .269      0.16
## 6  Conservative 43 -0.03 0.28 -0.66   .513      0.10
## 7       Liberal 33  0.16 0.24  3.82 < .001      0.66
## 8      Moderate 58  0.05 0.22  1.83   .073      0.24
## 9  Conservative 41  0.02 0.22  0.63   .532      0.10
## 10      Liberal 40 -0.04 0.23 -1.16   .254      0.18
## 11     Moderate 56 -0.03 0.22 -1.18   .245      0.16
## 12 Conservative 36 -0.14 0.25 -3.30   .002      0.55
