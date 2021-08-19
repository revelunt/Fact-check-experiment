## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Appendix Table B1
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a table with the data in Appendix Table B1 of the paper
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
  summarise(
    n = n(), 
    mean_t1 = mean(agree_t1) %>% round(2), 
    sd_t1 = sd(agree_t1) %>% round(2),
    mean_t2 = mean(agree_t2) %>% round(2), 
    sd_t2 = sd(agree_t2) %>% round(2)
    )

tab2 <- mydata %>%
  dplyr::select(ideology_discrete, group, agree_t1, agree_t2) %>%
  group_by(ideology_discrete, group) %>%
  group_modify(~tidy(t.test(.$agree_t2, .$agree_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- mydata %>%
  dplyr::select(agree_t1, agree_t2, group, ideology_discrete) %>%
  group_by(group, ideology_discrete) %>%
  group_modify(~ tidy(cohensD(.$agree_t2, .$agree_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)


## OUTPUT ======================================================================
output <- data.frame(summary_df)
output <- output[c(7:12, 1:6), 2:ncol(output)]
col_names <- c("Ideology", "N", "M(t1)", "SD(t1)", "M(t2)", "SD(t2)", "t", "p", "Cohen's d")
rownames(output) <- NULL
colnames(output) <- col_names

print(output)
##        Ideology  N M(t1) SD(t1) M(t2) SD(t2)     t      p Cohen's d
## 1       Liberal 31  4.65   1.43  4.10   1.40 -3.07   .005      0.39
## 2      Moderate 55  3.31   1.35  3.11   1.50 -1.09   .282      0.14
## 3  Conservative 51  2.14   1.34  2.16   1.47  0.17   .868      0.01
## 4       Liberal 38  4.42   1.48  3.92   1.63 -2.84   .007      0.32
## 5      Moderate 49  3.37   1.58  2.94   1.45 -3.37 < .001      0.28
## 6  Conservative 43  2.51   1.61  2.70   1.70  1.35   .186      0.11
## 7       Liberal 33  2.42   1.58  2.45   1.35  0.21   .839      0.02
## 8      Moderate 58  4.17   1.96  3.67   1.68 -3.58 < .001      0.27
## 9  Conservative 41  5.80   1.60  4.98   1.70 -4.86 < .001      0.50
## 10      Liberal 40  2.33   1.53  2.17   1.53 -0.69   .492      0.10
## 11     Moderate 56  3.98   1.92  3.59   1.90 -3.39 < .001      0.21
## 12 Conservative 36  5.58   1.66  4.78   1.53 -5.08 < .001      0.50

