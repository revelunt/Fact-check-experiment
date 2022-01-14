## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates the results of tables 
##              regarding study 1 in appendix 
## INPUT: 
##              ./01_data/FC_study-1.csv
## OUTPUT: 
##              Prints the results of tables in appendix 
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  broom,         # tidy() 
  conflicted,    # Avoid conflict of functions with same names
  estimatr,      # lm_robust()
  lsr,           # CohenD() 
  texreg,        # screenreg()
  tidyverse      # Tidyverse umbrella package 
)  

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


## IMPORT DATA =================================================================
df_study1 <- read_csv('./01_data/FC_study-1.csv')


## TABLE C1 ====================================================================
# Factor level setting
df_study1 <- df_study1 %>% mutate(ideology_discrete = factor(ideology_discrete, levels = c("Liberal", "Moderate", "Conservative")))

# Combine data frames with the results of the multiple t-tests
tab1 <- df_study1 %>% 
  group_by(group, ideology_discrete) %>% 
  summarise(
    n = n(), 
    mean_t1 = mean(agree_t1) %>% round(2), 
    sd_t1 = sd(agree_t1) %>% round(2),
    mean_t2 = mean(agree_t2) %>% round(2), 
    sd_t2 = sd(agree_t2) %>% round(2)
  )

tab2 <- df_study1 %>%
  dplyr::select(ideology_discrete, group, agree_t1, agree_t2) %>%
  group_by(ideology_discrete, group) %>%
  group_modify(~tidy(t.test(.$agree_t2, .$agree_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study1 %>%
  dplyr::select(agree_t1, agree_t2, group, ideology_discrete) %>%
  group_by(group, ideology_discrete) %>%
  group_modify(~ tidy(cohensD(.$agree_t2, .$agree_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
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


## TABLE C2 ====================================================================
tab1 <- df_study1 %>% 
  filter(ideology_discrete != "Moderate") %>% 
  group_by(group, ideology_discrete) %>% 
  summarise(n = n(), mean = round(mean(hmp_score),2), sd = round(sd(hmp_score),2))

tab2 <- df_study1 %>%
  filter(ideology_discrete != "Moderate") %>% 
  dplyr::select(ideology_discrete, group, hmp_score) %>%
  group_by(ideology_discrete, group) %>%
  group_modify(~tidy(t.test(.$hmp_score, mu = 0.5, alt = "two.sided"))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0") | (p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study1 %>%
  dplyr::select(ideology_discrete, group, hmp_score) %>%
  group_by(group, ideology_discrete) %>%
  group_modify(~ tidy(cohensD(.$hmp_score, mu = 0.5))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3) 

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


## TABLE C3 ====================================================================
# Recode the variables
df_study1$media_slant <- as.factor(df_study1$media_slant)
contrasts(df_study1$media_slant) <- contr.treatment(2, base = 1)
colnames(contrasts(df_study1$media_slant)) <- c("Liberal Media")

df_study1$valence <- as.factor(df_study1$valence)
contrasts(df_study1$valence) <- contr.treatment(2, base = 1)
colnames(contrasts(df_study1$valence)) <- c("Liberal-consistent")

df_study1$ideology_discrete <- as.factor(df_study1$ideology_discrete)
contrasts(df_study1$ideology_discrete) <- contr.treatment(3, base = 3)
colnames(contrasts(df_study1$ideology_discrete)) <- c("Liberal", "Moderate")

df_study1_liberal <- filter(df_study1, ideology_discrete == "Liberal")
df_study1_moderate <- filter(df_study1, ideology_discrete == "Moderate")
df_study1_conservative <- filter(df_study1, ideology_discrete == "Conservative")
df_study1_partisan <- filter(df_study1, ideology_discrete != "Moderate")

df_study1_partisan$valence <- as.factor(df_study1_partisan$valence)
contrasts(df_study1_partisan$valence) <- contr.treatment(2, base = 1)
colnames(contrasts(df_study1_partisan$valence)) <- c("Liberal-consistent")

df_study1_partisan$ideology_discrete <- factor(df_study1_partisan$ideology_discrete, levels = c("Liberal", "Conservative"))
contrasts(df_study1_partisan$ideology_discrete) <- contr.treatment(2, base = 2)
colnames(contrasts(df_study1_partisan$ideology_discrete)) <- c("Liberal")

# Fit the models
mod1 <- lm_robust(agree_diff ~ valence*media_slant, data = df_study1_liberal)
mod2 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, df_study1_liberal)
mod3 <- lm_robust(agree_diff ~ valence*media_slant, data = df_study1_moderate)
mod4 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, df_study1_moderate)
mod5 <- lm_robust(agree_diff ~ valence*media_slant, data = df_study1_conservative)
mod6 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, df_study1_conservative)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)

# Prints the output
screenreg(
  mod_list,
  include.ci = FALSE, booktabs = TRUE, single.row = TRUE,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Liberals" = 1:2, "Moderates" = 3:4, "Conservatives" = 5:6),
  custom.coef.map = list(
    "valenceLiberal-consistent" = "Debunking: LC", 
    "media_slantLiberal Media" = "fact-check by LM",
    "valenceLiberal-consistent:media_slantLiberal Media" = "LC X LM",
    "involvement" = "Involvement",  "media_trust" = "Media Trust",
    "age" = "Age", "education" = "Education",
    "genderFemale" = "Female", "(Intercept)" = "Constant"
  ),
  include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE,
  custom.note = "Note: * p < .05; ** p < .01; *** p < .001. LC = Liberal-Consistent misinformation; LM = fact-checking by Liberal Media. 
  Robust standard errors (HC2) are in parentheses. (-) value of dependent variable denotes persuasive effect 
  (i.e., reduced agreement towards initial misperception), whereas (+) value of dependent variable denotes backfire effect 
  (i.e., increased agreement towards initial misperception). "
)

## ==========================================================================================================================
##                     Liberals                          Moderates                         Conservatives           
##                   --------------------------------  --------------------------------  ------------------------------------
##                     (1)              (2)              (3)                (4)            (5)                (6)              
## --------------------------------------------------------------------------------------------------------------------------
## Debunking: LC      -0.58 (0.23) *   -0.54 (0.25) *    0.30 (0.23)        0.29 (0.24)    0.85 (0.21) ***    0.85 (0.21) ***
## fact-check by LM   -0.18 (0.26)     -0.13 (0.27)      0.11 (0.18)        0.11 (0.19)    0.02 (0.23)        0.08 (0.23)    
## LC X LM             0.23 (0.36)      0.16 (0.39)     -0.34 (0.29)       -0.32 (0.30)    0.14 (0.30)        0.14 (0.29)    
## Involvement                          0.18 (0.09) *                      -0.01 (0.06)                      -0.03 (0.04)    
## Media Trust                          0.03 (0.09)                         0.08 (0.06)                      -0.02 (0.07)    
## Age                                 -0.04 (0.07)                        -0.04 (0.06)                       0.09 (0.05)    
## Education                            0.26 (0.24)                         0.04 (0.14)                       0.01 (0.10)    
## Female                               0.13 (0.18)                        -0.07 (0.15)                       0.12 (0.14)    
## Constant            0.03 (0.15)     -2.02 (1.30)     -0.50 (0.14) ***   -0.66 (0.77)   -0.83 (0.17) ***   -0.99 (0.62)    
## --------------------------------------------------------------------------------------------------------------------------
## R^2                 0.04             0.10             0.01               0.02           0.19               0.22           
## Adj. R^2            0.02             0.04            -0.00              -0.02           0.18               0.18           
## Num. obs.         142              142              218                218            171                171              
## ==========================================================================================================================
## Note: * p < .05; ** p < .01; *** p < .001. LC = Liberal-Consistent misinformation; LM = fact-checking by Liberal Media. 
##   Robust standard errors (HC2) are in parentheses. (-) value of dependent variable denotes persuasive effect 
##   (i.e., reduced agreement towards initial misperception), whereas (+) value of dependent variable denotes backfire effect 
##   (i.e., increased agreement towards initial misperception). 


## TABLE C4 ====================================================================
# Fit the models
mod1 <- lm_robust(hmp_score ~ valence*media_slant, data = df_study1_liberal)
mod2 <- lm_robust(hmp_score ~ valence*media_slant + involvement + media_trust+ gender + age + education, data = df_study1_liberal)
mod3 <- lm_robust(hmp_score ~ valence*media_slant, data = df_study1_conservative)
mod4 <- lm_robust(hmp_score ~ valence*media_slant + involvement + media_trust+ gender + age + education, data = df_study1_conservative)
mod5 <- lm_robust(hmp_score ~ valence*media_slant*ideology_discrete, data = df_study1_partisan)
mod6 <- lm_robust(hmp_score ~ valence*media_slant*ideology_discrete + involvement + media_trust+ gender + age + education, data = df_study1_partisan)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


# Print the output
screenreg(
  mod_list,
  include.ci = FALSE, booktabs = TRUE, single.row = TRUE, 
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Liberals" = 1:2, "Conservatives" = 3:4, "Combined Partisan" = 5:6),
  custom.coef.map = list(
    "valenceLiberal-consistent" = "Debunking: LC", 
    "media_slantLiberal Media" = "fact-check by LM",
    "ideology_discreteLiberal" = "Liberals (vs. conservatives)",
    "valenceLiberal-consistent:media_slantLiberal Media" = "LC × LM",
    "valenceLiberal-consistent:ideology_discreteLiberal" = "LC × Liberals",
    "media_slantLiberal Media:ideology_discreteLiberal" = "LM × Liberals",
    "valenceLiberal-consistent:media_slantLiberal Media:ideology_discreteLiberal" = "LC × LM × Liberals",
    "media_trust" = "Media Trust", "involvement" = "Involvement", 
    "genderFemale" = "Female", "age" = "Age", "education" = "Education",
    "(Intercept)" = "Constant"
  ),
  include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE,
  custom.note = "Note: * p < .05; ** p < .01; *** p < .001. LC = Liberal-Consistent misinformation; LM = fact-checking by Liberal Media. 
  Robust standard errors (HC2) are in parentheses. (+) value of dependent variable denotes higher bias perception against their partisan in-group."
)
## ==============================================================================================================================================
##                                             Liberals                            Conservatives                       Combined Partisan         
##                               ------------------------------------  ------------------------------------  ------------------------------------
##                               (1)                (2)                (3)                (4)                (5)                (6)              
## ----------------------------------------------------------------------------------------------------------------------------------------------
## Debunking: LC                   0.06 (0.06)        0.05 (0.06)       -0.05 (0.04)       -0.04 (0.04)       -0.05 (0.04)       -0.06 (0.04)    
## fact-check by LM               -0.20 (0.06) ***   -0.20 (0.05) ***    0.16 (0.05) **     0.18 (0.06) **     0.16 (0.05) **     0.15 (0.05) ** 
## Liberals (vs. conservatives)                                                                                0.18 (0.05) ***    0.15 (0.05) ** 
## LC × LM                         0.06 (0.08)        0.08 (0.08)       -0.06 (0.07)       -0.08 (0.08)       -0.06 (0.07)       -0.05 (0.08)    
## LC × Liberals                                                                                               0.11 (0.08)        0.11 (0.08)    
## LM × Liberals                                                                                              -0.36 (0.08) ***   -0.36 (0.08) ***
## LC × LM × Liberals                                                                                          0.12 (0.11)        0.13 (0.11)    
## Media Trust                                       -0.04 (0.02) **                       -0.00 (0.02)                          -0.02 (0.01) *  
## Involvement                                        0.05 (0.02) **                       -0.02 (0.01)                           0.00 (0.01)    
## Female                                             0.02 (0.04)                           0.03 (0.04)                           0.02 (0.03)    
## Age                                                0.00 (0.02)                          -0.03 (0.01) *                        -0.01 (0.01)    
## Education                                         -0.02 (0.04)                           0.05 (0.02)                           0.03 (0.02)    
## Constant                        0.66 (0.04) ***    0.53 (0.18) **     0.48 (0.03) ***    0.47 (0.14) ***    0.48 (0.03) ***    0.44 (0.12) ***
## ----------------------------------------------------------------------------------------------------------------------------------------------
## R^2                             0.16               0.27               0.10               0.16               0.15               0.18           
## Adj. R^2                        0.14               0.23               0.08               0.12               0.13               0.15           
## Num. obs.                     142                142                171                171                313                313              
## ==============================================================================================================================================
## Note: * p < .05; ** p < .01; *** p < .001. LC = Liberal-Consistent misinformation; LM = fact-checking by Liberal Media. 
## Robust standard errors (HC2) are in parentheses. (+) value of dependent variable denotes higher bias perception against their partisan in-group.



