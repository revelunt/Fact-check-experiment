## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates the results of tables 
##              regarding study 3 in appendix 
## INPUT: 
##              ./01_data/FC_study-3.csv
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
df_study3 <- read_csv('./01_data/FC_study-3.csv')


## TABLE C11 ===================================================================
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study3 %>% 
  group_by(media_slant, nfc, partisanship) %>% 
  summarise(
    n = n(), 
    mean_t1 = mean(agree_t1) %>% round(2), 
    sd_t1 = sd(agree_t1) %>% round(2),
    mean_t2 = mean(agree_t2) %>% round(2), 
    sd_t2 = sd(agree_t2) %>% round(2)
  )

tab2 <- df_study3 %>%
  select(media_slant, nfc, partisanship, agree_t1, agree_t2) %>%
  group_by(media_slant, nfc, partisanship) %>% 
  group_modify(~tidy(t.test(.$agree_t2, .$agree_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study3 %>%
  dplyr::select(agree_t1, agree_t2, media_slant, nfc, partisanship) %>%
  group_by(media_slant, nfc, partisanship) %>%
  group_modify(~ tidy(cohensD(.$agree_t2, .$agree_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
col_names <- c("News media", "NfC", "Partisanship", "N", "M(t1)", "SD(t1)", "M(t2)", "SD(t2)", "t", "p", "Cohen's d")
rownames(output) <- NULL
colnames(output) <- col_names

print(output)
##    News media  NfC Partisanship  N M(t1) SD(t1) M(t2) SD(t2)      t      p Cohen's d
## 1         FOX High     Democrat 59  5.69   0.95  4.92   1.47  -5.16 < .001      0.63
## 2         FOX High   Republican 52  6.00   1.37  5.10   1.72  -4.95 < .001      0.58
## 3         FOX  Low     Democrat 64  5.48   1.07  4.70   1.40  -4.89 < .001      0.63
## 4         FOX  Low   Republican 52  5.79   1.39  4.77   2.06  -5.12 < .001      0.58
## 5       MSNBC High     Democrat 60  5.47   1.20  4.32   1.59  -7.23 < .001      0.82
## 6       MSNBC High   Republican 51  5.92   1.20  5.20   1.69  -3.99 < .001      0.50
## 7       MSNBC  Low     Democrat 64  5.30   1.31  4.02   1.55  -7.52 < .001      0.89
## 8       MSNBC  Low   Republican 51  5.82   1.41  5.18   1.77  -2.96   .005      0.40
## 9      Reuter High     Democrat 60  5.37   1.04  3.83   1.68 -10.27 < .001      1.10
## 10     Reuter High   Republican 51  5.82   1.51  5.35   1.86  -2.37   .022      0.28
## 11     Reuter  Low     Democrat 62  5.50   1.36  3.94   1.72  -9.12 < .001      1.01
## 12     Reuter  Low   Republican 52  5.77   1.42  4.69   1.83  -5.79 < .001      0.66


## TABLE C12 ===================================================================
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study3 %>% 
  group_by(media_slant, nfc, partisanship) %>% 
  summarise(
    n = n(), 
    mean_t1 = mean(belief_t1) %>% round(2), 
    sd_t1 = sd(belief_t1) %>% round(2),
    mean_t2 = mean(belief_t2) %>% round(2), 
    sd_t2 = sd(belief_t2) %>% round(2)
  )

tab2 <- df_study3 %>%
  select(media_slant, nfc, partisanship, belief_t1, belief_t2) %>%
  group_by(media_slant, nfc, partisanship) %>% 
  group_modify(~tidy(t.test(.$belief_t2, .$belief_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study3 %>%
  dplyr::select(belief_t1, belief_t2, media_slant, nfc, partisanship) %>%
  group_by(media_slant, nfc, partisanship) %>%
  group_modify(~ tidy(cohensD(.$belief_t2, .$belief_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
col_names <- c("News media", "NfC", "Partisanship", "N", "M(t1)", "SD(t1)", "M(t2)", "SD(t2)", "t", "p", "Cohen's d")
rownames(output) <- NULL
colnames(output) <- col_names

print(output)
#    News media  NfC Partisanship  N M(t1) SD(t1) M(t2) SD(t2)      t      p Cohen's d
# 1         FOX High     Democrat 59  5.63   1.13  4.75   1.61  -4.83 < .001      0.63
# 2         FOX High   Republican 52  5.77   1.25  5.10   1.72  -3.31   .002      0.45
# 3         FOX  Low     Democrat 64  5.52   1.04  4.69   1.44  -5.39 < .001      0.66
# 4         FOX  Low   Republican 52  5.81   1.21  4.77   2.10  -4.59 < .001      0.61
# 5       MSNBC High     Democrat 60  5.55   1.17  4.30   1.48  -7.70 < .001      0.94
# 6       MSNBC High   Republican 51  5.71   1.15  5.04   1.80  -3.31   .002      0.44
# 7       MSNBC  Low     Democrat 64  5.34   1.34  4.08   1.50  -6.98 < .001      0.89
# 8       MSNBC  Low   Republican 51  5.67   1.31  4.94   1.79  -3.66 < .001      0.46
# 9      Reuter High     Democrat 60  5.37   1.18  3.77   1.65 -10.89 < .001      1.12
# 10     Reuter High   Republican 51  5.82   1.21  5.25   1.78  -2.84   .007      0.37
# 11     Reuter  Low     Democrat 62  5.71   1.05  3.87   1.72  -9.54 < .001      1.29
# 12     Reuter  Low   Republican 52  5.50   1.48  4.62   1.87  -3.98 < .001      0.53


## TABLE C13 ===================================================================
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study3 %>% 
  group_by(media_slant, nfc, partisanship) %>% 
  summarise(n = n(), mean = round(mean(hmp_score),2), sd = round(sd(hmp_score),2))

tab2 <- df_study3 %>%
  select(media_slant, nfc, partisanship, hmp_score) %>%
  group_by(media_slant, nfc, partisanship) %>% 
  group_modify(~tidy(t.test(.$hmp_score, mu = 0.5, alt = "two.sided"))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study3 %>%
  select(media_slant, nfc, partisanship, hmp_score) %>%
  group_by(media_slant, nfc, partisanship) %>%
  group_modify(~ tidy(cohensD(.$hmp_score, mu = 0.5))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
rownames(output) <- NULL
col_names <- c("News media", "NfC", "Partisanship", "N", "M", "SD", "t", "p", "Cohen's d")
colnames(output) <- col_names

print(output)
##    News media  NfC Partisanship  N    M   SD    t      p Cohen's d
## 1         FOX High     Democrat 59 0.75 0.25 7.81 < .001      1.02
## 2         FOX High   Republican 52 0.68 0.21 6.11 < .001      0.85
## 3         FOX  Low     Democrat 64 0.75 0.22 9.30 < .001      1.16
## 4         FOX  Low   Republican 52 0.68 0.21 6.22 < .001      0.86
## 5       MSNBC High     Democrat 60 0.58 0.15 4.08 < .001      0.53
## 6       MSNBC High   Republican 51 0.80 0.25 8.70 < .001      1.22
## 7       MSNBC  Low     Democrat 64 0.54 0.14 2.15   .035      0.27
## 8       MSNBC  Low   Republican 51 0.80 0.23 9.41 < .001      1.32
## 9      Reuter High     Democrat 60 0.54 0.15 2.27   .027      0.29
## 10     Reuter High   Republican 51 0.74 0.25 7.02 < .001      0.98
## 11     Reuter  Low     Democrat 62 0.52 0.14 1.13   .263      0.14
## 12     Reuter  Low   Republican 52 0.74 0.21 8.18 < .001      1.13


## TABLE C14 ===================================================================
# Fit regression models
mod1 <- lm_robust(agree_diff ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Democrat')) 
mod2 <- lm_robust(agree_diff ~ nfc*media_slant + gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Democrat')) 
mod3 <- lm_robust(agree_diff ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Republican'))
mod4 <- lm_robust(agree_diff ~ nfc*media_slant + gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Republican'))
mod_list <- list(mod1, mod2, mod3, mod4)

# Print output
screenreg(
  mod_list, custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Republican" = 3:4),
  custom.coef.map = list(
    "nfcLow" = "NfC: Low (vs. High)", 
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. FOX)", "media_slantReuter" = "Delivered by: Reuter (vs. FOX)",
    "nfcLow:media_slantMSNBC" = "Low × MBNBC", "nfcLow:media_slantReuter" = "Low × Reuter",
    "gen_knowledge" = "General knowledge", "media_trust" = "Media trust", 
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "income" = "Income", "(Intercept)" = "Constant"
  ),
  include.ci = FALSE, single.row = TRUE, include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)

## =========================================================================================================
##                                                 Democrats                            Republican            
##                                 ------------------------------------  -----------------------------------
##                                   (1)                (2)                (3)                (4)             
## ---------------------------------------------------------------------------------------------------------
## NfC: Low (vs. High)              -0.00 (0.22)       -0.03 (0.22)       -0.12 (0.27)        0.03 (0.26)   
## Delivered by: MSNBC (vs. FOX)    -0.37 (0.22)       -0.34 (0.22)        0.18 (0.26)        0.29 (0.26)   
## Delivered by: Reuter (vs. FOX)   -0.75 (0.21) ***   -0.74 (0.22) ***    0.43 (0.27)        0.55 (0.26) * 
## Low × MBNBC                      -0.13 (0.32)       -0.10 (0.33)        0.19 (0.39)       -0.12 (0.39)   
## Low × Reuter                     -0.03 (0.32)        0.00 (0.32)       -0.49 (0.38)       -0.62 (0.38)   
## General knowledge                                   -0.08 (0.10)                          -0.12 (0.14)   
## Media trust                                          0.01 (0.05)                          -0.15 (0.07) * 
## Involvement                                          0.10 (0.08)                           0.17 (0.08) * 
## Gender                                              -0.11 (0.12)                          -0.30 (0.17)   
## Age                                                  0.06 (0.05)                          -0.02 (0.06)   
## Education                                            0.03 (0.06)                           0.18 (0.07) **
## Income                                              -0.04 (0.02)                          -0.01 (0.03)   
## Constant                         -0.78 (0.15) ***   -0.79 (0.64)       -0.90 (0.18) ***   -0.87 (0.91)   
## ---------------------------------------------------------------------------------------------------------
## R^2                               0.06               0.08               0.02               0.09          
## Adj. R^2                          0.05               0.05               0.01               0.06          
## Num. obs.                       369                366                309                308             
## =========================================================================================================
## *** p < 0.001; ** p < 0.01; * p < 0.05


## TABLE C15 ===================================================================
# Fit regression models
mod1 <- lm_robust(belief_diff ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Democrat'))
mod2 <- lm_robust(belief_diff ~ nfc*media_slant + gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Democrat'))
mod3 <- lm_robust(belief_diff ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Republican')) 
mod4 <- lm_robust(belief_diff ~ nfc*media_slant+ gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Republican')) 
mod_list <- list(mod1, mod2, mod3, mod4)

# Print output
screenreg(
  mod_list, custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Republican" = 3:4),
  custom.coef.map = list(
    "nfcLow" = "NfC: Low (vs. High)", 
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. FOX)", "media_slantReuter" = "Delivered by: Reuter (vs. FOX)",
    "nfcLow:media_slantMSNBC" = "Low × MBNBC", "nfcLow:media_slantReuter" = "Low × Reuter",
    "gen_knowledge" = "General knowledge", "media_trust" = "Media trust", 
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "income" = "Income", "(Intercept)" = "Constant"
  ),
  include.ci = FALSE, single.row = TRUE, include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)
## =======================================================================================================
##                                              Democrats                           Republican            
##                                 -----------------------------------  ----------------------------------
##                                 (1)                (2)               (3)               (4)             
## -------------------------------------------------------------------------------------------------------
## NfC: Low (vs. High)               0.05 (0.24)        0.04 (0.24)      -0.37 (0.30)      -0.22 (0.28)   
## Delivered by: MSNBC (vs. FOX)    -0.37 (0.24)       -0.34 (0.25)       0.01 (0.29)       0.07 (0.26)   
## Delivered by: Reuter (vs. FOX)   -0.72 (0.23) **    -0.71 (0.24) **    0.10 (0.29)       0.18 (0.28)   
## Low × MBNBC                      -0.07 (0.34)       -0.06 (0.35)       0.31 (0.42)      -0.00 (0.40)   
## Low × Reuter                     -0.29 (0.34)       -0.30 (0.34)       0.05 (0.43)      -0.07 (0.43)   
## General knowledge                                   -0.10 (0.11)                        -0.18 (0.17)   
## Media trust                                         -0.04 (0.06)                        -0.29 (0.09) **
## Involvement                                          0.13 (0.08)                         0.13 (0.09)   
## Gender                                              -0.26 (0.13)                        -0.23 (0.18)   
## Age                                                  0.04 (0.05)                        -0.00 (0.06)   
## Education                                            0.01 (0.06)                         0.11 (0.07)   
## Income                                              -0.01 (0.03)                        -0.01 (0.03)   
## Constant                         -0.88 (0.18) ***   -0.49 (0.64)      -0.67 (0.20) **    0.25 (1.08)   
## -------------------------------------------------------------------------------------------------------
## R^2                               0.07               0.09              0.01              0.09          
## Adj. R^2                          0.06               0.06             -0.01              0.05          
## Num. obs.                       369                366               309               308             
## =======================================================================================================
## *** p < 0.001; ** p < 0.01; * p < 0.05


## TABLE C16 ===================================================================
# Fit regression models
mod1 <- lm_robust(hmp_score ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Democrat'))
mod2 <- lm_robust(hmp_score ~ nfc*media_slant  + gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Democrat'))
mod3 <- lm_robust(hmp_score ~ nfc*media_slant, data = filter(df_study3, partisanship == 'Republican')) 
mod4 <- lm_robust(hmp_score ~ nfc*media_slant + gen_knowledge + media_trust + involvement + gender + age + education + income, data = filter(df_study3, partisanship == 'Republican')) 
mod_list <- list(mod1, mod2, mod3, mod4)

# Print output
screenreg(
  mod_list, custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Republican" = 3:4),
  custom.coef.map = list(
    "nfcLow" = "NfC: Low (vs. High)", 
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. FOX)", "media_slantReuter" = "Delivered by: Reuter (vs. FOX)",
    "nfcLow:media_slantMSNBC" = "Low × MBNBC", "nfcLow:media_slantReuter" = "Low × Reuter",
    "gen_knowledge" = "General knowledge", "media_trust" = "Media trust", 
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "income" = "Income", "(Intercept)" = "Constant"
  ),
  include.ci = FALSE, single.row = TRUE, include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)
## ==========================================================================================================
##                                               Democrats                            Republican             
##                                 ------------------------------------  ------------------------------------
##                                 (1)                (2)                (3)                (4)              
## ----------------------------------------------------------------------------------------------------------
## NfC: Low (vs. High)              -0.00 (0.04)        0.01 (0.04)       -0.00 (0.04)        0.03 (0.04)    
## Delivered by: MSNBC (vs. FOX)    -0.17 (0.04) ***   -0.16 (0.04) ***    0.12 (0.05) **     0.14 (0.04) ***
## Delivered by: Reuter (vs. FOX)   -0.21 (0.04) ***   -0.21 (0.04) ***    0.06 (0.05)        0.08 (0.04) *  
## Low × MBNBC                      -0.04 (0.05)       -0.05 (0.05)       -0.01 (0.06)       -0.08 (0.05)    
## Low × Reuter                     -0.02 (0.05)       -0.01 (0.05)        0.00 (0.06)       -0.03 (0.05)    
## General knowledge                                    0.04 (0.02) *                         0.04 (0.02) *  
## Media trust                                         -0.01 (0.01)                          -0.10 (0.01) ***
## Involvement                                          0.01 (0.01)                           0.02 (0.01)    
## Gender                                              -0.01 (0.02)                          -0.01 (0.02)    
## Age                                                  0.02 (0.01) **                        0.02 (0.01) *  
## Education                                           -0.01 (0.01)                           0.01 (0.01)    
## Income                                              -0.00 (0.00)                           0.00 (0.00)    
## Constant                          0.75 (0.03) ***    0.56 (0.10) ***    0.68 (0.03) ***    0.48 (0.13) ***
## ----------------------------------------------------------------------------------------------------------
## R^2                               0.24               0.29               0.04               0.40           
## Adj. R^2                          0.23               0.26               0.03               0.38           
## Num. obs.                       369                366                309                308              
## ==========================================================================================================
## *** p < 0.001; ** p < 0.01; * p < 0.05

