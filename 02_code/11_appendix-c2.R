## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates the results of tables 
##              regarding study 2 in appendix 
## INPUT: 
##              ./01_data/FC_study-2.csv
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
df_study2 <- read_csv('./01_data/FC_study-2.csv')


## TABLE C5 ====================================================================
# Factor level setting
df_study2 <- df_study2 %>% mutate(
  media_slant = factor(media_slant, levels = c("FOX", "Reuter", "MSNBC"))
)
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study2 %>% 
  group_by(valence, media_slant, partisanship) %>% 
  summarise(
    n = n(), 
    mean_t1 = mean(agree_t1) %>% round(2), 
    sd_t1 = sd(agree_t1) %>% round(2),
    mean_t2 = mean(agree_t2) %>% round(2), 
    sd_t2 = sd(agree_t2) %>% round(2)
  )

tab2 <- df_study2 %>%
  select(valence, media_slant, partisanship, agree_t1, agree_t2) %>%
  group_by(valence, media_slant, partisanship) %>% 
  group_modify(~tidy(t.test(.$agree_t2, .$agree_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study2 %>%
  dplyr::select(agree_t1, agree_t2, valence, media_slant, partisanship) %>%
  group_by(valence, media_slant, partisanship) %>%
  group_modify(~ tidy(cohensD(.$agree_t2, .$agree_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
col_names <- c("Valence", "Media Slant", "Partisanship", "N", "M(t1)", "SD(t1)", "M(t2)", "SD(t2)", "t", "p", "Cohen's d")
rownames(output) <- NULL
colnames(output) <- col_names

print(output)
##    Valence Media Slant Partisanship  N M(t1) SD(t1) M(t2) SD(t2)     t      p Cohen's d
## 1    Biden         FOX     Democrat 61  5.21   1.14  4.28   1.60 -4.62 < .001      0.67
## 2    Biden         FOX  Independent 40  3.95   1.68  3.52   1.66 -1.93   .061      0.25
## 3    Biden         FOX   Republican 53  2.79   1.78  2.28   1.68 -2.58   .013      0.29
## 4    Biden      Reuter     Democrat 58  5.17   1.09  3.72   1.59 -6.93 < .001      1.06
## 5    Biden      Reuter  Independent 41  3.95   1.76  3.15   1.56 -2.95   .005      0.48
## 6    Biden      Reuter   Republican 49  3.39   1.81  2.43   1.63 -4.25 < .001      0.56
## 7    Biden       MSNBC     Democrat 58  5.05   1.28  3.53   1.66 -6.68 < .001      1.03
## 8    Biden       MSNBC  Independent 42  3.93   1.58  2.43   1.29 -6.09 < .001      1.04
## 9    Biden       MSNBC   Republican 52  2.52   1.50  1.85   1.14 -3.44 < .001      0.50
## 10   Ducey         FOX     Democrat 58  2.67   1.50  2.05   1.56 -4.05 < .001      0.41
## 11   Ducey         FOX  Independent 42  4.19   1.89  3.60   2.14 -2.49   .017      0.29
## 12   Ducey         FOX   Republican 51  5.43   1.49  4.14   1.79 -5.63 < .001      0.79
## 13   Ducey      Reuter     Democrat 59  2.97   1.73  2.17   1.68 -3.86 < .001      0.47
## 14   Ducey      Reuter  Independent 41  3.71   1.75  2.95   1.92 -5.00 < .001      0.41
## 15   Ducey      Reuter   Republican 51  5.41   1.55  4.47   1.97 -4.03 < .001      0.53
## 16   Ducey       MSNBC     Democrat 57  2.25   1.24  1.63   1.19 -3.44 < .001      0.50
## 17   Ducey       MSNBC  Independent 40  3.67   1.99  2.88   2.03 -4.74 < .001      0.40
## 18   Ducey       MSNBC   Republican 51  5.59   1.51  4.92   1.85 -3.45 < .001      0.39


## TABLE C6 ====================================================================
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study2 %>% 
  group_by(valence, media_slant, partisanship) %>% 
  summarise(
    n = n(), 
    mean_t1 = mean(belief_t1) %>% round(2), 
    sd_t1 = sd(belief_t1) %>% round(2),
    mean_t2 = mean(belief_t2) %>% round(2), 
    sd_t2 = sd(belief_t2) %>% round(2)
  )

tab2 <- df_study2 %>%
  select(valence, media_slant, partisanship, belief_t1, belief_t2) %>%
  group_by(valence, media_slant, partisanship) %>% 
  group_modify(~tidy(t.test(.$belief_t2, .$belief_t1, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study2 %>%
  dplyr::select(belief_t1, belief_t2, valence, media_slant, partisanship) %>%
  group_by(valence, media_slant, partisanship) %>%
  group_modify(~ tidy(cohensD(.$belief_t2, .$belief_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
col_names <- c("Valence", "News media", "Partisanship", "N", "M(t1)", "SD(t1)", "M(t2)", "SD(t2)", "t", "p", "Cohen's d")
rownames(output) <- NULL
colnames(output) <- col_names

print(output)
##    Valence Media Slant Partisanship  N M(t1) SD(t1) M(t2) SD(t2)     t      p Cohen's d
## 1    Biden         FOX     Democrat 61  5.28   1.19  4.26   1.49 -5.54 < .001      0.75
## 2    Biden         FOX  Independent 40  4.32   1.64  3.55   1.60 -3.80 < .001      0.48
## 3    Biden         FOX   Republican 53  3.08   1.88  2.19   1.58 -4.58 < .001      0.51
## 4    Biden      Reuter     Democrat 58  5.28   1.14  3.57   1.58 -8.48 < .001      1.24
## 5    Biden      Reuter  Independent 41  3.95   1.84  3.05   1.38 -3.43 < .001      0.55
## 6    Biden      Reuter   Republican 49  3.29   1.76  2.24   1.60 -6.12 < .001      0.62
## 7    Biden       MSNBC     Democrat 58  5.34   1.12  3.60   1.67 -7.65 < .001      1.22
## 8    Biden       MSNBC  Independent 42  3.86   1.62  2.52   1.31 -5.61 < .001      0.91
## 9    Biden       MSNBC   Republican 52  2.58   1.64  2.08   1.38 -2.03   .048      0.33
## 10   Ducey         FOX     Democrat 58  3.02   1.48  2.07   1.53 -4.96 < .001      0.63
## 11   Ducey         FOX  Independent 42  4.31   1.80  3.43   2.11 -3.18   .003      0.45
## 12   Ducey         FOX   Republican 51  5.00   1.44  3.90   1.66 -5.70 < .001      0.71
## 13   Ducey      Reuter     Democrat 59  3.03   1.65  2.08   1.73 -4.58 < .001      0.56
## 14   Ducey      Reuter  Independent 41  4.07   1.56  3.02   1.90 -4.88 < .001      0.60
## 15   Ducey      Reuter   Republican 51  5.06   1.53  4.45   1.87 -2.87   .006      0.36
## 16   Ducey       MSNBC     Democrat 57  2.44   1.35  1.74   1.06 -4.38 < .001      0.58
## 17   Ducey       MSNBC  Independent 40  3.75   1.84  2.88   1.96 -4.24 < .001      0.46
## 18   Ducey       MSNBC   Republican 51  5.16   1.50  4.47   1.79 -3.20   .002      0.42


## TABLE C7 ====================================================================
# Combine data frames with the results of the multiple t-tests
tab1 <- df_study2 %>% 
  filter(partisanship != "Independent") %>% 
  group_by(valence, media_slant, partisanship) %>% 
  summarise(n = n(), mean = round(mean(hmp_score),2), sd = round(sd(hmp_score),2))

tab2 <- df_study2 %>%
  filter(partisanship != "Independent") %>% 
  select(valence, media_slant, partisanship, hmp_score) %>%
  group_by(valence, media_slant, partisanship) %>% 
  group_modify(~tidy(t.test(.$hmp_score, mu = 0.5, alt = "two.sided"))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- df_study2 %>%
  select(valence, media_slant, partisanship, hmp_score) %>%
  group_by(valence, media_slant, partisanship) %>%
  group_modify(~ tidy(cohensD(.$hmp_score, mu = 0.5))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3)

# Prints the output
output <- data.frame(summary_df)
rownames(output) <- NULL
col_names <- c("Valence", "News media", "Partisanship", "N", "M", "SD", "t", "p", "Cohen's d")
colnames(output) <- col_names

print(output)
##    Valence News media Partisanship  N    M   SD     t      p Cohen's d
## 1    Biden        FOX     Democrat 61 0.76 0.20 10.13 < .001      1.30
## 2    Biden        FOX   Republican 53 0.43 0.16 -3.07   .003      0.42
## 3    Biden     Reuter     Democrat 58 0.52 0.16  0.89   .378      0.12
## 4    Biden     Reuter   Republican 49 0.50 0.15  0.19   .852      0.03
## 5    Biden      MSNBC     Democrat 58 0.51 0.15  0.30   .765      0.04
## 6    Biden      MSNBC   Republican 52 0.55 0.17  1.99   .052      0.28
## 7    Ducey        FOX     Democrat 58 0.52 0.15  1.22   .228      0.16
## 8    Ducey        FOX   Republican 51 0.60 0.21  3.30   .002      0.46
## 9    Ducey     Reuter     Democrat 59 0.51 0.17  0.62    .54      0.08
## 10   Ducey     Reuter   Republican 51 0.70 0.25  5.72 < .001      0.80
## 11   Ducey      MSNBC     Democrat 57 0.49 0.14 -0.34   .739      0.04
## 12   Ducey      MSNBC   Republican 51 0.67 0.27  4.54 < .001      0.64


## TABLE C8 ====================================================================
# Contrast coding
df_study2$media_slant <- factor(df_study2$media_slant)
df_study2$media_slant <- relevel(df_study2$media_slant, "Reuter")
## contrasts(df_study2$media_slant)
##        FOX MSNBC
## Reuter   0     0
## FOX      1     0
## MSNBC    0     1

# Subsampling based on partisanship
dem_filtered <- df_study2 %>% filter(partisanship == "Democrat")
ind_filtered <- df_study2 %>% filter(partisanship == "Independent")
rep_filtered <- df_study2 %>% filter(partisanship == "Republican")


# Fit regression models
mod1 <- lm_robust(agree_diff ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(agree_diff ~ valence*media_slant, data = ind_filtered)
mod4 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = ind_filtered)
mod5 <- lm_robust(agree_diff ~ valence*media_slant, data = rep_filtered)
mod6 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


# Print outputs
screenreg(
  mod_list,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Independents" = 3:4, "Republican" = 5:6),
  custom.coef.map = list(
    "valenceDucey" = "Debunking: Ducey (vs. Biden)", 
    "media_slantFOX" = "Delivered by: FOX (vs. Reuter)",
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. Reuter)",
    "valenceDucey:media_slantFOX" = "Ducey × FOX",
    "valenceDucey:media_slantMSNBC" = "Ducey × MSNBC",
    "gen_knowledge" = "General knowledge",
    "immig_knowledge" = "Immigration knowledge", "media_trust" = "Media trust",
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "(Intercept)" = "Constant"
  ),
  custom.note = "Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
  parentheses. (* p < .05; ** p < .01; *** p < .001) ",
  include.ci = FALSE, single.row = TRUE,
  include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)
## =============================================================================================================================================
##                                                  Democrats                         Independents                          Republican            
##                                   ----------------------------------  ----------------------------------  -----------------------------------
##                                     (1)                (2)              (3)               (4)               (5)                (6)             
## ---------------------------------------------------------------------------------------------------------------------------------------------
## Debunking: Ducey (vs. Biden)        0.65 (0.29) *      0.66 (0.30) *    0.05 (0.31)       0.00 (0.34)       0.02 (0.32)       -0.04 (0.33)   
## Delivered by: FOX (vs. Reuter)      0.51 (0.29)        0.52 (0.29)      0.38 (0.35)       0.38 (0.39)       0.45 (0.30)        0.35 (0.32)   
## Delivered by: MSNBC (vs. Reuter)   -0.07 (0.31)       -0.07 (0.31)     -0.70 (0.37)      -0.83 (0.40) *     0.29 (0.30)        0.27 (0.29)   
## Ducey × FOX                        -0.34 (0.39)       -0.34 (0.39)     -0.22 (0.45)      -0.29 (0.50)      -0.80 (0.44)       -0.65 (0.46)   
## Ducey × MSNBC                       0.25 (0.41)        0.21 (0.41)      0.65 (0.43)       0.75 (0.46)      -0.01 (0.43)       -0.04 (0.41)   
## General knowledge                                     -0.11 (0.11)                       -0.15 (0.13)                         -0.19 (0.15)   
## Immigration knowledge                                  0.01 (0.09)                        0.11 (0.10)                         -0.08 (0.12)   
## Media trust                                           -0.02 (0.07)                       -0.24 (0.08) **                      -0.02 (0.09)   
## Involvement                                            0.17 (0.08) *                      0.02 (0.10)                          0.22 (0.08) **
## Gender                                                -0.14 (0.17)                        0.10 (0.17)                         -0.27 (0.18)   
## Age                                                    0.05 (0.07)                        0.06 (0.07)                         -0.04 (0.07)   
## Education                                              0.04 (0.07)                       -0.02 (0.08)                         -0.04 (0.07)   
## Constant                           -1.45 (0.21) ***   -1.63 (0.78) *   -0.80 (0.27) **    0.14 (1.08)      -0.96 (0.23) ***    0.22 (1.07)   
## ---------------------------------------------------------------------------------------------------------------------------------------------
## R^2                                 0.06               0.08             0.05              0.13              0.03               0.07          
## Adj. R^2                            0.04               0.04             0.04              0.08              0.01               0.03          
## Num. obs.                         351                351              246               246               307                307             
## =============================================================================================================================================
## Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
## parentheses. (* p < .05; ** p < .01; *** p < .001) 


## TABLE C9 ====================================================================
# Fit regression models
mod1 <- lm_robust(belief_diff ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(belief_diff ~ valence*media_slant, data = ind_filtered)
mod4 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = ind_filtered)
mod5 <- lm_robust(belief_diff ~ valence*media_slant, data = rep_filtered)
mod6 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


# Print output
screenreg(
  mod_list,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Independents" = 3:4, "Republican" = 5:6),
  custom.coef.map = list(
    "valenceDucey" = "Debunking: Ducey (vs. Biden)", 
    "media_slantFOX" = "Delivered by: FOX (vs. Reuter)",
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. Reuter)",
    "valenceDucey:media_slantFOX" = "Ducey × FOX",
    "valenceDucey:media_slantMSNBC" = "Ducey × MSNBC",
    "gen_knowledge" = "General knowledge",
    "immig_knowledge" = "Immigration knowledge", "media_trust" = "Media trust",
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "income" = "Income", "(Intercept)" = "Constant"
  ),
  custom.note = "Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
  parentheses. (* p < .05; ** p < .01; *** p < .001) ",
  include.ci = FALSE, single.row = TRUE,
  include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)
## ================================================================================================================================================
##                                                  Democrats                           Independents                          Republican            
##                                   ------------------------------------  ------------------------------------  ----------------------------------
##                                     (1)                (2)                (3)                (4)                (5)                (6)            
## ------------------------------------------------------------------------------------------------------------------------------------------------
## Debunking: Ducey (vs. Biden)        0.76 (0.29) **     0.75 (0.30) *     -0.15 (0.34)       -0.21 (0.33)        0.43 (0.27)        0.42 (0.27)  
## Delivered by: FOX (vs. Reuter)      0.69 (0.27) *      0.69 (0.27) *      0.13 (0.33)        0.15 (0.34)        0.15 (0.26)        0.07 (0.27)  
## Delivered by: MSNBC (vs. Reuter)   -0.03 (0.30)       -0.00 (0.30)       -0.43 (0.35)       -0.63 (0.37)        0.54 (0.30)        0.51 (0.29)  
## Ducey × FOX                        -0.69 (0.39)       -0.64 (0.39)        0.04 (0.48)       -0.07 (0.48)       -0.64 (0.38)       -0.60 (0.39)  
## Ducey × MSNBC                       0.28 (0.40)        0.26 (0.40)        0.60 (0.46)        0.74 (0.47)       -0.62 (0.42)       -0.70 (0.43)  
## General knowledge                                      0.02 (0.12)                          -0.17 (0.13)                          -0.03 (0.15)  
## Immigration knowledge                                  0.02 (0.09)                           0.15 (0.10)                           0.02 (0.11)  
## Media trust                                           -0.02 (0.07)                          -0.36 (0.07) ***                      -0.07 (0.08)  
## Involvement                                            0.12 (0.08)                           0.03 (0.09)                           0.18 (0.09) *
## Gender                                                -0.09 (0.17)                           0.21 (0.18)                          -0.20 (0.17)  
## Age                                                    0.04 (0.07)                           0.10 (0.07)                          -0.07 (0.07)  
## Education                                              0.08 (0.08)                          -0.02 (0.08)                           0.02 (0.07)  
## Income                                                 0.03 (0.03)                          -0.05 (0.03)                          -0.03 (0.03)  
## Constant                           -1.71 (0.20) ***   -2.70 (0.80) ***   -0.90 (0.26) ***    0.10 (1.01)       -1.04 (0.17) ***   -0.61 (1.09)  
## ------------------------------------------------------------------------------------------------------------------------------------------------
## R^2                                 0.07               0.09               0.01               0.16               0.02               0.06         
## Adj. R^2                            0.05               0.05              -0.01               0.12               0.01               0.02         
## Num. obs.                         351                351                246                246                307                307            
## ================================================================================================================================================
## Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
## parentheses. (* p < .05; ** p < .01; *** p < .001) 


## TABLE C10 ===================================================================
# Fit regression models
mod1 <- lm_robust(hmp_score ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(hmp_score ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(hmp_score ~ valence*media_slant, data = rep_filtered)
mod4 <- lm_robust(hmp_score ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)
mod5 <- lm_robust(hmp_score ~ valence*media_slant*partisanship, data = partisan_filtered)
mod6 <- lm_robust(hmp_score ~ valence*media_slant*partisanship + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = partisan_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


# Print output
screenreg(
  mod_list,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Republican" = 3:4, "Partisan combined" = 5:6),
  custom.coef.map = list(
    "valenceDucey" = "Debunking: Ducey (vs. Biden)", 
    "media_slantFOX" = "Delivered by: FOX (vs. Reuter)",
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. Reuter)",
    "partisanshipRepublican" = "Republican (vs. Democrats)",
    "valenceDucey:media_slantFOX" = "Ducey × FOX",
    "valenceDucey:media_slantMSNBC" = "Ducey × MSNBC",
    "valenceDucey:partisanshipRepublican" = "Ducey × Republican",
    "media_slantFOX:partisanshipRepublican" = "FOX × Republican",
    "media_slantMSNBC:partisanshipRepublican" = "MSNBC × Republican",
    "valenceDucey:media_slantFOX:partisanshipRepublican" = "Ducey × FOX × Republican",
    "valenceDucey:media_slantMSNBC:partisanshipRepublican" = "Ducey x MSNBC x Republican",
    "gen_knowledge" = "General knowledge",
    "immig_knowledge" = "Immigration knowledge", "media_trust" = "Media trust",
    "involvement" = "Involvement", "gender" = "Gender", "age" = "Age", 
    "education" = "Education", "income" = "Income", "(Intercept)" = "Constant"
  ),
  custom.note = "Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
  parentheses. (* p < .05; ** p < .01; *** p < .001). (-) value of dependent variable denotes lesser bias perception against one’s in-group, whereas 
  (+) value of dependent variable denotes greater bias perception against one’s in-group (i.e., perceiving greater favorability towards out-groups). ",
  include.ci = FALSE, single.row = TRUE,
  include.rmse = FALSE, include.rs = TRUE, include.adj = TRUE
)
## ==================================================================================================================================================
##                                                 Democrats                            Republican                         Partisan combined         
##                                   ------------------------------------  ------------------------------------  ------------------------------------
##                                     (1)                (2)                (3)                (4)                (5)                (6)              
## --------------------------------------------------------------------------------------------------------------------------------------------------
## Debunking: Ducey (vs. Biden)       -0.00 (0.03)       -0.00 (0.03)        0.20 (0.04) ***    0.20 (0.03) ***   -0.00 (0.03)       -0.01 (0.03)    
## Delivered by: FOX (vs. Reuter)      0.24 (0.03) ***    0.24 (0.03) ***   -0.07 (0.03) *     -0.08 (0.03) *      0.24 (0.03) ***    0.24 (0.03) ***
## Delivered by: MSNBC (vs. Reuter)   -0.01 (0.03)       -0.01 (0.03)        0.04 (0.03)        0.03 (0.03)       -0.01 (0.03)       -0.02 (0.03)    
## Republican (vs. Democrats)                                                                                     -0.01 (0.03)       -0.08 (0.03) *  
## Ducey × FOX                        -0.23 (0.04) ***   -0.23 (0.04) ***   -0.03 (0.05)       -0.06 (0.05)       -0.23 (0.04) ***   -0.22 (0.05) ***
## Ducey × MSNBC                      -0.01 (0.04)       -0.01 (0.04)       -0.07 (0.06)       -0.08 (0.05)       -0.01 (0.04)       -0.01 (0.04)    
## Ducey × Republican                                                                                              0.20 (0.05) ***    0.21 (0.05) ***
## FOX × Republican                                                                                               -0.31 (0.04) ***   -0.31 (0.04) ***
## MSNBC × Republican                                                                                              0.06 (0.04)        0.05 (0.04)    
## Ducey × FOX × Republican                                                                                        0.19 (0.07) **     0.17 (0.07) *  
## Ducey x MSNBC x Republican                                                                                     -0.06 (0.07)       -0.07 (0.07)    
## General knowledge                                     -0.01 (0.01)                           0.03 (0.02)                           0.01 (0.01)    
## Immigration knowledge                                  0.01 (0.01)                           0.01 (0.01)                           0.01 (0.01)    
## Media trust                                           -0.00 (0.01)                          -0.05 (0.01) ***                      -0.03 (0.01) ***
## Involvement                                            0.00 (0.01)                           0.02 (0.01)                           0.02 (0.01) *  
## Gender                                                -0.00 (0.02)                          -0.01 (0.02)                          -0.00 (0.01)    
## Age                                                   -0.00 (0.01)                           0.01 (0.01)                           0.01 (0.01)    
## Education                                              0.01 (0.01)                          -0.01 (0.01)                           0.00 (0.01)    
## Income                                                -0.00 (0.00)                          -0.00 (0.00)                          -0.00 (0.00)    
## Constant                            0.52 (0.02) ***    0.53 (0.07) ***    0.50 (0.02) ***    0.42 (0.12) ***    0.52 (0.02) ***    0.52 (0.07) ***
## --------------------------------------------------------------------------------------------------------------------------------------------------
## R^2                                 0.25               0.27               0.17               0.36               0.21               0.27           
## Adj. R^2                            0.24               0.24               0.16               0.33               0.20               0.25           
## Num. obs.                         351                351                307                307                658                658              
## ==================================================================================================================================================
## Note. Ducey = Debunking misinformation in Dough Ducey’s statement. Each cell includes regression coefficient and standard errors are in 
## parentheses. (* p < .05; ** p < .01; *** p < .001). (-) value of dependent variable denotes lesser bias perception against one’s in-group, whereas 
## (+) value of dependent variable denotes greater bias perception against one’s in-group (i.e., perceiving greater favorability towards out-groups). 



