## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 6 of Study 2 in the paper
## INPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
## OUTPUT: 
##              Prints a table with the data in Table 6 of the paper
##
## =============================================================================


## IMPORT PACKAGES==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  estimatr,       # HC2 robust SE linear regression
  texreg,         # Print regression table
  tidyverse       # Tidyverse umbrella package
)  


## IMPORT DATA==================================================================
INPUT_DATA_PATH <- "./Study 2/03_main-analysis/data/processed-data_study-2.csv"
mydata <- read_csv(INPUT_DATA_PATH)


## RECODE & SUBSAMPLES==========================================================
# Contrast coding
mydata$media_slant <- factor(mydata$media_slant)
mydata$media_slant <- relevel(mydata$media_slant, "Reuter")
## contrasts(mydata$media_slant)
##        Fox MSNBC
## Reuter   0     0
## Fox      1     0
## MSNBC    0     1

# Subsampling based on partisanship
dem_filtered <- mydata %>% filter(partisanship == "Democrat")
ind_filtered <- mydata %>% filter(partisanship == "Independent")
rep_filtered <- mydata %>% filter(partisanship == "Republican")


## FIT REGRESSION MODELS========================================================
mod1 <- lm_robust(belief_diff ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(belief_diff ~ valence*media_slant, data = ind_filtered)
mod4 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = ind_filtered)
mod5 <- lm_robust(belief_diff ~ valence*media_slant, data = rep_filtered)
mod6 <- lm_robust(belief_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


## OUTPUT=======================================================================
screenreg(
  mod_list,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Independents" = 3:4, "Republican" = 5:6),
  custom.coef.map = list(
    "valenceDucey" = "Debunking: Ducey (vs. Biden)", 
    "media_slantFox" = "Delivered by: FOX (vs. Reuter)",
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. Reuter)",
    "valenceDucey:media_slantFox" = "Ducey × Fox",
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
## Ducey × Fox                        -0.69 (0.39)       -0.64 (0.39)        0.04 (0.48)       -0.07 (0.48)       -0.64 (0.38)       -0.60 (0.39)  
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


