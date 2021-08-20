## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 5 of Study 2 in the paper
## INPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
## OUTPUT: 
##              Prints a table with the data in Table 5 of the paper
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
##        FOX MSNBC
## Reuter   0     0
## FOX      1     0
## MSNBC    0     1

# Subsampling based on partisanship
dem_filtered <- mydata %>% filter(partisanship == "Democrat")
ind_filtered <- mydata %>% filter(partisanship == "Independent")
rep_filtered <- mydata %>% filter(partisanship == "Republican")


## FIT REGRESSION MODELS========================================================
mod1 <- lm_robust(agree_diff ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(agree_diff ~ valence*media_slant, data = ind_filtered)
mod4 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = ind_filtered)
mod5 <- lm_robust(agree_diff ~ valence*media_slant, data = rep_filtered)
mod6 <- lm_robust(agree_diff ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


## OUTPUT=======================================================================
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

