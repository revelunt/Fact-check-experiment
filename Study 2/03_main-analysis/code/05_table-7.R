## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 7 of Study 2 in the paper
## INPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
## OUTPUT: 
##              Prints a table with the data in Table 7 of the paper
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  estimatr,       # HC2 robust SE linear regression
  texreg,         # Print regression table
  tidyverse       # Tidyverse umbrella package
)  


## IMPORT DATA =================================================================
INPUT_DATA_PATH <- "./Study 2/03_main-analysis/data/processed-data_study-2.csv"
mydata <- read_csv(INPUT_DATA_PATH)


## RECODE & SUBSAMPLES =========================================================
# Contrast coding: Political slant of news media
mydata$media_slant <- factor(mydata$media_slant)
mydata$media_slant <- relevel(mydata$media_slant, "Reuter")
## > contrasts(mydata$media_slant)
##        Fox MSNBC
## Reuter   0     0
## Fox      1     0
## MSNBC    0     1

# Sub-sampling based on partisanship
dem_filtered <- mydata %>% filter(partisanship == "Democrat")
rep_filtered <- mydata %>% filter(partisanship == "Republican")
partisan_filtered <- mydata %>% filter(partisanship != "Independent")


## FIT REGRESSION MODELS =======================================================
mod1 <- lm_robust(hmp_score ~ valence*media_slant, data = dem_filtered)
mod2 <- lm_robust(hmp_score ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = dem_filtered)
mod3 <- lm_robust(hmp_score ~ valence*media_slant, data = rep_filtered)
mod4 <- lm_robust(hmp_score ~ valence*media_slant + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = rep_filtered)
mod5 <- lm_robust(hmp_score ~ valence*media_slant*partisanship, data = partisan_filtered)
mod6 <- lm_robust(hmp_score ~ valence*media_slant*partisanship + gen_knowledge + immig_knowledge + media_trust + involvement + gender + age + education + income, data = partisan_filtered)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


## OUTPUT ======================================================================
screenreg(
  mod_list,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  custom.header = list("Democrats" = 1:2, "Republican" = 3:4, "Partisan combined" = 5:6),
  custom.coef.map = list(
    "valenceDucey" = "Debunking: Ducey (vs. Biden)", 
    "media_slantFox" = "Delivered by: FOX (vs. Reuter)",
    "media_slantMSNBC" = "Delivered by: MSNBC (vs. Reuter)",
    "partisanshipRepublican" = "Republican (vs. Democrats)",
    "valenceDucey:media_slantFox" = "Ducey × Fox",
    "valenceDucey:media_slantMSNBC" = "Ducey × MSNBC",
    "valenceDucey:partisanshipRepublican" = "Ducey × Republican",
    "media_slantFox:partisanshipRepublican" = "FOX × Republican",
    "media_slantMSNBC:partisanshipRepublican" = "MSNBC × Republican",
    "valenceDucey:media_slantFox:partisanshipRepublican" = "Ducey × Fox × Republican",
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
## Ducey × Fox                        -0.23 (0.04) ***   -0.23 (0.04) ***   -0.03 (0.05)       -0.06 (0.05)       -0.23 (0.04) ***   -0.22 (0.05) ***
## Ducey × MSNBC                      -0.01 (0.04)       -0.01 (0.04)       -0.07 (0.06)       -0.08 (0.05)       -0.01 (0.04)       -0.01 (0.04)    
## Ducey × Republican                                                                                              0.20 (0.05) ***    0.21 (0.05) ***
## FOX × Republican                                                                                               -0.31 (0.04) ***   -0.31 (0.04) ***
## MSNBC × Republican                                                                                              0.06 (0.04)        0.05 (0.04)    
## Ducey × Fox × Republican                                                                                        0.19 (0.07) **     0.17 (0.07) *  
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


