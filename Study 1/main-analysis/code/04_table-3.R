## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 3 in the paper
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a regression table identical to Table 3 of the paper
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Tidyverse umbrella package
  data.table,    # Data.table type operation
  estimatr,      # HC2 robust standard error linear regression
  texreg         # Print regression table
)  


## IMPORT DATA =================================================================
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## RECODE & SUBSAMPLES =========================================================
mydata$media_slant <- as.factor(mydata$media_slant)
contrasts(mydata$media_slant) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata$media_slant)) <- c("Liberal Media")

mydata$valence <- as.factor(mydata$valence)
contrasts(mydata$valence) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata$valence)) <- c("Liberal-consistent")

mydata$ideology_discrete <- as.factor(mydata$ideology_discrete)
contrasts(mydata$ideology_discrete) <- contr.treatment(3, base = 3)
colnames(contrasts(mydata$ideology_discrete)) <- c("Liberal", "Moderate")

mydata_liberal <- filter(mydata, ideology_discrete == "Liberal")
mydata_moderate <- filter(mydata, ideology_discrete == "Moderate")
mydata_conservative <- filter(mydata, ideology_discrete == "Conservative")
mydata_partisan <- filter(mydata, ideology_discrete != "Moderate")

mydata_partisan$valence <- as.factor(mydata_partisan$valence)
contrasts(mydata_partisan$valence) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata_partisan$valence)) <- c("Liberal-consistent")

mydata_partisan$ideology_discrete <- factor(mydata_partisan$ideology_discrete, levels = c("Liberal", "Conservative"))
contrasts(mydata_partisan$ideology_discrete) <- contr.treatment(2, base = 2)
colnames(contrasts(mydata_partisan$ideology_discrete)) <- c("Liberal")


## FIT REGRESSION MODELS =======================================================
mod1 <- lm_robust(hmp_score ~ valence*media_slant, data = mydata_liberal)
mod2 <- lm_robust(hmp_score ~ valence*media_slant + involvement + media_trust+ gender + age + education, data = mydata_liberal)
mod3 <- lm_robust(hmp_score ~ valence*media_slant, data = mydata_conservative)
mod4 <- lm_robust(hmp_score ~ valence*media_slant + involvement + media_trust+ gender + age + education, data = mydata_conservative)
mod5 <- lm_robust(hmp_score ~ valence*media_slant*ideology_discrete, data = mydata_partisan)
mod6 <- lm_robust(hmp_score ~ valence*media_slant*ideology_discrete + involvement + media_trust+ gender + age + education, data = mydata_partisan)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


## OUTPUT ======================================================================
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

