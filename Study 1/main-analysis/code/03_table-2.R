## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 2
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a table with the data in Table 2 of the paper
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
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## RECODE & SUBSAMPLES==========================================================
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


## FIT REGRESSION MODELS========================================================
mod1 <- lm_robust(agree_diff ~ valence*media_slant, data = mydata_liberal)
mod2 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, mydata_liberal)
mod3 <- lm_robust(agree_diff ~ valence*media_slant, data = mydata_moderate)
mod4 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, mydata_moderate)
mod5 <- lm_robust(agree_diff ~ valence*media_slant, data = mydata_conservative)
mod6 <- lm_robust(agree_diff ~ valence*media_slant + age + education + gender + involvement + media_trust, mydata_conservative)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)


## OUTPUT=======================================================================
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

