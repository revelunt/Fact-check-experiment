##==============================================================================
##
## PROJECT TITLE: Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## PROJECT AUTHOR: Je Hoon Chae (Yonsei University)
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code generates the Table 2
##
##==============================================================================

## Import Packages==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, conflicted, )
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("ungroup", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source("helper.R")
load("./data/mydata.RData")

# mydata %>% glimpse

## Recode and sub-samples=======================================================
mydata$source <- as.factor(mydata$source)
contrasts(mydata$source) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata$source)) <- c("Liberal Media")

mydata$favor <- as.factor(mydata$favor)
contrasts(mydata$favor) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata$favor)) <- c("Liberal-consistent")

mydata$ideology_discrete_1 <- as.factor(mydata$ideology_discrete_1)
contrasts(mydata$ideology_discrete_1) <- contr.treatment(3, base = 3)
colnames(contrasts(mydata$ideology_discrete_1)) <- c("Liberal", "Moderate")

mydata_liberal <- filter(mydata, ideology_discrete_1 == "Liberal")
mydata_moderate <- filter(mydata, ideology_discrete_1 == "Moderate")
mydata_conservative <- filter(mydata, ideology_discrete_1 == "Conservative")
mydata_partisan <- filter(mydata, ideology_discrete_1 != "Moderate")

mydata_partisan$favor <- as.factor(mydata_partisan$favor)
contrasts(mydata_partisan$favor) <- contr.treatment(2, base = 1)
colnames(contrasts(mydata_partisan$favor)) <- c("Liberal-consistent")

mydata_partisan$ideology_discrete_1 <- factor(mydata_partisan$ideology_discrete_1, levels = c("Liberal", "Conservative"))
contrasts(mydata_partisan$ideology_discrete_1) <- contr.treatment(2, base = 2)
colnames(contrasts(mydata_partisan$ideology_discrete_1)) <- c("Liberal")

# Bias Perception Linear Model (Relative Hostile Media Effect)==================
mod_hmp_1 <- lm_robust(bias ~ favor + source + ideology_discrete_1, data = mydata)
mod_hmp_2 <- lm_robust(bias ~ favor + source + ideology_discrete_1 + involvement + media_trust+ gender + age + education, data = mydata)
mod_hmp_3 <- lm_robust(bias ~ favor*source*ideology_discrete_1, data = mydata)
mod_hmp_4 <- lm_robust(bias ~ favor*source*ideology_discrete_1 + involvement + media_trust + gender + age + education, data = mydata)
mod_hmp_5 <- lm_robust(bias ~ favor + source + ideology_discrete_1, data = mydata_partisan)
mod_hmp_6 <- lm_robust(bias ~ favor + source + ideology_discrete_1 + involvement + media_trust  + gender + age + education, data = mydata_partisan)
mod_hmp_7 <- lm_robust(bias ~ favor*source*ideology_discrete_1, data = mydata_partisan)
mod_hmp_8 <- lm_robust(bias ~ favor*source*ideology_discrete_1 + involvement + media_trust + gender + age + education, data = mydata_partisan)

mod_hmp_list <- list(mod_hmp_1, mod_hmp_2, mod_hmp_3, mod_hmp_4, mod_hmp_5, mod_hmp_6, mod_hmp_7, mod_hmp_8)

texreg(
  mod_hmp_list,
  file = "./table/table_2.tex", label = "table:tab2",
  # override.pvalues = fix_ps(mod_hmp_1, mod_hmp_2, mod_hmp_3, mod_hmp_4, 
  #                           mod_hmp_5, mod_hmp_6, mod_hmp_7, mod_hmp_8),
  include.ci = F, booktabs = T,  use.packages = F, no.margin = T,
  # stars = c(0.001, 0.01, 0.05, 0.1),
  custom.model.names = str_c("(", seq_along(mod_hmp_list), ")"),
  caption = "Regression results of relative hostile media effect", 
  caption.above = T, threeparttable = T, bold = 0.05,
  custom.header = list("Whole Samples" = 1:4, "Only Partisans" = 5:8),
  custom.coef.map = list(
    "favorLiberal-consistent" = "LC", "sourceLiberal Media" = "LM",
    "ideology_discrete_1Liberal" = "Lib", "ideology_discrete_1Moderate" = "Mod",
    "favorLiberal-consistent:sourceLiberal Media" = "LC $\\times$ LM",
    "favorLiberal-consistent:ideology_discrete_1Liberal" = "LC $\\times$ Lib",
    "favorLiberal-consistent:ideology_discrete_1Moderate" = "LC $\\times$ Mod",
    "sourceLiberal Media:ideology_discrete_1Liberal" = "LM $\\times$ Lib",
    "sourceLiberal Media:ideology_discrete_1Moderate" = "LM $\\times$ Mod",
    "favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Liberal" = "LC $\\times$ LM $\\times$ Lib",
    "favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Moderate" = "LC $\\times$ LM $\\times$ Mod",
    "involvement" = "Involvement",  "media_trust" = "Media Trust",
    "age" = "Age", "education" = "Education",
    "genderFemale" = "Female", "(Intercept)" = "Constant"
  ),
  include.rmse = FALSE, include.rs = T,
  include.adj = T,
  custom.gof.names = c("R^2", "Adjusted R^2", "Observations"),
  reorder.gof = c(1, 2, 3),
  # custom.gof.rows = list("Covariates" = c("\\xmark", "\\xmark", "\\cmark", "\\xmark" ,"\\xmark", "\\cmark")),
  # custom.gof.rows = list("Covariates" = c("No", "Yes", "No", "Yes")),
  # omit.coef = c('age|education|gender|party|involvement|media_trust'),
  # caption.above = c("Dependent Variable: Degree of Agreement"),
  custom.note = "{\\vspace{.2cm} \\footnotesize \\textit{Note.} LC = Liberal-Consistent; LM = Liberal Media; Lib = Liberal; Mod = Moderate. 
  Robust standard errors (HC2) are in parentheses. \\\\ 
  \\vspace{.1cm} * \\textit{p} < .05; ** \\textit{p} < .01; *** \\textit{p} < .001.}"
  # custom.note = "\\parbox{1\\linewidth}{\\vspace{.2cm} \\small Note: * p < 0.05; ** p < 0.01; *** p < 0.001 \\\\ Robust standard errors (HC2) are in parentheses. \\\\ Covariates include age, education, gender, party identification, invovlement, media trust}"
)
