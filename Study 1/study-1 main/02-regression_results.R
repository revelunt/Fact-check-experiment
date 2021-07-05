##==============================================================================
##
## PROJECT TITLE: Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## PROJECT AUTHOR: JE HOON CHAE (YONSEI UNIVERSITY) 
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: The code replicates regression table
##
##==============================================================================

## Import Packages==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, conflicted, rstatix, estimatr,
               lm.beta, effectsize, lsr, texreg,
               xtable, QuantPsyc, kableExtra)
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

# Table t-test==================================================================
tab1 <- mydata %>% 
  group_by(group, ideology_discrete_1) %>% 
  summarise(n = n(), mean_t1 = mean(agree_before), sd_t1 = sd(agree_before),
            mean_t2 = mean(agree_after), sd_t2 = sd(agree_after))

tab2 <- mydata %>%
  dplyr::select(ideology_discrete_1, group, agree_before, agree_after) %>%
  group_by(ideology_discrete_1, group) %>%
  group_modify(~tidy(t.test(.$agree_after, .$agree_before, alt = "two.sided", paired = TRUE))) %>% 
  mutate(statistic = round(statistic, 2), p.value = as.character(round(p.value, 3))) %>% 
  mutate(p.value = if_else((p.value == "0")|(p.value == "0.001"), "< .001", substr(p.value, 2, 5))) %>% 
  dplyr::select(-estimate, -parameter, -conf.low, -conf.high, -method, -alternative)

tab3 <- mydata %>%
  dplyr::select(agree_before, agree_after, group, ideology_discrete_1) %>%
  group_by(group, ideology_discrete_1) %>%
  group_modify(~ tidy(cohensD(.$agree_after, .$agree_before))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

summary_df <- tab1 %>% left_join(tab2) %>% left_join(tab3) %>% 
  within({group[!((n == 31)|(n == 38)|(n == 33)|(n == 40))] <- NA}) %>% 
  rename(Group = group,
         "Ideology" = ideology_discrete_1, 
         "M(T1)" = mean_t1, "SD(T1)" = sd_t1, 
         "M(T2)" = mean_t2, "SD(T2)" = sd_t2,
         t = statistic, p = p.value)

kbl(summary_df[2:ncol(summary_df)], 
    format = "latex", booktabs = TRUE, digits = 2, row.names = FALSE, escape = FALSE,
    align = c(rep("l", 1), rep("c", 9)),
    caption = "Mean comparison in each sub-group",
    label = "table_b1", 
    col.names = c(" ", "\\textit{n}", "\\textit{M}", "\\textit{SD}", "\\textit{M}", 
                  "\\textit{SD}", "\\textit{t}", "\\textit{p}", "Cohen's \\textit{d}")) %>% 
    add_header_above(c(" " = 2, "Before" = 2, "After" = 2)) %>% 
    pack_rows("Liberal-consistent $\\times$ Conservative Media", 1, 3, escape = F, bold = F) %>% 
    pack_rows("Liberal-consistent $\\times$ Liberal Media", 4, 6, escape = F, bold = F) %>% 
    pack_rows("Conservative-consistent $\\times$ Conservative Media", 7, 9, escape = F, bold = F) %>% 
    pack_rows("Conservative-consistent $\\times$ Liberal Media", 10, 12, escape = F, bold = F) %>% 
    save_kable(file = "./table/table_b1.tex")
  

require(data.table)
setDT(mydata)
## (-) = persuasive effect, (+) = backfire effect
mydata[, diff := agree_after - agree_before] 
mydata[, table(agree_difference, diff)]

mydata[, pro_counter := 0]
mydata[favor == 'Fact-checking news debunking\nconservative-consistent statement' &
         ideology_discrete_1 == 'Conservative', pro_counter := 1] ## counter 
mydata[favor == 'Fact-checking news debunking\nconservative-consistent statement' &
         ideology_discrete_1 == 'Liberal', pro_counter := 0] ## pro 
mydata[favor == 'Fact-checking news debunking\nliberal-consistent statement' &
         ideology_discrete_1 == 'Conservative', pro_counter := 0] ## pro 
mydata[favor == 'Fact-checking news debunking\nliberal-consistent statement' &
         ideology_discrete_1 == 'Liberal', pro_counter := 1] ## counter 

mydata[ideology_discrete_1 != 'Moderate', 
       lm_robust(agree_difference ~ pro_counter)] %>% summary



# Effect of Fact-Checking=======================================================
# mod1 <- lm_robust(agree_difference ~ favor*source*ideology_discrete_1, data = mydata)
# mod2 <- lm_robust(agree_difference ~ favor*source*ideology_discrete_1 + age + education
#                   + gender + party + involvement + media_trust, mydata)

mod1 <- lm_robust(agree_difference ~ favor*source, data = mydata_liberal)
mod2 <- lm_robust(agree_difference ~ favor*source + age + education + gender + involvement + media_trust, mydata_liberal)
mod3 <- lm_robust(agree_difference ~ favor*source, data = mydata_moderate)
mod4 <- lm_robust(agree_difference ~ favor*source + age + education + gender + involvement + media_trust, mydata_moderate)
mod5 <- lm_robust(agree_difference ~ favor*source, data = mydata_conservative)
mod6 <- lm_robust(agree_difference ~ favor*source + age + education + gender + involvement + media_trust, mydata_conservative)

mod_list <- list(mod1, mod2, mod3, mod4, mod5, mod6)

texreg(
  mod_list,
  file = "./table/table_1.tex", label = "table:tab1",
  override.pvalues = fix_ps(mod1, mod2, mod3, mod4, mod5, mod6),
  include.ci = F, booktabs = T, use.packages = F,
  custom.model.names = str_c("(", seq_along(mod_list), ")"),
  caption = "Regression results of degree of agreement", 
  caption.above = T, threeparttable = T, bold = 0.05,
  custom.header = list("Liberals" = 1:2, "Moderates" = 3:4, "Conservatives" = 5:6),
  custom.coef.map = list(
    "favorLiberal-consistent" = "LC", "sourceLiberal Media" = "LM",
    "favorLiberal-consistent:sourceLiberal Media" = "LC $\\times$ LM",
    # "involvement" = "Involvement",  "media_trust" = "Media Trust",
    # "age" = "Age", "education" = "Education",
    # "genderFemale" = "Female", 
    "(Intercept)" = "Constant"
  ),
  include.rmse = FALSE, include.rs = T,
  include.adj = FALSE,
  custom.gof.names = c("R^2", "Observations"),
  reorder.gof = c(1, 2,3),
  custom.gof.rows = list("Covariates" = c("\\xmark", "\\cmark", "\\xmark", "\\cmark", "\\xmark", "\\cmark")),
  # custom.gof.rows = list("Covariates" = c("No", "Yes")),
  omit.coef = c('age|education|gender|party|involvement|media_trust'),
  # caption.above = c("Dependent Variable: Degree of Agreement"),
  custom.note = "{\\vspace{.2cm} \\small \\textit{Note.} LC = Liberal-Consistent; LM = Liberal Media. 
  Robust standard errors (HC2) are in parentheses. \\\\ 
  \\vspace{.15cm} * \\textit{p} < .05, ** \\textit{p} 
  < .01, *** \\textit{p} < .001.}"
)

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
  override.pvalues = fix_ps(mod_hmp_1, mod_hmp_2, mod_hmp_3, mod_hmp_4, 
                            mod_hmp_5, mod_hmp_6, mod_hmp_7, mod_hmp_8),
  include.ci = F, booktabs = T,  use.packages = F, no.margin = T,
  # stars = c(0.001, 0.01, 0.05, 0.1),
  custom.model.names = str_c("(", seq_along(mod_hmp_list), ")"),
  caption = "Regression results of relative hostile media effect", 
  caption.above = T, threeparttable = T, bold = 0.05,
  custom.header = list("Whole Samples" = 1:4, "Only Partisans" = 5:8),
  custom.coef.map = list(
    "favorLiberal-consistent" = "LC", "sourceLiberal Media" = "LM",
    "ideology_discrete_1Liberal" = "Lib", "ideology_discrete_1Moderate" = "Mod",
    # "favorLiberal-consistent:sourceLiberal Media" = "LC $\\times$ LM",
    # "favorLiberal-consistent:ideology_discrete_1Liberal" = "LC $\\times$ Lib",
    # "favorLiberal-consistent:ideology_discrete_1Moderate" = "LC $\\times$ Mod",
    # "sourceLiberal Media:ideology_discrete_1Liberal" = "LM $\\times$ Lib",
    # "sourceLiberal Media:ideology_discrete_1Moderate" = "LM $\\times$ Mod",
    # "favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Liberal" = "LC $\\times$ LM $\\times$ Lib",
    # "favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Moderate" = "LC $\\times$ LM $\\times$ Mod",
    # "involvement" = "Involvement",  "media_trust" = "Media Trust",
    # "age" = "Age", "education" = "Education",
    # "genderFemale" = "Female", 
    "(Intercept)" = "Constant"
    ),
  include.rmse = FALSE, include.rs = T,
  include.adj = F,
  custom.gof.names = c("R^2", "Observations"),
  reorder.gof = c(1, 2, 3, 4),
  custom.gof.rows = list(
    "Interaction" = c("\\xmark", "\\xmark", "\\cmark", "\\cmark", "\\xmark" , "\\xmark", "\\cmark", "\\cmark"),
    "Covariates" = c("\\xmark", "\\cmark", "\\xmark", "\\cmark", "\\xmark" , "\\cmark", "\\xmark", "\\cmark")
    ),
  # custom.gof.rows = list("Covariates" = c("No", "Yes", "No", "Yes")),
  omit.coef = c(
  'age|education|gender|party|involvement|media_trust|
  favorLiberal-consistent:sourceLiberal Media|
  favorLiberal-consistent:ideology_discrete_1Liberal|
  favorLiberal-consistent:ideology_discrete_1Moderate|
  sourceLiberal Media:ideology_discrete_1Liberal|
  sourceLiberal Media:ideology_discrete_1Moderate|
  favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Liberal|
  favorLiberal-consistent:sourceLiberal Media:ideology_discrete_1Moderate'
  ),
  # caption.above = c("Dependent Variable: Degree of Agreement"),
  custom.note = "{\\vspace{.2cm} \\footnotesize \\textit{Note.} LC = Liberal-Consistent; LM = Liberal Media; Lib = Liberal; Mod = Moderate. 
  Robust standard errors (HC2) are in parentheses. (* \\textit{p} < .05; ** \\textit{p} < .01; *** \\textit{p} < .001.)}"
  # custom.note = "\\parbox{1\\linewidth}{\\vspace{.2cm} \\small Note: * p < 0.05; ** p < 0.01; *** p < 0.001 \\\\ Robust standard errors (HC2) are in parentheses. \\\\ Covariates include age, education, gender, party identification, invovlement, media trust}"
)

# Intensity of Bias Perception==================================================
mod_intensity_1 <- lm_robust(bias_intensity ~ partisanship_three_group, data = mydata)
mod_intensity_2 <- lm_robust(bias_intensity ~ partisanship_three_group + favor*source, data = mydata)
mod_intensity_3 <- lm_robust(bias_intensity ~ partisanship_three_group + favor*source + age + education + gender + involvement + media_trust, data = mydata)

mod_intensity_list <- list(mod_intensity_1, mod_intensity_2, mod_intensity_3)

texreg(
  mod_intensity_list,
  file = "./table/table_3.tex", label = "table:tab3",
  override.pvalues = fix_ps(mod_intensity_1, mod_intensity_2, mod_intensity_3),
  include.ci = FALSE, booktabs = TRUE, use.packages = FALSE, bold = 0.05, 
  custom.model.names = str_c("   (", seq_along(mod_intensity_list), ")   "),
  caption = "Regression results of intensity of bias perception", 
  caption.above = TRUE, threeparttable = TRUE, 
  custom.header = list("Intensity of bias perception" = 1:3),
  custom.coef.map = list(
    "partisanship_three_groupWeak" = "Weak", 
    "partisanship_three_groupStrong" = "Strong",
    "favorLiberal-consistent" = "LC", "sourceLiberal Media" = "LM",
    "favorLiberal-consistent:sourceLiberal Media" = "LC $\\times$ LM",
    # "involvement" = "Involvement",  "media_trust" = "Media Trust",
    # "age" = "Age", "education" = "Education",
    # "genderFemale" = "Female", 
    "(Intercept)" = "Constant"
    ),
  include.rmse = FALSE, include.rs = T,
  include.adj = F,
  custom.gof.names = c("R^2", "Observations"),
  reorder.gof = c(1, 2, 3),
  custom.gof.rows = list("Covariates" = c("\\xmark","\\xmark", "\\cmark")),
  # custom.gof.rows = list("Covariates" = c("No", "Yes")),
  omit.coef = c('age|education|gender|party|involvement|media_trust'),
  # caption.above = c("Dependent Variable: Degree of Agreement"),
  custom.note = "{\\vspace{.2cm} \\small \\textit{Note.} LC = Liberal-Consistent; LM = Liberal Media. 
  Robust standard errors (HC2) are in parentheses. (* \\textit{p} < .05; ** \\textit{p} < .01; *** \\textit{p} < .001.)}"
  # custom.note = "\\parbox{0.6\\linewidth}{\\vspace{.2cm} \\small Note: * p < 0.05; ** p < 0.01; *** p < 0.001 \\\\ Robust standard errors (HC2) are in parentheses. \\\\ Covariates include age, education, gender, party identification, invovlement, media trust}"
)

  