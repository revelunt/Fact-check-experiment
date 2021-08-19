## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 1
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              Prints a table with the data in Table 1 of the paper
##
## =============================================================================


## IMPORT PACKAGES==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse      # Tidyverse umbrella package
)  

## IMPORT DATA==================================================================
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## GENERATE TABLE===============================================================
tab1 <- mydata %>% 
  group_by(valence, media_slant) %>% 
  summarise(n1 = n())

tab2 <- mydata %>% 
  group_by(valence, media_slant, ideology_discrete) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = ideology_discrete) %>% 
  mutate(n2 = paste0("(", Conservative, "/", Moderate, "/", Liberal, ")")) %>% 
  select(-c(Liberal, Moderate, Conservative))

tab3 <- tab1 %>% 
  left_join(tab2) %>% 
  mutate(n = paste0(n1, " ", n2)) %>% 
  select(-c(n1, n2)) %>% 
  pivot_wider(names_from = valence, values_from = n) %>% 
  select(-media_slant)


## OUTPUT ======================================================================
output <- data.frame(tab3)
row_names <- c("Conservative Media", "Liberal Media")
col_names <- c("Conservative-consistent misinformation", 
               "Liberal-consistent misinformation")
rownames(output) <- row_names
colnames(output) <- col_names
output <- output[, c(2, 1)]

print(output)
##                    Liberal-consistent misinformation Conservative-consistent misinformation
## Conservative Media                    132 (33/58/41)                         137 (31/55/51)
## Liberal Media                         132 (40/56/36)                         130 (38/49/43)
