## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 1 in the paper
## INPUT: 
##              ./01_data/FC_study-1.csv
## OUTPUT: 
##              Prints the results of Table 1
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  conflicted,    # Avoid conflict of functions with same names
  tidyverse      # Tidyverse umbrella package
)  

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


## IMPORT DATA =================================================================
df_study1 <- read_csv('./01_data/FC_study-1.csv')


## GENERATE TABLE===============================================================
tab1 <- df_study1 %>% 
  group_by(valence, media_slant) %>% 
  summarise(n1 = n())

tab2 <- df_study1 %>% 
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
##                      Liberal-consistent misinformation   Conservative-consistent misinformation 
## Conservative Media   137 (51/55/31)                      132 (41/58/33)                         
## Liberal Media        130 (43/49/38)                      132 (36/56/40)       


