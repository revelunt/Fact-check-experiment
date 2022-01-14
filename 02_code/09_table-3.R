## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 3 in the paper
## INPUT: 
##              ./01_data/FC_study-3.csv
## OUTPUT: 
##              Prints the results of Table 3
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
df_study3 <- read_csv('./01_data/FC_study-3.csv')


## GENERATE TABLE ==============================================================
tab1 <- df_study3 %>% 
  group_by(nfc, media_slant) %>% 
  summarise(n1 = n())

tab2 <- df_study3 %>% 
  group_by(nfc, media_slant, partisanship) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = partisanship) %>% 
  mutate(n2 = paste0("(", Democrat, "/", Republican, ")")) %>% 
  select(-c(Democrat, Republican))

tab3 <- tab1 %>% 
  left_join(tab2) %>% 
  mutate(n = paste0(n1, " ", n2)) %>% 
  select(-c(n1, n2)) %>% 
  pivot_wider(names_from = nfc, values_from = n) %>% 
  slice(c(1, 3, 2)) %>% 
  select(-media_slant)


## OUTPUT ======================================================================
output <- data.frame(tab3)
row_names <- c("FOX", "Reuter", "MSNBC")
col_names <- c("NfC: High", "NfC: Low")
rownames(output) <- row_names
colnames(output) <- col_names

print(output)
##          NfC: High    NfC: Low
## FOX    111 (59/52) 116 (64/52)
## Reuter 111 (60/51) 114 (62/52)
## MSNBC  111 (60/51) 115 (64/51)


  



