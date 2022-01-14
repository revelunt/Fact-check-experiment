## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Table 2 in the paper
## INPUT: 
##              ./01_data/FC_study-2.csv
## OUTPUT: 
##              Prints the results of Table 2
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
df_study2 <- read_csv('./01_data/FC_study-2.csv')


## GENERATE TABLE ==============================================================
tab1 <- df_study2 %>% 
  group_by(valence, media_slant) %>% 
  summarise(n1 = n())

tab2 <- df_study2 %>% 
  group_by(valence, media_slant, partisanship) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = partisanship) %>% 
  mutate(n2 = paste0("(", Democrat, "/", Independent, "/", Republican, ")")) %>% 
  select(-c(Democrat, Independent, Republican))

tab3 <- tab1 %>% 
  left_join(tab2) %>% 
  mutate(n = paste0(n1, " ", n2)) %>% 
  select(-c(n1, n2)) %>% 
  pivot_wider(names_from = valence, values_from = n) %>% 
  select(-media_slant)


## OUTPUT ======================================================================
output <- data.frame(tab3)
row_names <- c("FOX", "Reuter", "MSNBC")
col_names <- c("Debunking Joe Biden", "Debunking Doug Ducey")
rownames(output) <- row_names
colnames(output) <- col_names

print(output)
##        Debunking Joe Biden Debunking Doug Ducey
## FOX         154 (61/40/53)       151 (58/42/51)
## Reuter      152 (58/42/52)       148 (57/40/51)
## MSNBC       148 (58/41/49)       151 (59/41/51)