## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 4 in the paper
## INPUT: 
##              ./01_data/FC_study-2.csv
## OUTPUT: 
##              ./03_results/figure-4.png
##              ./03_results/figure-4.pdf
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  broom,
  lsr,           # CohenD() 
  conflicted,    # Avoid conflict of functions with same names
  tidyverse      # Tidyverse umbrella package
)  

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


## IMPORT CUSTOM THEME =========================================================
SUB_PATH <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste0(SUB_PATH, "/00_theme-set.R"))


## IMPORT DATA =================================================================
df_study2 <- read_csv('./01_data/FC_study-2.csv')


## GENERATE PLOTS ==============================================================
tab <- df_study2 %>% 
  filter(partisanship != "Independent") %>% 
  group_by(partisanship, valence, media_slant) %>% 
  summarise(mean = mean(hmp_score), sd = sd(hmp_score), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se),
         conf_80_low = (mean - 1.282*se), conf_80_high = (mean + 1.282*se))

tmp <- df_study2 %>%
  select(valence, media_slant, partisanship, hmp_score) %>%
  group_by(valence, media_slant, partisanship) %>%
  group_modify(~ tidy(cohensD(.$hmp_score, mu = 0.5))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  select(-x)

tab <- tab |> left_join(tmp, by = c("valence", "media_slant", "partisanship"))

plot <- tab %>% 
  ggplot(aes(media_slant, mean, colour = partisanship)) +
  geom_hline(yintercept = 0.5, size = 1.5, colour = 'grey', alpha = .5) +
  geom_pointrange(aes(ymin = conf_95_low, ymax = conf_95_high), fill = "white",
                  position = position_dodge(width = .6)) +
  geom_text(aes(x = media_slant, y = conf_95_high + 0.025,
                label = d, group = partisanship, colour = partisanship),
            position = position_dodge(width = .6), size = 3,
            family = "ps", fontface = "bold") +
  scale_colour_manual(values = c("#1405BD", "#DE0100")) +
  labs(x = NULL, y = 'Mean score of hostile media effect\n(0.5: Neutral)', colour = "Partisanship:", shape = "Partisanship:") +
  theme_plot() +
  coord_flip() +
  facet_grid(. ~ factor(valence, labels = c("Biden was wrong!", "Ducey was wrong!")))


## OUTPUT ======================================================================
# .png extension file output
FILE_PATH_PNG <- './03_results/figure-4.png'
ggsave(filename = FILE_PATH_PNG, width = 7, height = 3, device = 'png', dpi = 'retina')

# .pdf extension file output
FILE_PATH_PDF <- './03_results/figure-4.pdf'
ggsave(filename = FILE_PATH_PDF, width = 7, height = 3, device = 'pdf', dpi = 'retina')
  




