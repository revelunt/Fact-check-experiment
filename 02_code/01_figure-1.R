## =============================================================================
##
## TITLE:       Attitudinal Persuasion and Perceptual Backfire? 
## DATE:        2022-01-14
## AUTHORS:     Je Hoon Chae, Sang Yup Lee, & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 1 in the paper
## INPUT: 
##              ./01_data/FC_study-1.csv
## OUTPUT: 
##              ./03_results/figure-1.png
##              ./03_results/figure-1.pdf
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggrepel,
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
df_study1 <- read_csv('./01_data/FC_study-1.csv')


## GENERATE PLOT ===============================================================
tab <- df_study1 %>% 
  group_by(ideology_discrete, valence, media_slant) %>% 
  summarise(mean = mean(agree_diff), sd = sd(agree_diff), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se),
         conf_80_low = (mean - 1.282*se), conf_80_high = (mean + 1.282*se))

effectsize <- df_study1 %>%
  group_by(ideology_discrete, valence, media_slant) %>%
  group_modify(~ tidy(cohensD(.$agree_t2, .$agree_t1))) %>% 
  mutate(d = as.character(sprintf("%.2f", round(x, 2)))) %>% 
  dplyr::select(-x)

tab <- tab |> 
  left_join(effectsize, by = c("ideology_discrete", "valence", "media_slant"))

plot <- tab %>% 
  mutate(ideology_discrete = factor(ideology_discrete, 
                                    levels = c("Liberal", "Moderate", "Conservative"))) %>% 
  ggplot(aes(media_slant, mean, 
             colour = ideology_discrete)) +
  geom_hline(yintercept = 0, size = 1.5, colour = 'grey', alpha = .5) +
  geom_pointrange(aes(ymin = conf_95_low, ymax = conf_95_high), fill = "white",
                  position = position_dodge(width = .6)) +
  geom_text(aes(x = media_slant, y = conf_95_low - 0.1,
                label = d, group = ideology_discrete, colour = ideology_discrete),
            position = position_dodge(width = .6), size = 3,
            family = "ps", fontface = "bold") +
  scale_colour_manual(values = c("#004EA1", "grey60", "#D22730")) +
  labs(x = NULL, y = 'Mean change of agreement\nPersuasive <-> Backfire', 
       colour = "Ideology:", shape = NULL, label = NULL, fill = "Ideology:") +
  theme_plot() +
  coord_flip() +
  facet_grid(. ~ valence)


## OUTPUT ======================================================================
# .png extension file output
FILE_PATH_PNG <- './03_results/figure-1.png'
ggsave(filename = FILE_PATH_PNG, width = 7, height = 3, device = 'png', dpi = 'retina')

# .pdf extension file output
FILE_PATH_PDF <- './03_results/figure-1.pdf'
ggsave(filename = FILE_PATH_PDF, width = 7, height = 3, device = 'pdf', dpi = 'retina')

