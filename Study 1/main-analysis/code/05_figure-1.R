## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 1
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              ./Study 1/main-analysis/results/figure-1.png
##              ./Study 1/main-analysis/results/figure-1.pdf
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  broom,         # tidy() function
  showtext,      # Call google font to use PT Sans
  tidyverse      # Tidyverse umbrella package
)  


## IMPORT FONT =================================================================
# Call PT Sans from Google Font
font_add_google('PT Sans', 'PT Sans')
showtext_auto()

PLOT_FONT_FAMILY <- 'PT Sans'


## IMPORT DATA =================================================================
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## GENERATE PLOT ===============================================================
# Change the order of the factor
mydata$valence <- relevel(mydata$valence, "Liberal-consistent")

# Summary stats of difference score of agreement
agree_tab <- mydata %>%  
  group_by(ideology_discrete, valence, media_slant) %>% 
  summarise(mean = mean(agree_t2 - agree_t1), sd = sd(agree_t2 - agree_t1), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se))

# Generate a plot replicating Figure 1 in the paper
agree_plot <- agree_tab %>% 
  ggplot(aes(ideology_discrete, mean, colour = ideology_discrete)) +
  geom_hline(yintercept = 0, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1.5, fatten = 3, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(-2, 2)) +
  scale_colour_manual(values = c("cornflowerblue", "grey40", "firebrick")) +
  labs(x = NULL, y = 'Change score of agreement on the statement\n(persuasive <---> backfire)') +
  theme_bw(base_size = 18, base_family = PLOT_FONT_FAMILY) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16, face = "bold"),
    strip.background = element_rect(size = .5),
    plot.margin = margin(.2, .2, .2, .2, "cm"),
    strip.text = element_text(face = "bold", margin = margin(.1,0,.1,0, "cm")),
    legend.position = "none"
    ) +
  facet_wrap(~ factor(valence, labels = c("Debunking liberal-consistent", "Debunking conservative-consistent")) +
               factor(media_slant, labels = c("Conservative media", "Liberal media")), nrow = 2) +
  coord_flip(); agree_plot


## OUTPUT ======================================================================
# .png extension file output
PLOT_FILE_PATH_PNG <- './Study 1/main-analysis/results/figure-1.png'
ggsave(filename = PLOT_FILE_PATH_PNG, width = 9, height = 5, device = 'png', dpi = 'retina')

# .pdf extension file output
PLOT_FILE_PATH_PDF <- './Study 1/main-analysis/results/figure-1.pdf'
ggsave(filename = PLOT_FILE_PATH_PDF, width = 9, height = 5, device = 'pdf', dpi = 'retina')
