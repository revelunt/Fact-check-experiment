## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 4 of Study 2 in the paper
## INPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
## OUTPUT: 
##              ./Study 2/03_main-analysis/results/figure-4.png
##              ./Study 2/03_main-analysis/results/figure-4.pdf
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  cowplot,        # Merge multiple plots
  showtext,       # Import font from Google Font
  tidyverse       # Tidyverse umbrella package
)  


## IMPORT FONT =================================================================
font_add_google(name = "Fira Sans", family = "Fira Sans")
showtext_auto()
PLOT_FONT_FAMILY <- "Fira Sans"


## IMPORT DATA =================================================================
INPUT_DATA_PATH <- "./Study 2/03_main-analysis/data/processed-data_study-2.csv"
mydata <- read_csv(INPUT_DATA_PATH)


## GENERATE PLOTS ==============================================================
# Figure 4: Hostile Media Perception
hmp_tab <- mydata %>% 
  filter(partisanship != "Independent") %>% 
  group_by(partisanship, valence, media_slant) %>% 
  summarise(mean = mean(hmp_score), sd = sd(hmp_score), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se),
         conf_95_high = (mean + 1.96*se),
         conf_66_low = (mean - 1*se),
         conf_66_high = (mean + 1*se))

hmp_plot <- hmp_tab %>% 
  ggplot(aes(partisanship, mean, colour = partisanship)) +
  geom_hline(yintercept = 0.5, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1.5, fatten = 3, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(0.15, 0.85), breaks = c(0.25, 0.5, 0.75)) +
  scale_colour_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = NULL, y = 'Hostile media perception (scale: [0, 1])\n(0: assimilation --- 0.5: neutral --- 1: hostility)') +
  theme_bw(base_size = 17, base_family = PLOT_FONT_FAMILY) +
  theme(
    panel.grid.minor = element_blank(), 
    axis.title = element_text(size = 15, face = "bold"),
    strip.background = element_rect(size = .5),
    plot.margin = margin(.3,.3,.3,.3, "cm"),
    strip.text = element_text(face = "bold", margin = margin(.1,0,.1,0, "cm")),
    legend.position = "none"
    ) +
  facet_wrap(
    ~ factor(valence, labels = c("Biden was wrong!", "Ducey was wrong!")) +
    factor(media_slant, levels = c("FOX", "Reuter", "MSNBC")), nrow = 2
    ) +
  coord_flip()


## OUTPUT ======================================================================
# .png extension file output
PLOT_FILE_PATH_PNG <- './Study 2/03_main-analysis/results/figure-4.png'
hmp_plot %>% ggsave(filename = PLOT_FILE_PATH_PNG, width = 10, height = 5, device = 'png', dpi = 'retina')

# .pdf extension file output
PLOT_FILE_PATH_PDF <- './Study 2/03_main-analysis/results/figure-4.pdf'
hmp_plot %>% ggsave(filename = PLOT_FILE_PATH_PDF, width = 10, height = 5, device = 'pdf', dpi = 'retina')

