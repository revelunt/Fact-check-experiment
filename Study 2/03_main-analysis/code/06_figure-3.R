## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-20
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 3 of Study 2 in the paper
## INPUT: 
##              ./Study 2/03_main-analysis/data/processed-data_study-2.csv
## OUTPUT: 
##              ./Study 2/03_main-analysis/results/figure-3.png
##              ./Study 2/03_main-analysis/results/figure-3.pdf
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
# Panel A of Figure 3
agree_tab <- mydata %>% 
  group_by(partisanship, valence, media_slant) %>% 
  summarise(mean = mean(agree_diff), sd = sd(agree_diff), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se))

plot_1 <- agree_tab %>% 
  ggplot(aes(partisanship, mean, colour = partisanship)) +
  geom_hline(yintercept = 0, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_colour_manual(values = c("cornflowerblue", "grey40", "firebrick")) +
  labs(x = NULL, y = 'Change score of agreement on the statement\n(persuasive <-> backfire)') +
  theme_bw(base_size = 17, base_family = PLOT_FONT_FAMILY) +
  theme(
    panel.grid.minor = element_blank(), 
    axis.title = element_text(size = 15, face = "bold"),
    strip.background = element_rect(size = .5),
    plot.margin = margin(.5,.5,.5,.5, "cm"),
    strip.text = element_text(face = "bold", margin = margin(.1,0,.1,0, "cm")),
    legend.position = "none"
    ) +
  facet_wrap(
    ~ factor(valence, labels = c("Biden was wrong!", "Ducey was wrong!")) +
      factor(media_slant, levels = c("FOX", "Reuter", "MSNBC")), nrow = 2
    ) +
  coord_flip()

# Panel B of Figure 3
belief_tab <- mydata %>% 
  group_by(partisanship, valence, media_slant) %>% 
  summarise(mean = mean(belief_diff), sd = sd(belief_diff), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se))

plot_2 <- belief_tab %>% 
  ggplot(aes(partisanship, mean, colour = partisanship)) +
  geom_hline(yintercept = 0, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_colour_manual(values = c("cornflowerblue", "grey40", "firebrick")) +
  labs(x = NULL, y = 'Change score of factual belief on the statement\n(persuasive <-> backfire)') +
  theme_bw(base_size = 17, base_family = PLOT_FONT_FAMILY) +
  theme(
    panel.grid.minor = element_blank(), 
    axis.title = element_text(size = 15, face = "bold"),
    strip.background = element_rect(size = .5),
    plot.margin = margin(.5,.5,.5,.5, "cm"),
    strip.text = element_text(face = "bold", margin = margin(.1,0,.1,0, "cm")),
    legend.position = "none"
  ) +
  facet_wrap(
    ~ factor(valence, labels = c("Biden was wrong!", "Ducey was wrong!")) +
      factor(media_slant, levels = c("FOX", "Reuter", "MSNBC")), nrow = 2
  ) +
  coord_flip()

# Merge two plots
merged_plot <- plot_grid(plot_1, plot_2, ncol = 1, labels = c("A", "B"), label_size = 15)


## OUTPUT ======================================================================
# .png extension file output
PLOT_FILE_PATH_PNG <- './Study 2/03_main-analysis/results/figure-3.png'
merged_plot %>% ggsave(filename = PLOT_FILE_PATH_PNG, width = 10, height = 10, device = 'png', dpi = 'retina')

# .pdf extension file output
PLOT_FILE_PATH_PDF <- './Study 2/03_main-analysis/results/figure-3.pdf'
merged_plot %>% ggsave(filename = PLOT_FILE_PATH_PDF, width = 10, height = 10, device = 'pdf', dpi = 'retina')

