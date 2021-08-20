## =============================================================================
##
## TITLE:       Perceiving Fact-Checks as Biased but Nevertheless Persuaded? 
## DATE:        2021-08-18
## AUTHORS:     Je Hoon Chae & Hyunjin (Jin) Song
## EMAIL:       chaejehoon@yonsei.ac.kr
## DESCRIPTION: This code replicates Figure 2
## INPUT: 
##              ./Study 1/main-analysis/data/processed-data_study-1.RData
## OUTPUT: 
##              ./Study 1/main-analysis/results/figure-2.png
##              ./Study 1/main-analysis/results/figure-2.pdf
##
## =============================================================================


## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  broom,         # tidy() function
  cowplot,       # Merge multiple plots as on figure
  showtext,      # Call Google Font to use PT Sans
  tidyverse      # Tidyverse umbrella package
)  


## IMPORT FONT =================================================================
# Call PT Sans from Google Font
font_add_google('PT Sans', 'PT Sans')
showtext_auto()

PLOT_FONT_FAMILY <- 'PT Sans'


## IMPORT DATA =================================================================
load('./Study 1/main-analysis/data/processed-data_study-1.RData')


## GENERATE PLOTS ============================================================== 
# Plot 1: Bias perception
mydata$valence <- relevel(mydata$valence, "Liberal-consistent")

bias_tab <- mydata %>% 
  group_by(ideology_discrete, valence, media_slant) %>% 
  summarise(mean = mean(bias), sd = sd(bias), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se))

plot_1 <- bias_tab %>%
  ggplot(aes(ideology_discrete, mean, colour = ideology_discrete)) +
  geom_hline(yintercept = 0, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1.5, fatten = 3, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(-.4, .4), breaks = c(-0.25, 0, 0.25)) +
  scale_colour_manual(values = c("cornflowerblue", "grey40", "firebrick")) +
  labs(x = NULL, y = 'Direction of bias perception (Scale: [-0.5, 0.5])\n(Favorable toward Liberals <---> Favorable toward Conservatives)') +
  theme_bw(base_size = 17, base_family = PLOT_FONT_FAMILY) +
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
  coord_flip()


# Plot 2: Hostile Media Perception
hmp_score_tab <- mydata %>% 
  filter(ideology_discrete != "Moderate") %>% 
  group_by(ideology_discrete, valence, media_slant) %>% 
  summarise(mean = mean(hmp_score), sd = sd(hmp_score), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(conf_95_low = (mean - 1.96*se), conf_95_high = (mean + 1.96*se))

plot_2 <- hmp_score_tab %>% 
  ggplot(aes(ideology_discrete, mean, colour = ideology_discrete)) +
  geom_hline(yintercept = 0.5, size = 1.5, colour = 'black', alpha = .3) +
  geom_pointrange(size = 1.5, fatten = 3, aes(ymin = conf_95_low, ymax = conf_95_high)) +
  scale_y_continuous(limits = c(0.15, 0.85), breaks = c(0.25, 0.5, 0.75)) +
  scale_colour_manual(values = c("cornflowerblue", "firebrick")) +
  labs(x = NULL, y = 'Hostile Media Perception (scale: [0, 1])\n(0: Assimilation --- 0.5: Neutral --- 1: Hostility)') +
  theme_bw(base_size = 17, base_family = PLOT_FONT_FAMILY) +
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
  coord_flip()

# Merge Plot 1 and Plot 2
merged_plot <- plot_grid(plot_1, plot_2, ncol = 1, labels = c("A", "B"), label_size = 15)


## OUTPUT ======================================================================
# .png extension file output
PLOT_FILE_PATH_PNG <- './Study 1/main-analysis/results/figure-2.png'
merged_plot %>% ggsave(filename = PLOT_FILE_PATH_PNG, width = 10, height = 10, device = 'png', dpi = 'retina')

# .pdf extension file output
PLOT_FILE_PATH_PDF <- './Study 1/main-analysis/results/figure-2.pdf'
merged_plot %>% ggsave(filename = PLOT_FILE_PATH_PDF, width = 10, height = 10, device = 'pdf', dpi = 'retina')

