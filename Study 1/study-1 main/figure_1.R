##==============================================================================
##
## PROJECT TITLE: Fact-Checking News by Partisan Media
## PROJECT AUTHOR: JE HOON CHAE (YONSEI UNIVERSITY) 
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: Following code replicates Figure 1
##
##==============================================================================

## Import Packages==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, lsr, rstatix)
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("group_by", "dplyr")
conflicted::conflict_prefer("ungroup", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

load("./data/mydata.RData")

# font_add("Helvetica", "Helvetica.ttc")
# font_add("Arial", "Arial.ttf")
# font_add_google('Lato', 'Lato', regular.wt = 400)
# font_add_google('PT Sans', 'PT Sans', regular.wt = 400)
# font_add_google('Raleway', 'Raleway', regular.wt = 400)
# font_add_google('Roboto', 'Roboto', regular.wt = 400)
# font_add_google('Encode Sans', 'ES', regular.wt = 400)
# font_add_google("Fira Sans", 'FS', regular.wt = 400)
# showtext_auto()

# t-test========================================================================
levels(mydata$favor) <- c("Fact-checking news debunking\nconservative-consistent statement", "Fact-checking news debunking\nliberal-consistent statement")
levels(mydata$source) <- c("Conservative media", "Liberal media")
mean_comparison <-
  mydata %>%
  dplyr::select(ideology_discrete_1, favor, source, agree_before, agree_after) %>%
  group_by(ideology_discrete_1, favor, source) %>%
  group_modify(~tidy(
    t.test(.$agree_after, .$agree_before, alt = "two.sided", paired = TRUE)
  ))

# Cohen's d=====================================================================
cohen_d <- mydata %>%
  dplyr::select(ideology_discrete_1, favor, source, agree_before, agree_after) %>%
  dplyr::group_by(ideology_discrete_1, favor, source) %>%
  group_modify( ~ tidy(cohensD(.$agree_after, .$agree_before, method = "paired")))

# Join two data frame===========================================================
mean_comparison <- mean_comparison %>%
  left_join(cohen_d) %>%
  rename(effect_size = x) %>%
  mutate(effect_size = round(effect_size, 2))

df1 <- mydata %>% 
  select(ideology_discrete_1, source, favor, agree_before, agree_after) %>% 
  group_by(ideology_discrete_1, source, favor) %>%
  get_summary_stats(type = "mean_ci") 
# pivot_wider(names_from = variable, values_from = c(n, mean, ci))

mean_comparison <- mean_comparison %>% 
  mutate(star = case_when(
    p.value < .05 & p.value > .01 ~ "*",
    p.value < .01 & p.value > .001 ~ "**",
    p.value < .001 ~ "***",
    TRUE ~ " ",
  )) 

mean_comparison <- mean_comparison %>% left_join(df1)

mean_comparison <- mean_comparison %>% 
  mutate(
    ideology = factor(ideology_discrete_1, levels = c('Conservative', 'Moderate', 'Liberal')), 
    time = recode(variable, agree_before = "Before", agree_after = "After"),
    time = factor(time, levels = c('Before', 'After')),
    effect_size = as.character(effect_size)
  ) %>% 
  unite(label, effect_size, star, sep = "")

p <- mean_comparison %>% 
  ggplot(aes(ideology, mean)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci, colour = time), 
                size = 2, width = 0, position = position_dodge(width = .2)) +
  geom_point(position = position_dodge(width = .2), 
             fill = 'white', size = 3.5, color = 'black',
             aes(colour = time, shape = time)) +
  # geom_line(aes(colour = time, group = time), 
  #           size = 1, alpha = .5, position = position_dodge(width = .2)) +
  geom_text(aes(label = ifelse(time == 'Before', label, "")),
            size = 5, family = 'IBM Plex Sans Condensed',
            nudge_x = .05, nudge_y = 1) +
  scale_y_continuous(limits = c(1, 7)) +
  scale_colour_discrete(type = c('#aaaaaa', '#111111')) +
  scale_shape_manual(values = c(24, 21)) +
  # scale_shape_manual(values = c(17, 16)) +
  labs(x = NULL, y = "Agreement on the Statement (scale: 1 to 7)") +
  theme_bw(base_size = 20, base_rect_size = .7) +  #
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    # strip.text = element_text(face = "bold"),
    # text = element_text(colour = "grey30"),
    # text = element_text(face = "bold"),
    axis.title = element_text(size = 17),
    # strip.text = element_text(face = "bold"),
    # axis.title.y = element_text(margin = margin(t = 0, r = -3.0, b = 0, l = 0, unit = "cm"), size = 12),
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_blank(),
    panel.grid = element_line(linetype = 'longdash', colour = 'grey95'),
    legend.position = 'bottom'
  ) +
  facet_grid(source ~ favor)

p

# ggsave(filename = './figure/figure_1.png', width = 9, height = 7, device = 'png', dpi = 'retina')

# Save the plot as .pdf=========================================================
# quartz(type = 'pdf', 
#        file = './figure/figure_1.pdf', width = 9, height = 7, family = "IBM Plex Sans Condensed")
# p
# dev.off()
