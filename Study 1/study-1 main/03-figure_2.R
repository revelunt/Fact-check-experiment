##==============================================================================
##
## PROJECT TITLE: Fact-Checking News by Partisan Media
## PROJECT AUTHOR: JE HOON CHAE (YONSEI UNIVERSITY) 
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: Following code replicates Figure 2
##
##==============================================================================

## Import Packages==============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, Cairo, lsr, rstatix, showtext)
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("group_by", "dplyr")
conflicted::conflict_prefer("ungroup", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

load("./study1/data/mydata.RData")

font_add("Helvetica", "Helvetica.ttc")
font_add("Arial", "Arial.ttf")
# font_add_google('Lato', 'Lato', regular.wt = 400)
# font_add_google('Roboto', 'Roboto', regular.wt = 400)
# font_add_google('Encode Sans', 'ES', regular.wt = 400)
# font_add_google("Fira Sans", 'FS', regular.wt = 400)
showtext_auto()

# t-test========================================================================
levels(mydata$favor) <- c("Fact-checking news debunking\nconservative-consistent statement", "Fact-checking news debunking\nliberal-consistent statement")
levels(mydata$source) <- c("Conservative\npartisan media", "Liberal\npartisan media")
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

# Generate Plot 1===============================================================
p1 <- mean_comparison %>%
  ggplot(aes(ideology_discrete_1, estimate)) +
  geom_hline(yintercept = 0, size = 1, color = "grey80", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 1, width = 0, color = 'grey60') +
  geom_point(aes(fill = p.value > .05), size = 4, shape = 21) +
  geom_text(aes(label = effect_size), family = 'Arial', nudge_x = .25, nudge_y = .35, size = 3.5) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_fill_manual(values = c("black", "white")) +
  labs(x = NULL, y = "The effect of fact-checking news\n(Persuasive \u2194 Backfire)") +
  theme_bw(base_size = 13, base_family = 'Arial', base_rect_size = .7) +  #
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"), 
    text = element_text(colour = "grey30"),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid = element_line(linetype = 'longdash', colour = 'grey95'),
    legend.position = 'none'
  ) +
  coord_flip() +
  facet_grid(source ~ favor)

# p1

# Generate Plot 2===============================================================
mydata %>%
  select(favor, source, ideology_discrete_1, agree_before, agree_after) %>%
  gather(key = "time", value = "effect", agree_before, agree_after) %>%
  mutate(time = fct_reorder(time, desc(effect)), 
         ideology = factor(.$ideology_discrete_1, 
                           levels = c('Conservative', 'Moderate', 'Liberal'),
                           ordered = T),
         time = recode(time, agree_before = "Before", agree_after = "After")) %>%
  group_by(ideology, source, favor, time) %>%
  get_summary_stats(type = "mean_ci")

p2 <- mydata %>%
  select(favor, source, ideology_discrete_1, agree_before, agree_after) %>%
  gather(key = "time", value = "effect", agree_before, agree_after) %>%
  mutate(time = fct_reorder(time, desc(effect)), 
         ideology = factor(.$ideology_discrete_1, 
                           levels = c('Conservative', 'Moderate', 'Liberal'),
                           ordered = T),
         time = recode(time, agree_before = "Before", agree_after = "After")) %>%
  group_by(ideology, source, favor, time) %>%
  get_summary_stats(type = "mean_ci") %>%
  ggplot(aes(ideology, mean)) +
  geom_point(aes(colour = time, shape = time), size = 3, position = position_dodge(width = .1)) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci, colour = time), 
                width = 0, position = position_dodge(width = .1)) +
  geom_line(aes(colour = time, group = time), 
            size = 1, alpha = .5, position = position_dodge(width = .2)) +
  scale_y_continuous(limits = c(1, 7)) +
  scale_colour_discrete(type = c('#aaaaaa', '#111111')) +
  scale_shape_manual(values = c(17, 16)) +
  labs(x = NULL, y = "Agreement on the statement (scale: 1-7)") +
  theme_bw(base_size = 13, base_family = 'Arial', base_rect_size = .7) +  #
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"), 
    text = element_text(colour = "grey30"),
    # strip.text = element_text(face = "bold"),
    # axis.title.y = element_text(margin = margin(t = 0, r = -3.0, b = 0, l = 0, unit = "cm"), size = 12),
    # plot.margin = margin(.2, .2, .2, 1.4, "cm"), 
    legend.title = element_blank(),
    panel.grid = element_line(linetype = 'longdash', colour = 'grey95'),
    legend.position = 'bottom'
  ) +
  facet_grid(source ~ favor)


p2

# Merge Plot 1 and Plot 2=======================================================
plot <- cowplot::plot_grid(
  p1, p2, ncol = 1, labels = "AUTO", align = "v", rel_heights = c(1, 1), 
  label_fontfamily = 'Arial'
)
plot

# Save the plot as .pdf=========================================================
quartz(type = 'pdf', file = './figure/figure_2.pdf', width = 7, height = 9, family = "ES")
plot
dev.off()



