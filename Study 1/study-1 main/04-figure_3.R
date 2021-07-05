##==============================================================================
##
## PROJECT TITLE: Fact-Checking News by Partisan Media
## PROJECT AUTHOR: JE HOON CHAE (Yonsei University)
## E-MAIL: chaejehoon@yonsei.ac.kr
## DESCRIPTION: The following code replicates Figure 3
##
##==============================================================================

## Import packages==============================================================
pacman::p_load(tidyverse, broom, lsr, car, rstatix, lsmeans, ggsignif)
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("group_by", "dplyr")
conflicted::conflict_prefer("ungroup", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("recode", "dplyr")

load("./data/mydata.RData")

levels(mydata$favor) <- c("Fact-checking news debunking\nconservative-consistent statement", "Fact-checking news debunking\nliberal-consistent statement")
levels(mydata$source) <- c("Conservative media", "Liberal media")

# bonferroni_comp <- model_bias_intensity[[1]] %>%
#   lsmeans(., pairwise ~ partisanship_three_group, adjust = "Bonferroni")
# bonferroni_comp$lsmeans

# Generate Plot 1===============================================================
p1 <- mydata %>%
  select(favor, source, ideology_discrete_1, bias) %>%
  group_by(ideology_discrete_1, source, favor) %>%
  get_summary_stats(type = "mean_ci") %>%
  ggplot(aes(ideology_discrete_1, mean)) +
  geom_bar(aes(fill = ideology_discrete_1), 
           width = .9, colour = "black", size = .5, stat = "identity") +
  geom_errorbar(colour = "black", width = .1, size = .5, 
                position = position_dodge(width = .1),
                aes(ymin = mean - ci, ymax = mean + ci)) +
  geom_hline(yintercept = 0, colour = "black", alpha = .4) +
  scale_y_continuous(limits = c(-3.5, 3.5)) +
  scale_fill_discrete(type = c("cornflowerblue", "grey60", "firebrick")) +
  scale_shape_manual(values = c(17, 16)) +
  labs(x = NULL, 
       y = "Direction of Bias Perception (Scale: -5 to 5) \n(Favorable to Liberal <-> Favorable to Conservative)") +
  theme_bw(base_size = 20, base_rect_size = .7) +
  theme(
    # text = element_text(colour = "grey30"),
    strip.background = element_rect(fill = "grey90"), 
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid = element_line(linetype = "longdash", colour = "grey95"),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  coord_flip() +
  facet_grid(source ~ favor)

p1


# quartz(
#   type = "pdf", file = "./figure/figure_2.pdf",
#   width = 9.5, height = 7, family = "IBM Plex Sans Condensed"
# )
# p1
# dev.off()

# ggsave(filename = './figure/figure_2.png', width = 9.5, height = 7, device = 'png', dpi = 'retina')


# Generate Plot 2===============================================================

annotation_df <-
  data.frame(
    # favor = c("Conservative-consistent", "Liberal-consistent", "Liberal-consistent"),
    start = c(1, 2), end = c(3, 3), y = c(.8, .65), label = c("***", "**")
  )

p2 <- mydata %>%
  group_by(partisanship_three_group) %>%
  get_summary_stats(bias_intensity, type = "mean_ci") %>%
  ggplot(aes(partisanship_three_group, mean)) +
  geom_bar(stat = "identity", width = 0.9, colour = "black", size = .5,
           aes(fill = partisanship_three_group)) +
  geom_errorbar(width = 0, size = 3,
                aes(x = partisanship_three_group, 
                    ymin = mean - ci, ymax = mean + ci)) +
  geom_signif(data = annotation_df, 
              manual = T, tip_length = 0, size = 1, textsize = 6, 
              family = "IBM Plex Sans Condensed", fontface = "bold",
              aes(y_position = y, xmin = start, xmax = end, annotations = label)) +
  labs(x = "Ideological Strength", y = "Intensity of Bias Perception (Scale: 0 to 1)") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = c("#c8d6b9", "#8fc0a9", "#68b0ab")) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    # text = element_text(colour = "grey30"),
    panel.grid.major = element_line(linetype = "longdash"),
    # axis.title = element_text(size = 15),
    axis.line = element_line(size = .5),
    axis.ticks = element_line(size = .5),
    axis.ticks.length.y = unit(.3, "cm")
    # axis.title.y = element_text(margin = margin(t = 0, r = .5, b = 0, l = 0, unit = "cm")),
    # axis.title.x = element_text(margin = margin(t = .5, r = 0, b = 0, l = 0, unit = "cm")),
    # panel.grid = element_line(linetype = "longdash"),
    # plot.margin = margin(0, 0, 0, 0)
  )

p2

# quartz(
#   type = "pdf", file = "./figure/figure_3.pdf",
#   width = 6, height = 4.5, family = "IBM Plex Sans Condensed"
# )
# p2
# dev.off()

# ggsave(filename = './figure/figure_3.png', width = 6, height = 4.5, device = 'png', dpi = 'retina')


# Merge Plot 1 and Plot 2=======================================================

plot <- cowplot::plot_grid(p1, p2,
  ncol = 1, labels = "AUTO", align = "h", rel_heights = c(1.3, 1),
  label_fontfamily = "Arial"
)
plot

# Save the plot=================================================================
# quartz(
#   type = "pdf", file = "./figure/figure_3.pdf",
#   width = 7, height = 8, family = "Arial"
# )
# plot
# dev.off()
