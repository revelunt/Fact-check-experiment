## IMPORT PACKAGES =============================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  showtext,       # Import font from Google Font
  tidyverse       # Tidyverse umbrella package
)  

## IMPORT FONT =================================================================
font_add_google('Fira Sans Condensed', 'fs')
showtext_auto()
PLOT_FONT_FAMILY <- 'fs'


## THEME SET ===================================================================
theme_plot <- function() {
  theme_grey(base_size = 15, base_family = PLOT_FONT_FAMILY) +
    theme(axis.text = element_text(colour = "black"),
          panel.grid.minor = element_blank(), 
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 15, face = "bold"),
          strip.background = element_rect(size = .5),
          plot.margin = margin(.1, .1, 0, .1, "cm"), 
          strip.text = element_text(face = "bold", margin = margin(.2, .2, .2, .2, "cm")),
          legend.position = "bottom",
          legend.title = element_text(size = 13), 
          legend.margin = margin(0, .2, .2, .2, "cm"))
}