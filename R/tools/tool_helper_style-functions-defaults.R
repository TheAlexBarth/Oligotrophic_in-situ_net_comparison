###
# style tools
###
library(ggplot2)

# a color blind pallete
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

# ggplot color selection
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# colorblind pallete selection
gg_cbb_col <- function(n) {
  if(n > 8) {
    cbbPalette <- c(cbbPalette,gg_color_hue(n-8))
  }
  return(cbbPalette[1:n])
}

# some themeatic defaults
ab_theme <- theme(axis.text = element_text(face = 'bold'),
                  axis.title = element_text(face = 'bold'))
