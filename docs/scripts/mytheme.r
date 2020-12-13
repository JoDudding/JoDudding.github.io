#-------------------------------------------------------------------------------
# theme defaults
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# libraries
#-------------------------------------------------------------------------------

library(tidyverse)
library(ggtext)

#-------------------------------------------------------------------------------
# colour scheme
#-------------------------------------------------------------------------------

# pal <- viridisLite::magma(8)[2:8]
# 
# demoplot(pal, "bar")
# 
# pal2 <- desaturate(pal)
# 
# demoplot(pal2, "bar")
# 
# swatchplot(
#   "-40%" = lighten(pal, 0.4),
#   "-20%" = lighten(pal, 0.2),
#   "  0%" = pal,
#   " 20%" =  darken(pal),
#   " 40%" =  darken(pal, 0.4),
#   off = c(0, 0)
# )
# 
# jo_charcoal <- "#4C4C53"
# jo_palecharcoal <- "#9b9ba8"

#-------------------------------------------------------------------------------
# font
#-------------------------------------------------------------------------------

library(showtext)
font_add_google("Fira Sans", "Fira Sans")
font_families()
showtext_auto()

#-------------------------------------------------------------------------------
# theme
#-------------------------------------------------------------------------------

theme_jo <- function(
  add_colour = "#5F187FFF",
  base_family = "Fira Sans",
  base_size = 15,
  x_grid_colour = "grey92",
  y_grid_colour = "grey92",
  ...
) {
  
  theme_bw(...) %+replace%
    theme(
      # default font
      text = element_text(family = base_family, size = base_size),
      # align title and caption to the plot not the panel
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      # change the title and caption to markdown and move them futher from the plot
      # change the title and caption to markdown and move them futher from the plot
      plot.title = element_markdown(
        hjust = 0, 
        margin = margin(c(0, 0, 10, 0)),
        colour = add_colour
      ),
      plot.subtitle = element_markdown(
        hjust = 0, 
        margin = margin(c(0, 0, 15, 0))
      ),
      plot.caption = element_markdown(
        hjust = 1, 
        margin = margin(c(10, 0, 0, 0))
      ),
      # move axis titles to the left/top and change them to markdown
      axis.title = element_markdown(hjust = 1),
      # allow the axis values to the markdown as well
      axis.text = element_markdown(),
      # remove the panel border
      panel.border = element_blank(),
      # put in the axis lines with a slightly thicker line than the gridlines
      axis.line = element_line(colour = "grey92", size = rel(1.5)),
      # make the tickmarks the same colour
      axis.ticks = element_line(colour = "grey92"),
      # facet strip text left aligned with extra space above
      strip.text = element_markdown(
        hjust = 0, margin = margin(c(10, 0, 0, 0)), colour = add_colour
      ),
      # clear colour and fill for strip
      strip.background = element_rect(colour = NA, fill = NA),
      # dotted gridlines
      panel.grid = element_line(linetype = 'dotted'),
      # ability to use a different colour for the gridlines
      panel.grid.major.x = element_line(colour = x_grid_colour),
      panel.grid.major.y = element_line(colour = y_grid_colour)
    )
}

#-------------------------------------------------------------------------------
# set theme
#-------------------------------------------------------------------------------

theme_set(theme_jo())

#-------------------------------------------------------------------------------
# scale adjustments
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
