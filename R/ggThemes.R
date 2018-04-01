
theme_climSurface <- function(font_size = 10, ...) {

  theme_set(theme_light(base_size = font_size))

  theme_climateSurface <- theme(
        text             = element_text(color = "#444444"),
        strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
        plot.margin      = unit(c(10,5,5,5),"mm"),
        plot.title       = element_text(size = font_size + 1, face = "bold"),
        plot.subtitle    = element_text(size = font_size),
        axis.title       = element_text(size = font_size),
        axis.text        = element_text(size = font_size),
        legend.title     = element_blank(),
        legend.text      = element_text(size = font_size),
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        aspect.ratio     = 1
  )
}

theme_timeSeries <- function(font_size = 10, ...) {

  theme_set(theme_light(base_size = font_size))

  theme_timeSeries <- theme(
  text             = element_text(color = "#444444"),
  strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
  plot.title       = element_text(size = font_size, face = "bold"),
  plot.subtitle    = element_text(size = font_size),
  axis.title       = element_text(size = font_size),
  axis.text        = element_text(size = font_size),
  legend.title     = element_blank(),
  legend.text      = element_text(size = font_size),
  ...)

}

theme_generic <- function(font_size = 10, ...) {

  theme_set(theme_light(base_size = font_size))

    theme_timeSeries <- theme(
    text             = element_text(color = "#444444"),
    strip.background = element_rect(colour = "#f0f0f0",fill = "#f0f0f0"),
    plot.title       = element_text(size = font_size, face = "bold"),
    plot.subtitle    = element_text(size = font_size),
    axis.title       = element_text(size = font_size),
    axis.text        = element_text(size = font_size),
    legend.title     = element_blank(),
    legend.text      = element_text(size = font_size),
    panel.grid.minor = element_blank(),
    ...)
}

# Use with stylish GCM projections ribbon plot...
theme_climTrends <- function(font_size = 10, ...) {

  theme_set(theme_light(base_size = font_size))

    theme_timeSeries <- theme(
    text             = element_text(color = "#444444"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    plot.margin      = unit(c(10,5,5,5),"mm"),
    plot.title       = element_text(size = font_size, face = "bold"),
    plot.subtitle    = element_text(size = font_size),
    axis.title       = element_text(size = font_size),
    axis.text        = element_text(size = font_size),
    legend.title     = element_blank(),
    legend.text      = element_text(size = font_size),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border     = element_rect(colour = "gray90", fill=NA, size=0.1),
    ...)
}


