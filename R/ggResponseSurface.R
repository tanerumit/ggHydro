

#' Climate Response Surface
#'
#' @param data data frame object with x, y, and z columns
#' @param x.specs list of parameters specified for x dimension
#' @param y.specs list of parameters specified for y dimension
#' @param z.specs list of parameters specified for z dimension
#' @param display.center.value
#' @param plot.title
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples

ggResponseSurface <- function(
  data = NULL,
  x.specs = list(name = NULL, label = NULL),
  y.specs = list(name = NULL, label = NULL),
  z.specs = list(name = NULL, bins.up = NULL, bins.low = NULL, bins.mid = NULL, dir = "+"),
  display.center.value = TRUE,  plot.title = NULL,
  verbose = FALSE)
{

  #Libraries required
  require(ggplot2)
  require(dplyr)
  require(ggsci)

  # x, y, and z axis variables
  x_data <- data[[x.specs$name]]
  y_data <- data[[y.specs$name]]
  z_data <- data[[z.specs$name]]

  # Specify x, y breaks
  x_breaks <- unique(x_data)
  y_breaks <- unique(y_data)

  # Specify x,y intervals
  x_interval <- (x_breaks[2] - x_breaks[1])/2
  y_interval <- (y_breaks[2] - y_breaks[1])/2

  # Specify x,y limits
  x_lim <- range(x_data) + x_interval * c(-1,1)
  y_lim <- range(y_data) + y_interval * c(-1,1)

  # Number of bins in the lower and upper ranges
  z_bin_up_num  <- length(z.specs$bins.up)  - 1
  z_bin_low_num <- length(z.specs$bins.low) - 1

  # If the improvement direction is positive
  if (z.specs$dir == "+") {
    color_low <- "red"; color_up <- "blue"
  # If the improvement direction is negative
  } else {
    color_low <- "blue"; color_up <- "red"
  }

  z_bin_color_low <- if (!is.null(z.specs$bins.low)) rev(pal_material(color_low)(z_bin_low_num)) else NULL
  z_bin_color_up  <- if (!is.null(z.specs$bins.up)) pal_material(color_up)(z_bin_up_num) else NULL
  z_bin_color_mid <- if (!is.null(z.specs$bins.mid)) "white" else NULL
  z_bin_color     <- c(z_bin_color_low, z_bin_color_mid, z_bin_color_up)

  # Define bins
  z_bins <- c(z.specs$bins.low, z.specs$bins.mid, z.specs$bins.up) %>% unique()

  # Final data frame for plotting
  df <- data_frame(x = x_data, y = y_data, z = z_data) %>%
    mutate(z = cut(z, breaks = z_bins, dig.lab = 5, include.lowest = T))


  if (display.center.value == TRUE) {
    z_bin_labels <- (z_bins[-length(z_bins)] + z_bins[-1])/2
  } else {
    z_bin_labels <- unique(df$z_bins)
  }

  p <- ggplot(df, aes(x = x, y = y)) +
    # Z-axis tiling
    geom_tile(aes(fill = z), color = "gray70") +
    # set z-axis parameters
    scale_fill_manual(values = z_bin_color, drop = F, labels = z_bin_labels) +
    # set x-axis parameters
    scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
    # set y-axis parameters
    scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
    # labels
    labs(x = x.specs$label, y = y.specs$label, color = NULL, fill = z.specs$label) +
    # Legand
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

  if (!is.null(plot.title)) {
    p <- p + ggtitle(plot.title)
  }

  if (verbose == TRUE) {
    out <- list(plot = p, z_bins = z_bins, z_color = z_color)

  } else {

    out <- p
  }

  return(out)
}
