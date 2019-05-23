
#' Generate a Climate Response Map
#'
#' @param data input dataset
#' @param x.specs x dimension specifications
#' @param y.specs y dimension specifications
#' @param z.specs z dimension specifications
#' @param z.bins  z bins
#' @param z.mid   critical threshold value
#' @param color.low first color
#' @param color.mid second color
#' @param color.high third color
#' @param resolution resolution for interpolation
#'
#' @return
#' @export
#'
#' @examples
climateResponseMap <- function(data = NULL,
  x.specs = list(name = NULL, label = NULL),
  y.specs = list(name = NULL, label = NULL),
  z.specs = list(name = NULL, label = NULL),
  z.bins = NULL, z.mid = NULL,
  color.low = "red", color.mid = "white", color.high = "blue",
  resolution = 300)

{

  #Libraries required
  require(ggplot2)
  require(dplyr)

  gridInterpolate <- function(x, y, z = NULL, resolution = resolution, ...) {

    # Interpolation for three-dimensional array
    if (is.null(z)) {z <- rep(0, length(x))}

    z <- data.frame(z)

    df1 <- lapply(seq_len(ncol(z)), function(i) akima::interp(x, y, z[, i],
                                                              xo = seq(min(x), max(x), length = resolution),
                                                              yo = seq(min(y), max(y), length = resolution)), ...)

    df2 <- do.call("cbind", lapply(df1, function(x) c(x$z)))
    df3 <- as_tibble(cbind(expand.grid(x = df1[[1]]$x, y = df1[[1]]$y), z = df2))

  }

  # Extract x, y, and z dimensions from the data matrix
  x_data <- data[[x.specs$name]]
  y_data <- data[[y.specs$name]]
  z_data <- data[[z.specs$name]]

  # Default bin number and mid.point
  if (is.null(z.bins)) z.bins <- pretty(c(min(z_data)*0.85, max(z_data)*1.15), 10)
  if (is.null(z.mid))  z.mid  <- mean(z_data)

  # Specify x, y breaks
  x_breaks <- unique(x_data)
  y_breaks <- unique(y_data)

  #z-dimension parameters
  zcut  <- (z.bins[-length(z.bins)] + z.bins[-1])/2
  zstep <- (z.bins[2] - z.bins[1])/2
  zlab  <-  z.bins[-c(1, length(z.bins))]

  # Prepare data matrix
  df <- gridInterpolate(x = x_data , y = y_data, z = z_data, resolution = resolution) %>%
    mutate(z = cut(z, breaks = zcut, dig.lab = 5, include.lowest = T, labels = zlab),
           z = as.numeric(as.character(z)))

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_tile(aes(fill = z), color = NA) +
    scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
    scale_y_continuous(expand = c(0, 0), breaks = y_breaks) +
    labs(x = x.specs$label, y = y.specs$label, fill = z.specs$label) +
    scale_fill_gradient2(low = color.low, mid = color.mid, high = color.high, midpoint = z.mid, limits = range(z.bins), breaks = z.bins) +
    guides(fill = guide_colorbar(nbin=length(z.bins), raster=F, barwidth=1, frame.colour="black", ticks = FALSE, barheight = 12))

  return(p)

}



