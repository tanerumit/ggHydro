

#' @title Climate response surface in ggplot
#' @description FUNCTION_DESCRIPTION
#' @param data data frame containing climate response surfaca data
#' @param tavg.col name of the temperature changes column, Default: 'tavg'
#' @param prcp.col name of the precipitation changes column, Default: 'prcp'
#' @param metric.col name of the metric column, Default: NULL
#' @param tavg.breaks temperature change axis breaks, Default: NULL
#' @param prcp.breaks precipitation change axis breaks, Default: NULL
#' @param bin.pass.range placeholder
#' @param bin.fail.range placeholder
#' @param bin.threshold placeholder
#' @param metric.dir placeholder
#' @param display.center.value placeholder
#'
#' @return returns a ggplot2 object
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ggResponseSurface
#' @export


ggResponseSurface <- function(
  data = NULL,
  tavg.col = "tavg",
  prcp.col = "prcp",
  metric.col = NULL,
  tavg.breaks = NULL,
  prcp.breaks = NULL,
  bin.pass.range = NULL,
  bin.fail.range = NULL,
  bin.threshold = NULL,
  metric.dir = "increasing",
  display.center.value = TRUE,
  verbose = FALSE)

{

  require(ggplot2)
  require(dplyr)
  require(ggsci)

  #Climate response surface dimensions
  tavg <- data[[tavg.col]]
  prcp <- data[[prcp.col]]
  metric <- data[[metric.col]]

  # tavg axis scaling
  tavg_breaks   <- unique(tavg)
  tavg_step <- (tavg[2] - tavg[1])/2
  tavg_lim  <- range(tavg) + c(- tavg_step, tavg_step)

  # precip axis scaling
  prcp_breaks   <- unique(prcp)
  prcp_step <- (prcp[2] - prcp[1])/2
  prcp_lim  <- range(prcp) + c(- prcp_step, prcp_step)

  # performance metric scaling
  bin_pass_num <- length(bin.pass.range)-1
  bin_fail_num <- length(bin.fail.range)-1
  if(metric.dir == "increasing") {
    col_low <- if(!is.null(bin.fail.range)) rev(pal_material("red")(bin_fail_num)) else NULL
    col_up  <- if(!is.null(bin.pass.range)) pal_material("blue")(bin_pass_num) else NULL
    col_mid <- if(!is.null(bin.threshold)) "white" else NULL
  } else {
    col_low  <- if(!is.null(bin.pass.range)) rev(pal_material("blue")(bin_pass_num)) else NULL
    col_up   <- if(!is.null(bin.fail.range)) pal_material("red")(bin_fail_num) else NULL
  }

  col_mid <- if(!is.null(bin.threshold)) "white" else NULL
  var_col <- c(col_low, col_mid, col_up)
  bins <- c(bin.fail.range, bin.threshold, bin.pass.range) %>% unique()

  # Prepare final data frame for plotting
  df <- data %>% ungroup() %>%
      mutate(bins = cut(metric, breaks = bins, dig.lab = 5, include.lowest = T))

  #Display range or center value of the range
  bin_labs <- bins
  if (display.center.value == TRUE) bin_labs <- (bins[-length(bins)] + bins[-1])/2

  #Base-plot
  gg <- ggplot(df, aes(x = tavg, y = prcp)) +
    geom_tile(aes(fill = bins), color = "gray70") +
    scale_fill_manual(values=var_col, drop = FALSE, labels = bin_labs) +
    scale_x_continuous(expand = c(0,0), breaks = tavg_breaks) +
    scale_y_continuous(expand = c(0,0), breaks = prcp_breaks) +
    labs(x = expression("Temperature change (" * degree * C *")"),
         y = "Precipation change (%)", color = NULL, fill = NULL) +
    guides(fill  = guide_legend(order = 1),
           color = guide_legend(order = 2))

  if(verbose == TRUE) {
    out <- list(plot = gg, bins = bins, color = var_col)
  } else {
    out <- gg
  }

  return(out)

}



