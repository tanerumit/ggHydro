

#' @title Climate response surface in ggplot
#' @description FUNCTION_DESCRIPTION
#' @param data data frame containing climate response surfaca data
#' @param tavg.col name of the temperature changes column, Default: 'tavg'
#' @param prcp.col name of the precipitation changes column, Default: 'prcp'
#' @param metric.col name of the metric column, Default: NULL
#' @param tavg.breaks temperature change axis breaks, Default: NULL
#' @param prcp.breaks precipitation change axis breaks, Default: NULL
#' @param metric.min minimum value of the metric, Default: NULL
#' @param metric.threshold critical metric threshold, Default: NULL
#' @param metric.max maximum value of the metric, Default: NULL
#' @param max.bins maximum number of bins for plotting, Default: 5
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

ggResponseSurface <- function(data, tavg.col = "tavg", prcp.col = "prcp", 
  metric.col = NULL, tavg.breaks = NULL, prcp.breaks = NULL, 
  metric.threshold = NULL, metric.bin.num = 5,
  metric.improvement = "increasing") 
  
  {
  
  require(ggplot2)
  require(dplyr)

  require(ggsci)
  
  tavg <- data[[tavg.col]]
  prcp <- data[[prcp.col]]
  metric <- data[[metric.col]]
  
  ## SCALE PARAMETERS
  tavg_breaks   <- unique(tavg)
  prcp_breaks   <- unique(prcp)
  
  tavg_step <- (tavg[2] - tavg[1])/2 
  tavg_lim  <- range(tavg) + c(- tavg_step, tavg_step)
  
  prcp_step <- (prcp[2] - prcp[1])/2 
  prcp_lim  <- range(prcp) + c(- prcp_step, prcp_step)

  bins    <- pretty(metric, n = metric.bin.num, min.n = metric.bin.num - 1)
  bin_mid_index <- findInterval(x = metric.threshold, vec = bins)
  bin_mid <- bins[bin_mid_index]

  bin_low <- bins[1:bin_mid_index-1]
  bin_up  <- bins[(bin_mid_index+1):length(bins)]
    
  if(metric.improvement == "increasing") {

    col_low  <- rev(pal_material("red")(length(bin_low)))
    col_up   <- pal_material("blue")(length(bin_up))
    col_mid <- "white"
    var_col <- c(col_low, col_mid, col_up) 
  
  } else {
    
    col_low  <- rev(pal_material("blue")(length(bin_up)))
    col_up   <- pal_material("red")(length(bin_low))
    col_mid <- "white"
    var_col <- c(col_low, col_mid, col_up) 

  }

  df <- data %>% ungroup() %>%
      mutate(bins = cut(metric, breaks = bins, dig.lab = 5, include.lowest = T))

  #Base-plot
  gg <- ggplot(df, aes(x = tavg, y = prcp)) +
    geom_tile(aes(fill = bins), color = "gray70") +
    scale_fill_manual(values=var_col, drop = FALSE) +
    scale_x_continuous(expand = c(0,0), breaks = tavg_breaks) +
    scale_y_continuous(expand = c(0,0), breaks = prcp_breaks) +
    labs(x = expression("Temperature change (" * degree * C *")"),
         y = "Precipation change (%)", color = NULL, fill = NULL) 

  return(gg)

}



