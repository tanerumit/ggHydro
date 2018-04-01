

gcmScatterPlot <- function( data = NULL, hist_period = NULL, proj_period = NULL, 
  scenarios = c("rcp26", "rcp85"), 
  tavg_axis_breaks = NULL, prcp_axis_breaks = NULL, 
  save = FALSE, plot_historical = TRUE) 
  
  {
  
  #browser()
  
  require(ggplot2)
  require(dplyr)
  
  snlabs <- list("rcp26" = "RCP2.6", "rcp45" = "RCP4.5", "rcp60" = "RCP6.0", "rcp85" = "RCP8.5")
  sncols <- list("rcp26" = "blue3", "rcp45" = "dodgerblue", "rcp60" = "darkorange", "rcp85" = "firebrick")

  snlabs_sel <- snlabs[which(names(snlabs) %in% scenarios)] %>% unlist(use.names = FALSE)
  sncols_sel <- sncols[which(names(snlabs) %in% scenarios)] %>% unlist(use.names = FALSE) 
  
  scenarios_all = c("historical", scenarios)
  
  
  # Summarize data for each projection

  df <- lapply(names(data), 
    function(x) bind_rows(data[[x]], .id = "model")) %>%
    setNames(names(data)) %>%
    bind_rows(.id = "scenario") %>% ungroup() %>%
    filter(scenario %in% scenarios_all)
  
  data_hist <- df %>% filter(scenario == "historical") %>%
    filter(year %in% hist_period) %>%
    group_by(model) %>%
    summarize_at(vars(prcp, tavg), mean) %>%
    mutate(prcp = prcp * 12)
  
  data_proj <- df %>% filter(scenario != "historical") %>%
    filter(year %in% proj_period) %>%
    group_by(scenario, model) %>%
    summarize_at(vars(prcp, tavg), mean) %>%
    mutate(prcp = prcp * 12)

  # mean precip and temp changes
  delta_prcp <- data_proj %>% 
    select(scenario, model, prcp) %>%
    left_join(select(data_hist, model, hist_prcp = prcp), by = "model") %>%
    mutate(prcp = (prcp - hist_prcp) / hist_prcp * 100)
  
  delta_tavg <- data_proj %>% 
    select(scenario, model, tavg) %>%
    left_join(select(data_hist, model, hist_tavg = tavg), by = "model") %>%
    mutate(tavg = tavg - hist_tavg)

  delta_clim <- delta_prcp %>%
    left_join(delta_tavg, by = c("scenario", "model")) %>%
    na.omit() %>% select(scenario, model, prcp, tavg) 

  # Axis breaks (if not provided)
  if(is.null(tavg_axis_breaks)) {
    tavg_axis_breaks <- seq(0, round(max(delta_tavg$tavg, na.rm = T),0) + 2, 1)
  }
  if(is.null(prcp_axis_breaks)) {
  prcp_axis_breaks <- seq(
    round(min(delta_prcp$prcp, na.rm = TRUE),-1) -20,
    round(max(delta_prcp$prcp, na.rm = TRUE),-1) +20,
    10)
  }

  # Axis limits
  tavg_step <- (tavg_axis_breaks[2] - tavg_axis_breaks[1])/2 
  tavg_axis_lim  <- range(tavg_axis_breaks) + c(- tavg_step, tavg_step)
  prcp_step <- (prcp_axis_breaks[2] - prcp_axis_breaks[1])/2 
  prcp_axis_lim  <- range(prcp_axis_breaks) + c(- prcp_step, prcp_step)

  # Set plot title  
  histp_range <- paste(min(hist_period), max(hist_period), sep = "-")
  projp_range <- paste(min(proj_period), max(proj_period), sep = "-")
  title <- paste0("Climate Changes:\nFuture period (", 
    projp_range,") - Historical period (", histp_range, ")")
  gg_name <- paste0("./gcmScatter_hist(",histp_range,")_proj(",projp_range,").png")


  #Base-plot
  gg <- ggplot(mapping = aes(x = tavg, y = prcp)) + 
    geom_point(aes(color = scenario), delta_clim, shape = 1, stroke = 1, size = 2, alpha = 0.8) +
    scale_color_manual(values = sncols_sel, breaks = scenarios, labels = snlabs_sel) + 
    scale_x_continuous(expand = c(0,0), breaks = tavg_axis_breaks, limits = tavg_axis_lim) +
    scale_y_continuous(expand = c(0,0), breaks = prcp_axis_breaks, limits = prcp_axis_lim, labels = prcp_axis_breaks) +
    geom_point(aes(x = 0, y = 0), shape = 8, size = 2)
  
  #Modify plot labels
  gg <- gg + labs(title = title, 
      x = expression("Temperature change (" * degree * C *")"), 
      y = "Precipation change (%)", color = NULL)
  
  #Save output to file
  if(save == TRUE) ggsave(gg_name, height = 6.5, width = 7) 
  
  return(gg)
  
}