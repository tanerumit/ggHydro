

gcmScatterPlot <- function(data = NULL, hist.period = NULL, proj.period = NULL,
  scenarios = c("rcp26", "rcp85"),tavg.breaks = NULL, prcp.breaks = NULL,
  save = FALSE, plot_historical = TRUE, plot.title = TRUE, chull = FALSE,
  output.df = FALSE)

{
  #browser()

  require(ggplot2)
  require(dplyr)
  require(ggConvexHull)

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
    filter(year %in% hist.period) %>%
    group_by(model) %>%
    summarize_at(vars(prcp, tavg), mean) %>%
    mutate(prcp = prcp * 12)

  data_proj <- df %>% filter(scenario != "historical") %>%
    filter(year %in% proj.period) %>%
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
  if(is.null(tavg.breaks)) {
    tavg.breaks <- seq(0, round(max(delta_tavg$tavg, na.rm = T),0) + 2, 1)
  }
  if(is.null(prcp.breaks)) {
  prcp.breaks <- seq(
    round(min(delta_prcp$prcp, na.rm = TRUE),-1) -20,
    round(max(delta_prcp$prcp, na.rm = TRUE),-1) +20,
    10)
  }

  # Axis limits
  tavg_step <- (tavg.breaks[2] - tavg.breaks[1])/2
  tavg_lim  <- range(tavg.breaks) + c(- tavg_step, tavg_step)
  prcp_step <- (prcp.breaks[2] - prcp.breaks[1])/2
  prcp_lim  <- range(prcp.breaks) + c(- prcp_step, prcp_step)

  # Set plot title
  histp_range <- paste(min(hist.period), max(hist.period), sep = "-")
  projp_range <- paste(min(proj.period), max(proj.period), sep = "-")
  title <- paste0("Climate Changes:\nFuture period (",
    projp_range,") - Historical period (", histp_range, ")")
  gg_name <- paste0("./gcmScatter_hist(",histp_range,")_proj(",projp_range,").png")


  #Base-plot
  gg <- ggplot(delta_clim, aes(x = tavg, y = prcp)) +
    geom_point(aes(color = scenario), shape = 1, stroke = 1, size = 2, alpha = 0.8) +
    scale_color_manual(values = sncols_sel, breaks = scenarios, labels = snlabs_sel) +
    scale_fill_manual(values = sncols_sel, breaks = scenarios, labels = snlabs_sel) +
    scale_x_continuous(expand = c(0,0), breaks = tavg.breaks, limits = tavg_lim) +
    scale_y_continuous(expand = c(0,0), breaks = prcp.breaks, limits = prcp_lim, labels = prcp.breaks) +
    geom_point(aes(x = 0, y = 0), shape = 8, size = 2) +
    labs(x = expression("Temperature change (" * degree * C *")"),
         y = "Precipation change (%)", color = NULL, fill = NULL) +
    guides(fill=FALSE)

  #Plot title
  if(plot.title == TRUE) {gg <- gg + ggtitle(title)}

  #Draw convex hull
  if(chull == TRUE) {gg <- gg + geom_convexhull(aes(fill = scenario), delta_clim, alpha = 0.3)}

  #Save output to file
  if(save == TRUE) {ggsave(gg_name, height = 6.5, width = 7)}

  if(output.df == TRUE) {
    out <- list(gg = gg, df = delta_clim)
  } else {
    out <- list(gg)
  }

  return(out)

}
