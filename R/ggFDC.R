

#Flow-duration-curves
ggFDC <- function(..., log.scale = FALSE) {

    require(dplyr)
    require(ggplot2)
    
    #Dataframe
    df <- data_frame(...) 
    
    df <- df %>%
      gather(key = variable, value = value) %>%
      group_by(variable) %>%
      mutate(value = sort(value, decreasing = TRUE, na.last = TRUE),
        x = 100*(1:n())/(n()+1))
    
    var_levels <- unique(df$variable)
    var_colors <- c("black", "blue", "red", "yellow")[1:length(var_levels)]
    
    #Print ggplot object
    p <- ggplot(df, aes(x = x, y = value)) +
      geom_line(aes(color = variable, group = variable)) +
      scale_color_manual(values = var_colors)  +
      xlab("Percentage exceedance (%)") 
    
    if(log.scale == TRUE) {
    
      require(scales)
      
      p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x))) 
    }
    
    return(p)
}


