

ggACF <- function(data, name.arg = NULL, ...) {
  
   df1 <- acf(data, plot = FALSE, ...)
   df2 <- data.frame(lag = df1$lag, acf = df1$acf)
   
   #Confidence intervals
   ci <- 0.95 # Indicates 95% confidence level
   clim0 <- qnorm((1 + ci)/2)/sqrt(df1$n.used)
   clim <- c(-clim0,clim0)
   hline.data <- data.frame(z=c(0,clim), type=c("base","ci","ci"))

   #Plot acf
   p <- ggplot(df2) +
    ggtitle(name.arg) +
    geom_hline(aes(yintercept = z, color = type,linetype = type), hline.data) +
    geom_linerange(aes(x = lag, ymin = 0, ymax = acf, size = 0.8)) +
    scale_colour_manual(values = c("black","red")) +
    scale_linetype_manual(values =c("solid","dashed")) +
    scale_y_continuous(limits=c(-0.5,1), breaks=seq(-0.50,1,0.5)) +
    labs(x=NULL, y=NULL) +
    theme(legend.position="none")
   
   
    return(p)
}