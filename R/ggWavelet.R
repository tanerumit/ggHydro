

#' @title ggplot wrapper for wavelet spectra plot
#' @description FUNCTION_DESCRIPTION
#' @param period fourier periods
#' @param sig sigicance level
#' @param obs observed global wavelet spectra 
#' @param sim simulated global wavelet spectra  default: NULL
#' @return global wavelet spectra plot
#' @details place holder
#' @rdname ggWaveletSpectra <- function(period, sig, obs, sim = NULL) {
#' @export 

ggWaveletSpectra <- function(period, sig, obs, sim = NULL) {
  
  require(ggplot2)
  require(scales)
  require(dplyr)

  if(is.null(sim)) {
    
    df <- data_frame(period, sig, obs)
    
    p <- ggplot(df, aes(x = period)) + 
      geom_line(aes(y=sig), color = "red", linetype = "dashed", size=0.6) +
      geom_line(aes(y=obs), color = "blue", size=0.6)
    
  } else {
    
    savg <- apply(sim, 1, mean)
    slow <- apply(sim, 1, function(x) quantile(x, 0.025))
    sup  <- apply(sim, 1, function(x) quantile(x, 0.975))
    
    df <- data_frame(period, sig, obs, slow, sup, savg)
    
    p <- ggplot(df, aes( x = period)) +
      geom_ribbon(aes(ymin = slow, ymax = sup), alpha = 0.2) +
      geom_line(aes(y=sig), color = "red" , linetype = "dashed", size=0.6) +
      geom_line(aes(y=savg), color = "black", size=0.6) +
      geom_line(aes(y=obs), color = "blue", size=0.6) 
  }
  
  p <- p + theme_light(base_size = 11) + 
    labs(x="Period (years)", y = expression(paste("Power (", mm^2,")"))) #+
    #scale_x_continuous(breaks=seq(0,50,5), expand=c(0,0)) +
    #scale_y_log10(limits = c(5*10**2,5*10**5),labels = comma)
  
  return(p)
  
}
