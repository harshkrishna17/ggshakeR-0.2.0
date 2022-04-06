#' Plotting football pitch with jdp line markings
#' 
#' This function plots a football pitch with juego de posicion markings to improve the visual value of 
#' visualizations
#' 
#' @param pitchType The coordinates of the pitch based on the data provider. Options are opta & statsbomb. Default set to "statsbomb"
#' @param fill The colour of the pitch. Default set to white.
#' @param colour The colour of the pitch outline and jdp lines, Default set to black.
#' 
#' @return a ggplot2 object
#' 
#' @import ggplot2
#' @import ggsoccer
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' pitch <- pitch_jdp(pitchType = "opta", fill = "black", colour = "white")
#' pitch
#' }

pitch_jdp <- function(pitchType = "statsbomb", fill = "white", colour = "black") {
  
  if(pitchType == "opta") {
    
    pitch <- ggplot() +
      annotate_pitch(dimensions = pitch_opta, fill = fill, colour = colour) +
      geom_segment(aes(x = 17, xend = 83, y = 21.1, yend = 21.1), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 17, xend = 83, y = 78.9, yend = 78.9), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 17, xend = 17, y = 100, yend = 78.9), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 33.5, xend = 33.5, y = 100, yend = 78.9), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 66.5, xend = 66.5, y = 100, yend = 78.9), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 83, xend = 83, y = 100, yend = 78.9), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 17, xend = 17, y = 0, yend = 21), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 33.5, xend = 33.5, y = 0, yend = 21), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 66.5, xend = 66.5, y = 0, yend = 21), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 83, xend = 83, y = 0, yend = 21), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 17, xend = 83, y = 61.5, yend = 61.5), linetype = "longdash", alpha = 0.7, color = colour) +
      geom_segment(aes(x = 17, xend = 83, y = 39, yend = 39), linetype = "longdash", alpha = 0.7, color = colour)
  }
  else if(pitchType == "statsbomb") {
    
    pitch <- ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb, fill = fill, colour = colour) +
      geom_segment(aes(x = 18, xend = 102, y = 62, yend = 62), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 18, xend = 102, y = 18, yend = 18), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 18, xend = 18, y = value4, yend = 62), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 39, xend = 39, y = 80, yend = 62), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 81, xend = 81, y = 80, yend = 62), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 102, xend = 102, y = 80, yend = 62), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 18, xend = 18, y = 0, yend = 18), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 39, xend = 39, y = 0, yend = 18), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 81, xend = 81, y = 0, yend = 18), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 102, xend = 102, y = 0, yend = 18), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 18, xend = 102, y = 28, yend = 28), linetype = "longdash", alpha = 0.7, colour = colour) +
      geom_segment(aes(x = 18, xend = 102, y = 52, yend = 52), linetype = "longdash", alpha = 0.7, colour = colour)
  }
  return(pitch)
}
