#' Function for plotting convex hulls.
#'
#' This function allows for data, that can be from Opta or Statsbomb, to be used
#' for plotting convex hulls on top of an outline of a football pitch.
#'
#' @param eventData Dataframe that houses pass data. Opta dataframe must contain atleast the following columns: `x`, `y`, `finalX`, `finalY`, `playerId`. With StatsBomb data, the default dataset is to be used. 
#' @param dataType Type of data that is being put in: opta or statsbomb. Default set to "statsbomb"
#' @param colour The colour of the outline of the convex hull.
#' @param titlePlot Title of the plot
#' @return a ggplot2 object
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggsoccer
#' @import purrr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot <- plot_convexhull(eventData, dataType = "statsbomb", colour = "blue")
#' plot
#' }

plot_convexhull <- function(eventData, dataType = "statsbomb", 
                            colour, titlePlot = "", theme = "") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    colour_b <- "white"
  } else if (theme == "white") {
    fill_b <- "#F5F5F5"
    colour_b <- "black"
  } else if (theme == "rose") {
    fill_b <- "#FFE4E1"
    colour_b <- "#696969"
  } else if (theme == "almond") {
    fill_b <- "#FFEBCD"
    colour_b <- "#696969"
  }
    
  if(dataType == "opta") {
    if(nrow(eventData) > 0 &&
          sum(x = c("x", "y", "finalX", "finalY", "playerId") %in% names(eventData)) == 5) {
      } else {
        stop("The dataset has insufficient columns")
      }
      
      to_sb <- rescale_coordinates(from = pitch_opta, to = pitch_statsbomb)
      eventData$x <- to_sb$x(eventData$x)
      eventData$y <- to_sb$y(eventData$y)
      
      eventData <- eventData %>%
        drop_na(playerId, x, y)
      
      } else if(dataType == "statsbomb") {
      
      eventData <- eventData %>%
        rename(x = location.x,
               y = location.y,
               playerId = player.name) %>%
        drop_na(playerId, x, y)
    }
  
  data <- eventData
  
  x_low <- quantile(data$x, 0.05)
  x_high <- quantile(data$x, 0.95)
  
  y_low <- quantile(data$y, 0.05)
  y_high <- quantile(data$y, 0.95)
  
  list_data <- split(data, data$playerId)
  
  hull_fun <- function(data) {
    hull_data <- data %>%
      filter((x > x_low) & (x < x_high)) %>%
      filter((y > y_low) & (y < y_high)) %>%
      slice(chull(x, y))
    
    return(hull_data)
  }
  
  hull_data <- list_data %>%
    purrr::map(hull_fun) %>%
    purrr::reduce(full_join)
  
  if(titlePlot == "") {
    titlePlot <- "Convex Hulls"
  } else {
    
  }

  convex_hull <- ggplot(hull_data) +
    annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colour_b) +
    theme_pitch() +
    geom_point(data = data, aes(x = x, y = y), alpha = 0.5, colour = colour_b) +
    geom_polygon(aes(x = x, y = y), colour = colour, alpha = 0.2, fill = colour, size = 1) +
    facet_wrap(~playerId) +
    labs(title = titlePlot) +
    theme(plot.background = element_rect(fill = fill_b, colour = NA),
          panel.background = element_rect(fill = fill_b, colour = NA),
          strip.background = element_rect(fill = fill_b, colour = NA),
          strip.text = element_text(colour = colour_b, size = 10),
          plot.title = element_text(colour = colour_b, size = 18, hjust = 0.5, face = "bold"))
  
  return(convex_hull)
}
