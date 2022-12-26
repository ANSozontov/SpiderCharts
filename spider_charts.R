#' Spider sharts
#'
#' @param df Incoming `data.frame`. If `NULL` then function provides an example.
#' @param top Maximum of radial scale.
#' @param subdivisions Number of internal sections on scale.
#' @param zero_shift Central shift from zero for getting more beauty central empty space.
#' @param label_shift Labels' shift out for more clear reading. 
#' @param scale_shist Shift 
#' @param webtype 
#' @param flexure 
#' @param need_label 
#' @param need_scale 
#' @param need_web 
#'
#' @return
#' @export
#'
#' @examples
#' rio::import("table.xlsx") %>% 
#' mutate(id = as_factor(id)) %>% 
#' spiderchart(label_shift = -10)+ 
#' facet_wrap(~year) + 
#' theme_classic() +
#' labs(title = "Changes in ecological structure of spiders' communities", 
#'        x = NULL, y = NULL) + 
#'   guides(fill = guide_legend(title="Biotop"),
#'          color= guide_legend(title="Biotop")) +
#'   theme(legend.position = "bottom", 
#'         plot.title = element_text(face = "bold", hjust = 0.5), 
#'         axis.line = element_blank(), 
#'         axis.text = element_blank(), 
#'         axis.ticks = element_blank())
#' 
#' 
spiderchart <- function(df = NULL, 
               top = NULL, 
               subdivisions = 4, # manual
               zero_shift = 15, # manual
               scale_shist = -5, # manual
               label_shift = 5, # manual
               label_size = 3.5, # manual
               webtype = "dotted", # manual
               flexure = 0.2, #manual
               need_label = TRUE, #manual
               need_scale = TRUE, # manual
               need_web = TRUE # manual
) {
  # require(crayon)
  if(is.null(df))
  {
    cat(crayon::red("There is no data!
"))
    cat(crayon::green("Do you need an example?
"))
    df <- tibble::tibble(id = c("A", "B", "C"), 
                         Z =      c(85, 25, 25), 
                         M =      c(25, 85, 25), 
                         big =    c(25, 25, 85), 
                         med =    c(85, 25, 25), 
                         sml =    c(25, 85, 25), 
                         runing = c(25, 25, 85), 
                         jumping= c(25, 85, 25))
    df
  } else {
    require(tidyverse)
    ID = max(which(!sapply(df, is.numeric)))
    N = ncol(df) - ID
    AL = 2*pi/(N)
    if(is.null(top)) {top = ceiling(max(df[,-c(1:ID)])/10)*10} # but can be manual
    
    
    df <- df %>% 
      rename(id = ID) %>% 
      pivot_longer(names_to = "lab", values_to = "L", -all_of(c(1:ID))) %>% 
      mutate(i = 0:(nrow(.)-1), 
             M = L + max(L)/3, 
             L = L + zero_shift,
             x = L*cos(pi/2-AL*i), 
             y = L*sin(pi/2-AL*i)) 
    
    G1 <- data.frame( # Web-Grid
      x1 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * cos(pi/2-AL*rep(c(1:N), subdivisions+1)),
      y1 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * sin(pi/2-AL*rep(c(1:N), subdivisions+1)), 
      x2 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * cos(pi/2-AL*rep(c(2:N, 1), subdivisions+1)),
      y2 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * sin(pi/2-AL*rep(c(2:N, 1), subdivisions+1))
    )
    G2 <- data.frame( # rays
      x1 = top*cos(pi/2-AL*1:N),
      y1 = top*sin(pi/2-AL*1:N), 
      x2 = zero_shift*cos(pi/2-AL*1:N),
      y2 = zero_shift*sin(pi/2-AL*1:N)
    )
    G3 <- data.frame(
      x = -5, 
      L = seq(0, top, by = top/subdivisions),
      y = seq(0, top, by = top/subdivisions) + zero_shift)
    G4 <- data.frame(
      lb = unique(df$lab),
      x = (max(df$L) + zero_shift + label_shift) * cos(pi/2-AL*0:(N-1)), 
      y = (max(df$L) + zero_shift + label_shift) * sin(pi/2-AL*0:(N-1))
    )
    
    ggplot() + 
      {if(need_web) # web: segments
        geom_curve(aes(x1, y1, xend = x2, yend = y2), data = G1, curvature = flexure, linetype = webtype)
      } +
      {if(need_web)  # web: radial rays
        geom_segment(aes(x1, y1, xend = x2, yend = y2), data = G2, linetype = webtype)
      } + 
      {if(need_scale) # scale
        geom_text(aes(x, y, label = L), data = G3)
      } +
      geom_polygon(aes(x, y, color = id, fill = id), data = df, alpha = 0.5) + # polygons
      geom_point(aes(x, y, color = id, fill = id), data = df) + # peaks
      {if(need_label) # peak labels
        geom_text(aes(x, y, label = lb), data = G4, size = label_size)
      } + 
      coord_equal() + 
      theme_void() 
  }
}









