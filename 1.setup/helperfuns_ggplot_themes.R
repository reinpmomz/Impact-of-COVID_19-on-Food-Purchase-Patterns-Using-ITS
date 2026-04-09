library(ggplot2)

### colours
GRAY1 <- "#231F20"
GRAY2 <- "#414040"
GRAY3 <- "#555655"
GRAY4 <- "#646369"
GRAY5 <- "#76787B"
GRAY6 <- "#828282"
GRAY7 <- "#929497"
GRAY8 <- "#A6A6A5"
GRAY9 <- "#BFBEBE"
BLUE1 <- "#174A7E"
BLUE2 <- "#4A81BF"
BLUE3 <- "#94B2D7"
BLUE4 <- "#94AFC5"
BLUE5 <- "#22435e"
BLUE6 <- "#95B3D7"
RED1 <- "#C3514E"
RED2 <- "#E6BAB7"
RED3 <- "#800000"
GREEN1 <- "#0C8040"
GREEN2 <- "#9ABB59"
GREEN3 <- "#31859C"
GREEN4 <- "#4BACC6"
GREEN5 <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"

### ggplot themes

ggtheme_descriptive_plot <- function(angletext_yaxis=0, angletext_xaxis=0,
                                     striptext_size_x = 9, striptext_size_y = 9){
  theme_set(theme_minimal() +
              theme(
                legend.position="bottom",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
                axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
                axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
                axis.ticks.y = element_line(linewidth = 0.5, color="black"),
                axis.ticks.x = element_line(linewidth = 0.5, color="black"),
                axis.text.y = element_text(angle = angletext_yaxis, lineheight = 0.7, hjust = 0.5),
                axis.text.x = element_text(angle = angletext_xaxis, lineheight = 0.7, vjust = 0.5),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                plot.caption = element_text(angle = 0, size = 10, face = "italic"),
                axis.title.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                strip.text.x = element_text(size = striptext_size_x),
                strip.text.y = element_text(size = striptext_size_y),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
              )
  )
}

ggtheme_rank_plot <- function() {
  theme_set( theme_bw(base_size=11) +
               theme( 
                 legend.position="right", #default
                 axis.text.x = element_text(angle = 0, lineheight = 0.6),
                 axis.text.y = element_text(angle = 0, lineheight = 0.6),
                 plot.title = element_text(hjust = 0.5),
                 axis.title.x = element_text(size = 12, face = "bold"),
                 axis.title.y = element_text(size = 12, face = "bold"),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "grey"
                                             , fill = NA
                                             , linewidth = 0.8
                                             ),
                 strip.text.x = element_text(size = 8
                                             , colour = "black"
                                             , face = "bold"
                                             )
               )
  )
}

ggtheme_sf_plot <- function(){
  theme_set(#theme_minimal() +
    theme(
      rect = element_blank(),
      #plot.background = element_blank(),
      #panel.background = element_rect(fill = "white"),
      #legend.position="bottom",
      #legend.text = element_text(size = 8),
      #legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
      #axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
      #axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      plot.caption = element_text(angle = 0, size = 10, face = "italic"),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
    )
  )
}

ggtheme_heat_plot = function(angletext_yaxis=0, angletext_xaxis=0) {
  theme_set(theme_bw(base_size = 10)
            + theme(panel.grid = element_blank()
                    , plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
                    , axis.title.x = element_blank()
                    , axis.title.y = element_blank()
                    , axis.text.y = element_text(size = 8.5, angle = angletext_yaxis, lineheight = 0.7)
                    , axis.text.x = element_text(size = 8.5, angle = angletext_xaxis, lineheight = 0.7)
            )
  )
}

ggtheme_regression_plot = function(angletext_yaxis=0, angletext_xaxis=0) {
  theme_set(theme_bw(base_size = 10)
            + theme(panel.grid = element_blank()
                    , plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
                    , axis.title.x = element_text(size = 10, face = "bold")
                    , axis.title.y = element_text(size = 10, face = "bold")
                    , axis.text.y = element_text(size = 8.5, angle = angletext_yaxis, lineheight = 0.7)
                    , axis.text.x = element_text(size = 8.5, angle = angletext_xaxis, lineheight = 0.7)
            )
  )
}

