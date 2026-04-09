library(dplyr)
library(forcats)
library(scales)
library(labelled)
library(stringr)
library(ggplot2)
library(cowplot)

### Grouped Line graphs
line_group_count_grid_plot <- 
  function(df, x_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, facet_wrap=FALSE,
           facet_ncol = 4, title_label = NULL, legend=TRUE, nrow_legend = 1, ncol_legend = NULL,
           y_axis_breaks =10, x_axis_breaks =10, x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL),
           x_axis_min_max_break_months = 2, facet_scales = "free_y", y_axis_limits = c(NULL, NULL),
           line_width = 0.5, expand_xaxis = c(0.04,0.04), expand_yaxis = c(0.02,0.05),
           title_label_wrap_width = 35, x_axis_date_label = "%Y-%m", x_axis_label_wrap_width = 20,
           add_point = FALSE, point_size = 1, point_shape = 19, add_text = FALSE, text_label_size = 3,
           text_label_digits = 0, text_label_angle = 0, text_label_fontface = "bold", 
           text_label_position = "jitter", text_label_overlap = FALSE, text_label_size_unit = "mm",
           grid_ncol =  1, grid_nrow = NULL) {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             dplyr::mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), sjlabelled::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
        } else {
      labelled::var_label(index_colour)
          }
    
      p <- ggplot(data=df, aes(x=index, colour=index_colour, group=index_colour))
      
      p1 <- if (legend == TRUE) {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
      } else {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE)
            }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE)
            }
          }
        }
      
      p2 <- if ("Date" %in% class(index)) {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if ("POSIXct" %in% class(index) | "POSIXt" %in% class(index)) {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if ("factor" %in% class(index)) { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else if ("numeric" %in% class(index) | "integer" %in% class(index)) { 
          p1 +
            scale_x_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = x_axis_limits,
                                n.breaks = x_axis_breaks, expand = expansion(mult = expand_xaxis))
          } else {
            p1
            }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, colour=index_colour, group=index_colour))
      
      p1 <- if (legend == TRUE) {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          }
        }
      } else {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE)
          }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE)
          }
        }
      }
      
      p2 <- if ("Date" %in% class(index)) {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if ("POSIXct" %in% class(index) | "POSIXt" %in% class(index)) {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if ("factor" %in% class(index)) { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
        } else if ("numeric" %in% class(index) | "integer" %in% class(index)) { 
          p1 +
            scale_x_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = x_axis_limits,
                                n.breaks = x_axis_breaks, expand = expansion(mult = expand_xaxis))
          } else {
            p1
            }
      
      p3 <- p2 
      
    }
    
    plot1 <- plot +
      scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                          n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    
    plot2 <- if (is.null(title_label)) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(title_label, width = title_label_wrap_width))
    }
    
    plot2
    
  })
  out
  
  grid <- cowplot::plot_grid(plotlist = out, ncol = grid_ncol, nrow = grid_nrow)
}

line_group_sum_mean_grid_plot <- 
  function(df, x_vars, y_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, stat_fun_sum = TRUE,
           title_label = NULL, facet_wrap=FALSE, legend=TRUE, nrow_legend = 1, ncol_legend = NULL, facet_ncol = 4,
           y_axis_breaks =10, x_axis_breaks =10 , x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL),
           x_axis_min_max_break_months = 2, y_axis_label_percent = FALSE, y_axis_limits = c(NULL, NULL),
           facet_scales = "free_y", line_width = 0.5, title_label_wrap_width = 35, expand_xaxis = c(0.04,0.04), 
           x_axis_date_label = "%Y-%m", x_axis_label_wrap_width = 20, expand_yaxis = c(0.02,0.05),
           add_point = FALSE, point_size = 1, point_shape = 19, add_text = FALSE, text_label_size = 3, 
           text_label_digits = 0, text_label_angle = 0, text_label_fontface = "bold",
           text_label_position = "jitter", text_label_overlap = FALSE, text_label_size_unit = "mm",
           grid_ncol =  1, grid_nrow = NULL) {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             mutate(across(where(is.logical), sjlabelled::as_factor))
    )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour) 
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y, color=index_colour, group=index_colour,
                               label = round(after_stat(y),text_label_digits))
                  )
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) + 
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) 
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) 
              }
            
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) 
              }
            }
          }
      } else {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
            }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE)
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) 
              }
          }
        }
      }
      
      p2 <- if ("Date" %in% class(index)) {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if ("POSIXct" %in% class(index) | "POSIXt" %in% class(index)) {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if ("factor" %in% class(index)) { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else if ("numeric" %in% class(index) | "integer" %in% class(index)) { 
          p1 +
            scale_x_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = x_axis_limits,
                                n.breaks = x_axis_breaks, expand = expansion(mult = expand_xaxis))
          } else {
            p1
            }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y, color=index_colour, group=index_colour,
                               label = round(after_stat(y),text_label_digits))
                  )
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) + 
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) 
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) 
              }
            
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) 
              }
            }
          }
      } else {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
            }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE)
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) 
              }
          }
        }
      }
      
      p2 <- if ("Date" %in% class(index)) {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if ("POSIXct" %in% class(index) | "POSIXt" %in% class(index)) {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if ("factor" %in% class(index)) { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else if ("numeric" %in% class(index) | "integer" %in% class(index)) { 
          p1 +
            scale_x_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = x_axis_limits,
                                n.breaks = x_axis_breaks, expand = expansion(mult = expand_xaxis))
          } else {
            p1
            }
      
      p3 <- p2 
      
    }
    
    plot1 <- if (y_axis_label_percent == TRUE) {
      plot +
        scale_y_continuous( labels = scales::percent, limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    } else {
      plot +
        scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    }
    
    plot2 <- if (is.null(title_label)) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(title_label, width = title_label_wrap_width))
    }
    
    plot2
  })
  out
  
  grid <- cowplot::plot_grid(plotlist = out, ncol = grid_ncol, nrow = grid_nrow)
}


