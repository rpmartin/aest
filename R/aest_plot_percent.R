#' @title aest_plot_percent
#' @description makes a time series plot where y variable is percent formatted
#' @param df the dataframe that contains the data you want to plot
#' @param x_var the variable that will go on the x-axis... likely time.
#' @param y_var the thing you are plotting (could be a value, change...)
#' @param filter_var a column in the dataframe that you are filtering on.
#' @param plot_this a particular value of filter_var (the series you want to plot.)
#' @param geom either column or line
#' @param bcPalette colour palette for BC government
#' @return a ggplot object
#' @details This function is nearly identical with aest_plot_comma.  The only difference is the formatting for the y axis and the tool tip.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_plot_percent
#' @export
aest_plot_percent <- function(df,
                              x_var,
                              y_var,
                              filter_var,
                              plot_this,
                              geom,
                              bcPalette=c("#1f4181", "#fabc29", "#000000", "#808080")){
  if(plot_this!="all_series"){
    df <- df%>%
      filter({{  filter_var  }} %in% plot_this)
  }
  plt <- df%>%
    ggplot(aes({{  x_var  }},
               {{  y_var  }},
               colour={{  filter_var  }},
               fill={{  filter_var  }},
               group={{  filter_var  }},
               text= paste(str_to_title(str_replace_all({{  filter_var  }},"_"," ")),
                           "\n", {{  x_var  }},
                           "\n", percent(round({{  y_var  }},2)))))+
    scale_y_continuous(labels=percent)+
    scale_fill_manual(values = bcPalette)+
    scale_colour_manual(values = bcPalette)+
    labs(title=paste0(substitute(y_var)," of ", plot_this),
         x="",
         y="")
  ifelse(geom=="line", plt <- plt+geom_line(), plt <- plt+geom_col())
  aest_fix_labs(plt)+
    theme(legend.position='none')
}
