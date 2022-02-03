#' @title Plot parts and sum of parts
#' @description From a dataframe containing two discrete and one numeric variable,  plots bars for each combination of the discrete variables and a line for the aggregate for each value of x_var
#' @param df dataframe that contains (at a minimum) two discrete and one numeric variables.
#' @param x_var a discrete variable to plot on the x-axis
#' @param group_var another discrete variable that the data is grouped by
#' @param measure a numeric variable to be plotted.
#' @param position Either "stack" or "dodge", Default: 'dodge'
#' @param legend_spot where to put the legend, Default: 'bottom'
#' @param caption_size font size for caption, Default=5
#' @param line_colour the colour of the line
#' @param title A title, Default: NULL
#' @param subtitle A subtitle, Default: NULL
#' @param caption A caption, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_parts_and_whole
#' @export
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @import dplyr
aest_parts_and_whole <- function(df,
                                 x_var,
                                 group_var,
                                 measure,
                                 position="dodge",
                                 legend_spot="right",
                                 caption_size=5,
                                 line_colour="black",
                                 title=NULL,
                                 subtitle=NULL,
                                 caption=NULL){
  assert_that(is.data.frame(df))
  names_df <- names(df)
  assert_that(deparse(substitute(x_var)) %in% names_df)
  assert_that(deparse(substitute(group_var)) %in% names_df)
  assert_that(deparse(substitute(measure)) %in% names_df)
  assert_that("file_name" %in% names_df)
  assert_that(length(unique(df[[deparse(substitute(x_var))]]))<20)
  assert_that(length(unique(df[[deparse(substitute(group_var))]]))<10)
  assert_that(is.numeric(df[[deparse(substitute(measure))]]))


  if(is.null(caption)){
    caption_text <- paste0('Variable name: "', substitute(measure), '" File name: "', df$file_name[1],'"')
  }else{
    caption_text <- caption
  }

  net <- df%>%
    group_by({{  x_var  }})%>%
    summarize(net=sum({{  measure  }}))
  p <-  ggplot()+
    geom_col(data=df,
             aes({{  x_var  }},{{  measure  }},fill={{  group_var  }}),
             position=position)+
    scale_fill_viridis_d()+
    geom_line(data=net,aes({{  x_var  }}, net, colour='Net'))+
    geom_point(data=net,aes({{  x_var  }}, net, colour='Net'))+
    scale_color_manual(name=NULL,
                       breaks=c('Net'),
                       values=c('Net'=line_colour))+
    labs(title=title,
         subtitle=subtitle,
         caption=caption_text)+
    theme(legend.position = legend_spot,
          plot.caption = element_text(size=caption_size))
  aest_fix_labs(p)
}
