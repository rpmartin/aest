#' @title Area Plot
#' @description makes either a stacked or filled (proportions) area plot
#' @param df a dataframe
#' @param x_var The variable to plot on the x axis
#' @param y_var The variable to plot on the y axis
#' @param group_var The grouping variable
#' @param stack_or_fill stack: original scale, fill: proportions, Default: 'stack'
#' @param title optional title, Default: NULL
#' @param subtitle optional subtitle, Default: NULL
#' @param caption If not provided, the caption is variable name and original file name, Default: NULL
#' @param caption_size font size for caption, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{label_number}}
#' @rdname aest_area_plot
#' @export
#' @importFrom scales comma

aest_area_plot <- function(df,
                           x_var,
                           y_var,
                           group_var,
                           stack_or_fill="stack",
                           title=NULL,
                           subtitle=NULL,
                           caption=NULL,
                           caption_size=5){

  assert_that(is.data.frame(df))
  names_df <- names(df)
  assert_that(deparse(substitute(x_var)) %in% names_df)
  assert_that(deparse(substitute(y_var)) %in% names_df)
  assert_that("file_name" %in% names_df)
  assert_that(is.numeric(df[[deparse(substitute(y_var))]]))

  group_label <- deparse(substitute(group_var))
  if(is.null(caption)){
    caption_text <-paste0('Variable name: "',
                          deparse(substitute(y_var)),
                          '" File name: "',
                          df$file_name[1],'"')
  }else{
    caption_text <- caption
  }
  df <- df%>%
    mutate(ordered_group=fct_reorder({{  group_var  }},{{  y_var  }}))

  p <-   ggplot(df,aes({{  x_var  }},{{  y_var  }},fill=ordered_group))+
    geom_area(position = stack_or_fill)+
    scale_fill_viridis_d()+
    theme_minimal()+
    guides(fill = guide_legend(title = group_label))+
    labs(title=title,
         subtitle=subtitle,
         caption=caption_text)+
    theme(plot.caption = element_text(size=caption_size))

  if(stack_or_fill=="stack"){
    p <- p + scale_y_continuous(labels=scales::comma)
  }else if(stack_or_fill=="fill"){
    p <- p + scale_y_continuous(labels=scales::percent)
  }else{
    p
  }

  aest_fix_labs(p)
}
