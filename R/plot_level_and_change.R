#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param var_name PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION
#' @param scale_y PARAM_DESCRIPTION
#' @param caption PARAM_DESCRIPTION
#' @param caption_size PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_level_and_change
#' @export
#' @import patchwork
plot_level_and_change <- function(df,
                                  var_name,
                                  title,
                                  scale_y,
                                  caption,
                                  caption_size){
  if(is.null(caption)){
    caption_text <- paste0('Variable name: "', var_name, '" File name: "', file_name,'"')
  }else{
    caption_text <- caption
  }
  plt1 <- ggplot(df ,aes(date, level))+
    geom_line()+
    scale_y_continuous(trans=scale_y, labels=comma)
  plt1 <- aest_fix_labs(plt1)

  plt2 <-ggplot(df,aes(date, change))+
    geom_col()+
    scale_y_continuous(labels=percent)+
    labs(x="date",
         y=paste0("Change (",df$description,")"),
         title=title,
         caption=caption_text)+
    theme(plot.caption = element_text(size=caption_size))
  plt2 <- aest_fix_labs(plt2)
  plt1/plt2
}
