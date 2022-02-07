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
    caption_text <- paste0('Variable name: "', var_name, '" File name: "', df$file_name[1],'"')
  }else{
    caption_text <- caption
  }
  plt1 <- ggplot(df ,aes(date, level))+
    geom_line()+
    scale_y_continuous(trans=scale_y, labels=comma)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0)) # Left margin
  plt1 <- aest_fix_labs(plt1)

  plt2 <-ggplot(df,aes(date, change))+
    geom_col()+
    scale_y_continuous(labels=percent)+
    labs(x="",
        y=paste0("Change (",df$description,")"),
        title=title,
        caption=caption_text)+
    theme(plot.caption = element_text(size=caption_size),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0)) # Left margin

  plt2 <- aest_fix_labs(plt2)
  plt1/plt2+ xlab(label = "Date")
}
