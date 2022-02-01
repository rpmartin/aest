#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param var_name PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION
#' @param scale_y PARAM_DESCRIPTION
#' @param caption PARAM_DESCRIPTION
#' @param text_size PARAM_DESCRIPTION
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
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @importFrom plotly add_annotations
#' @importFrom plotly subplot
plot_level_and_change <- function(df,
                                  var_name,
                                  title,
                                  scale_y,
                                  caption,
                                  text_size,
                                  caption_size){
  if(is.null(caption)){
    caption_text <- paste0('Variable name: "', var_name, '" File name: "', file_name,'"')
  }else{
    caption_text <- caption
  }
  plt1 <- ggplot(df ,aes(date, level))+
    geom_line()+
    geom_point(aes(text = paste("On",date,"the level was ",comma(level, accuracy = .01))), alpha=0)+
    scale_y_continuous(trans=scale_y, labels=comma)+
    theme(text=element_text(size=text_size))
  plt1 <- aest_fix_labs(plt1)%>%
    ggplotly(tooltip = "text")%>%
    config(displayModeBar = F)
  plt2 <-ggplot(df,aes(date, change))+
    geom_col()+
    geom_point(aes(text = paste("On", date, "the ", description, " rate of change was ",round(change*100),"%")), alpha=0)+
    scale_y_continuous(labels=percent)+
    labs(x="date",
         y=paste0("Change (",df$description,")"),
         title=title)+
    theme(text=element_text(size=text_size))
  plt2 <- aest_fix_labs(plt2)%>%
    ggplotly(tooltip = "text")%>%
    config(displayModeBar = F)%>%
    add_annotations(
      text = ~caption_text,
      x = 1,
      y = -.15,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = caption_size)
    )
  subplot(plt1, plt2, nrows = 2, margin = 0.04, titleY=TRUE, shareX=TRUE, heights = c(0.5, 0.5))
}
