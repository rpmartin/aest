#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param group PARAM_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @param limit_groups PARAM_DESCRIPTION, Default: NULL
#' @param show_percent PARAM_DESCRIPTION, Default: FALSE
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @param caption PARAM_DESCRIPTION, Default: NULL
#' @param lab_size PARAM_DESCRIPTION, Default: 8
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_treemap
#' @export
#' @import forcats
#' @import ggplot2
#' @import treemapify
#' @import scales
aest_treemap <- function(df, group, value, limit_groups=NULL, show_percent=FALSE, title=NULL, subtitle=NULL,caption=NULL,lab_size=5){
  if(is.null(limit_groups)){
    df <- df%>%
      mutate(group={{  group  }},
             value={{  value  }},
             prop=value/sum(value))
  }else{
    df <- df%>%
      mutate(group=fct_lump({{  group  }}, n=limit_groups, w={{  value  }}))%>%
      group_by(group)%>%
      summarize(value=sum({{  value  }}))%>%
      mutate(prop=(value/sum(value)))
  }
  if(show_percent==TRUE){
    df <- df%>%
      mutate(value=prop)
  }
  df <- df%>%
    mutate(label_colour=case_when(value<mean(value)~"white",
                                  value>=mean(value)~"black"),
           label=case_when(show_percent==TRUE ~ paste0(group,"\n(", round(100*value),"%)"),
                           show_percent==FALSE ~ paste0(group,"\n(", label_comma(1)(value),")")),
           label_size=lab_size+lab_size*(value-min(value))/(max(value)-min(value))
    )

  p <- ggplot(df, aes(area = value , fill = value, label = label))+
    geom_treemap()+
    geom_treemap_text(colour = df$label_colour,
                      place = "centre",
                      size=df$label_size) +
    labs(title=title,
         caption=caption,
         subtitle=subtitle)

  if(show_percent==TRUE){
    p <- p+scale_fill_viridis_c(labels = percent)
  }else{
    p <- p+scale_fill_viridis_c(labels = comma)
  }
  aest_fix_labs(p)
}
