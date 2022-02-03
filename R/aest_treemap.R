#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df A dataframe that must contain column file_name (for auto caption)
#' @param grp a grouping variable
#' @param vrbl the variable being plotted
#' @param limit_groups what is the maximum number of groups to consider, Default: NULL
#' @param title title if you want, Default: NULL
#' @param subtitle subtitle if you want, Default: NULL
#' @param caption override the default caption, Default: NULL
#' @param lab_size blunt control over labeling size, Default: 5
#' @param caption_size font size for caption Default: 5
#' @param legend_spot where to put the legend Default: "right"
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
aest_treemap <- function(df,
                         grp,
                         vrbl,
                         limit_groups=NULL,
                         title=NULL,
                         subtitle=NULL,
                         caption=NULL,
                         lab_size=5,
                         caption_size=5,
                         legend_spot="right"){
  if(is.null(caption)){
    caption_text <-paste0('Variable name: "', deparse(substitute(vrbl)), '" File name: "', df$file_name[1],'"')
  }else{
    caption_text <- caption
  }

  if(is.null(limit_groups)){
    df <- df%>%
      mutate(group={{  grp  }},
             value={{  vrbl  }},
             prop=value/sum(value))
  }else{
    df <- df%>%
      mutate(group=fct_lump({{  grp  }}, n=limit_groups, w={{  vrbl  }}))%>%
      group_by(group)%>%
      summarize(value=sum({{  vrbl  }}))%>%
      mutate(prop=(value/sum(value)))
  }
  df <- df%>%
    mutate(label_colour=case_when(value<mean(value)~"white",
                                  value>=mean(value)~"black"),
           label=paste0(group,"\n",label_comma(1)(value)," (",round(100*prop,1),"%)"),
           label_size=lab_size+lab_size*(value-min(value))/(max(value)-min(value))
    )

  p <- ggplot(df, aes(area = value , fill = value, label = label))+
    geom_treemap()+
    geom_treemap_text(colour = df$label_colour,
                      place = "centre",
                      size=df$label_size) +
    labs(title=title,
         caption=caption_text,
         subtitle=subtitle)+
    theme(legend.position = legend_spot,
          plot.caption = element_text(size=caption_size))+
    scale_fill_viridis_c(labels = comma)

  aest_fix_labs(p)
}
