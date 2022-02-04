#' @title Donut plot
#' @description takes a dataframe with a grouping variable and a measure and makes a donut.
#' @param df dataframe that contains
#' @param grp a grouping variable
#' @param vrbl a measure
#' @param center_text what text to put in middle, Default: NULL
#' @param center_text_size how large center font is, Default: 7
#' @param caption user provided caption, Default: NULL
#' @param caption_size font size for caption, Default: 5
#' @return a donut chart
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[scales]{label_percent}}
#' @rdname aest_donut
#' @export
#' @importFrom scales percent
#' @importFrom ggrepel geom_label_repel
#' @importFrom utils head
aest_donut <- function(df,
                       grp,
                       vrbl,
                       center_text=NULL,
                       center_text_size=7,
                       caption=NULL,
                       caption_size=5){
  df <- df%>%
    arrange(desc({{  vrbl  }}))%>%
    mutate(category=fct_reorder({{  grp  }}, {{  vrbl  }}),
           fraction={{  vrbl  }}/sum({{  vrbl  }}),
           ymax=cumsum(fraction),
           ymin=c(0, head(ymax, n=-1)),
           labelPosition =(ymax + ymin) / 2,
           label = paste0(category, "\n(", scales::percent(fraction, accuracy = 1),")"),
           label_colour=ifelse({{  vrbl  }} < mean({{  vrbl  }}),"white","black")
    )
  if(is.null(caption)){
    caption_text <-paste0('Variable name: "', deparse(substitute(vrbl)), '" File name: "', df$file_name[1],'"')
  }else{
    caption_text <- caption
  }
  ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=5, xmin=3, fill=category)) +
    geom_rect(colour="lightgrey") +
    geom_label_repel(aes(x=4.5, y=labelPosition, label=label),colour=df$label_colour,nudge_x = 1) +
    scale_fill_viridis_d()+
    xlim(c(0, 6)) +
    coord_polar(theta="y") +
    geom_text(x=0,
              y=0,
              aes(label = center_text),
              size=center_text_size)+
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = 'lightgrey', color = 'grey'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(size=caption_size))+
    labs(caption=caption_text)
}
