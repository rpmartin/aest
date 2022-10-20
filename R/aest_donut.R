#' @title Donut plot
#' @description takes a dataframe with a grouping variable and a measure and makes a donut.
#' @param df dataframe that contains
#' @param y_var The measure being plotted
#' @param group_var a grouping variable
#' @param center_text what text to put in middle, Default: NULL
#' @param center_text_size how large center font is, Default: 7
#' @param caption user provided caption, Default: NULL
#' @param lab_size font size for labels, Default: 5
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
#' @importFrom assertthat assert_that
aest_donut <- function(df,
                       y_var,
                       group_var,
                       center_text=NULL,
                       center_text_size=7,
                       caption=NULL,
                       lab_size=5){
  assert_that(is.data.frame(df))
  names_df <- names(df)
  assert_that(deparse(substitute(y_var)) %in% names_df)
  assert_that(deparse(substitute(group_var)) %in% names_df)
#  assert_that("file_name" %in% names_df)
  assert_that(is.numeric(df[[deparse(substitute(y_var))]]))
  df <- df%>%
    arrange(desc({{  y_var  }}))%>%
    mutate(category=fct_reorder({{  group_var  }}, {{  y_var  }}),
           fraction={{  y_var  }}/sum({{  y_var  }}),
           ymax=cumsum(fraction),
           ymin=c(0, head(ymax, n=-1)),
           labelPosition =(ymax + ymin) / 2,
           label = paste0(category, "\n", scales::comma({{  y_var  }}), " (", scales::percent(fraction, accuracy = 1),")"),
           label_colour=ifelse({{  y_var  }} < mean({{  y_var  }}),"white","black")
    )
  # if(is.null(caption)){
  #   caption_text <-paste0('Variable name: "', deparse(substitute(y_var)), '" File name: "', df$file_name[1],'"')
  # }else{
  #   caption_text <- caption
  # }
  p <- ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=5, xmin=3, fill=category)) +
    geom_rect(colour="lightgrey") +
    geom_label_repel(aes(x=4.5, y=labelPosition, label=label),colour=df$label_colour, nudge_x = 1, size = lab_size) +
    scale_fill_viridis_d()+
    xlim(c(0, 6)) +
    coord_polar(theta="y") +
    theme_void() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())#,
 #         plot.caption = element_text(size=caption_size))+
#    labs(caption=caption_text)
  if(is.null(center_text)){
    p
  }else{
    p+
      geom_text(x=0,
            y=0,
            aes(label = center_text),
            size=center_text_size)
  }
}
