#' @title Level and Change Plot.
#' @description Makes a two panel plot of the levels and changes of a non-negative variable
#' @param df a dataframe that contains the variable of interest, a column with a time index, and the file_name column.
#' @param x_var the column that contains the time index.
#' @param y_var The variable to plot on y axis
#' @param date_format the date format: see details.
#' @param what_change either "annual" or "from_last"
#' @param title Optional title to include.
#' @param scale_y fed into ggplot's scale_y_continuous(trans=scale_y). E.g. "log", "sqrt".
#' @param last_n Option to only use the last_n observations.
#' @param caption Over-rides the default caption (variable and file)
#' @param caption_size the font size for the caption, Default=5
#' @return a two panel plotly object.
#' @details Dataframe df MUST contain a column file_name, which is shown in the plot caption.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_level_and_change
#' @export
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @importFrom assertthat assert_that

aest_level_and_change <- function(df,
                                  x_var,
                                  y_var,
                                  date_format,
                                  what_change="annual",
                                  title=NULL,
                                  scale_y="identity",
                                  last_n=NULL,
                                  caption=NULL,
                                  caption_size=5){
  assert_that(is.data.frame(df))
  names_df <- names(df)
  min_value_var <-min(df[,deparse(substitute(y_var))])
  assert_that(deparse(substitute(x_var)) %in% names_df)
  assert_that(deparse(substitute(y_var)) %in% names_df)
  assert_that(min_value_var>=0)
  assert_that("file_name" %in% names_df)
  assert_that(date_format %in% c("ymd","ydm","myd","mdy","dmy","dym","ym","my","yq"))
  assert_that(what_change %in% c("annual","from_last"))
  assert_that(scale_y %in% c("identity","log","log2","log10","sqrt"))

  df <-parse_and_change(df,{{  y_var  }}, {{  x_var  }}, date_format, what_change)

  if(!is.null(last_n)){
    df <- df%>%
      tail(n=last_n)
  }
  var_name <- as.character(substitute(y_var))
  df
  plot_level_and_change(df, var_name, title, scale_y, caption, caption_size)
}
