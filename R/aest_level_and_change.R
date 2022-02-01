#' @title Level and Change Plot.
#' @description Makes a two panel plot of the levels and changes of a non-negative variable
#' @param df a dataframe that contains the variable of interest, a column with a time index, and the file_name column.
#' @param var the column that contains the variable of interest.
#' @param date_col the column that contains the time index.
#' @param date_format the date format: see details.
#' @param what_change either "annual" or "from_last"
#' @param title Optional title to include.
#' @param scale_y fed into ggplot's scale_y_continuous(trans=scale_y). E.g. "log", "sqrt".
#' @param last_n Option to only use the last_n observations.
#' @param caption Over-rides the default caption (variable and file)
#' @param text_size font size for everything else
#' @param caption_size font size for caption
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
                                  var,
                                  date_col,
                                  date_format,
                                  what_change="annual",
                                  title=NULL,
                                  scale_y="identity",
                                  last_n=NULL,
                                  caption=NULL,
                                  text_size=20,
                                  caption_size=12){
  assert_that(is.data.frame(df))
  names_df <- names(df)
  min_value_var <-min(df[,deparse(substitute(var))])
  assert_that(deparse(substitute(date_col)) %in% names_df)
  assert_that(deparse(substitute(var)) %in% names_df)
  assert_that(min_value_var>=0)
  assert_that("file_name" %in% names_df)
  assert_that(date_format %in% c("ymd","ydm","myd","mdy","dmy","dym","ym","my","yq"))
  assert_that(what_change %in% c("annual","from_last"))
  assert_that(scale_y %in% c("identity","log","log2","log10","sqrt"))

  df <-parse_and_change(df,{{  var  }}, {{  date_col  }}, date_format, what_change)

  if(!is.null(last_n)){
    df <- df%>%
      tail(n=last_n)
  }
  var_name <- as.character(substitute(var))
  df
  plot_level_and_change(df, var_name, title, scale_y, caption, text_size, caption_size)
}
