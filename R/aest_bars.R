#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_bars
#' @export
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics hist
#' @importFrom graphics par
aest_bars <- function(df){
  likely_factors <- aest_factors(df)
  fact_df <-df[, likely_factors]
  par(mar=c(5.1, 20, 4.1, 2.1))
  for (i in seq_along(fact_df)) {
    fact_df[[i]]%>%
      table()%>%
      barplot(horiz=TRUE,
              las=1,
              cex.names=.75,
              xlab="frequency",
              main=colnames(fact_df[i]))
  }
}
