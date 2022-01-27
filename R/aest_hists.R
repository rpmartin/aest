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
#' @rdname aest_hists
#' @export
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics hist
#' @importFrom graphics par
aest_hists <- function(df){
  likely_factors <- aest_factors(df)
  num_df <- df[, !likely_factors]
  for (i in seq_along(num_df)) {
    marks <- seq(min(num_df[[i]],na.rm=TRUE),
                 max(num_df[[i]],na.rm=TRUE),
                 length.out=5)
    hist(num_df[[i]],
         main=colnames(num_df)[i],
         xlab="value",
         xaxt="n")
    axis(1,
         at=marks,
         labels=format(marks, scientific = FALSE, trim = TRUE, big.mark = ','))
  }
}
