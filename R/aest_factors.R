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
#' @seealso
#'  \code{\link[dplyr]{summarise}}
#' @rdname aest_factors
#' @export
#' @import dplyr
aest_factors <- function(df){
  num_distinct <- df%>%
    dplyr::summarize(across(.cols = everything(), .fns =n_distinct))
  not_many_distinct=(num_distinct/dim(df)[1])<.01 | num_distinct < 10
  character <- df%>%
    summarize_all(is.character)
  return(c(not_many_distinct | character))
}
