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
#' @rdname aest_summary
#' @export
#' @import dplyr
#' @importFrom DT datatable
aest_summary <- function(df){
  `Number of Observations` <- dim(df)[1]
  `Number of Distinct Values`<- df%>%
    dplyr::summarize(across(.cols = everything(), .fns =n_distinct))%>%
    t()%>%
    as.vector()
  `NAs (%)` <- round(100*colSums(is.na(df))/dim(df)[1])
  `Dataframe Column` <- names(df)
  tibble(`Dataframe Column`,`Number of Observations`,`Number of Distinct Values`,`NAs (%)`)%>%
    datatable(rownames=FALSE)
}
