#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gg PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{labs}}
#'  \code{\link[stringr]{case}},\code{\link[stringr]{str_replace}}
#' @rdname aest_fix_labs
#' @export
#' @importFrom ggplot2 labs
#' @importFrom stringr str_to_title str_replace_all
aest_fix_labs <- function(gg){
  gg <- gg+
    ggplot2::labs(title=stringr::str_to_title(stringr::str_replace_all(gg$labels$title, "_", " ")),
         x=stringr::str_to_title(stringr::str_replace_all(gg$labels$x, "_", " ")),
         y=stringr::str_to_title(stringr::str_replace_all(gg$labels$y, "_", " ")),
         colour=stringr::str_to_title(stringr::str_replace_all(gg$labels$colour, "_", " ")),
         fill=stringr::str_to_title(stringr::str_replace_all(gg$labels$fill, "_", " ")),
         edge_colour=stringr::str_to_title(stringr::str_replace_all(gg$labels$edge_colour, "_", " ")))
  return(gg)
}
