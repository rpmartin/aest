#' @title wrapper function that calls one of vroom, read_csv or read_excel
#' @description Reads in a spreadsheet,, cleans the column names and appends a column containing the file name.
#' @param quoted_path the (quoted) path to the file (within the project directory) e.g. "raw-data"
#' @param quoted_file the (quoted) name of the file
#' @param speed determines whether vroom or read_csv is used: read_csv is more robust, but slow, Default: 'slow'
#' @param ... additional parameters to pass.
#' @return a dataframe
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname aest_read_spreadsheet
#' @export
#' @importFrom readxl read_excel
#' @importFrom vroom vroom
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @importFrom stringr str_split
#' @importFrom here here
#' @import dplyr
#' @import assertthat

aest_read_spreadsheet <- function(quoted_path, quoted_file, speed="slow", ...){
  thing <- here(quoted_path, quoted_file)
  assert_that(file.exists(thing))
  extension <- dplyr::last(str_split(quoted_file, pattern="\\.",simplify = TRUE))
  delimited <-  extension=="csv" |
    extension == "CSV"
  excel_file <- extension=="xls" |
    extension == "xlsx" |
    extension == "xlsm" |
    extension=="XLS" |
    extension == "XLSX" |
    extension == "XLSM"
  df <-if(excel_file){
    read_excel(thing, ...)
  }else if(delimited & speed=="fast"){
    vroom(thing, ...)
  }else{
    tryCatch(read_csv(thing, ...),
             error = function(e){
               print("There was some problem reading this file... Is it a spreadsheet?")
             }
    )
  }
  df <- df%>%
    clean_names()%>%
    mutate(file_name=quoted_file)
}
