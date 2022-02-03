#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param var PARAM_DESCRIPTION
#' @param date_col PARAM_DESCRIPTION
#' @param date_format PARAM_DESCRIPTION
#' @param what_change PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{interval}}
#' @rdname parse_and_change
#' @export
#' @import lubridate
#' @importFrom dplyr near
parse_and_change <- function(df, var, date_col, date_format, what_change){
  df%>%
    mutate(date=case_when(date_format=="ymd"~ymd({{  date_col  }}, quiet = TRUE),
                          date_format=="ydm"~ydm({{  date_col  }}, quiet = TRUE),
                          date_format=="mdy"~mdy({{  date_col  }}, quiet = TRUE),
                          date_format=="myd"~myd({{  date_col  }}, quiet = TRUE),
                          date_format=="dmy"~dmy({{  date_col  }}, quiet = TRUE),
                          date_format=="dym"~dym({{  date_col  }}, quiet = TRUE),
                          date_format=="yq"~yq({{  date_col  }}, quiet = TRUE),
                          date_format=="ym"~ym({{  date_col  }}, quiet = TRUE),
                          date_format=="my"~my({{  date_col  }}, quiet = TRUE)
          ),
          month_diff=lubridate::interval(lag(date),date) %/% months(1),
          what_lag=case_when(near(mean(month_diff,na.rm=TRUE),1) & what_change=="annual" ~ 12,
                       near(mean(month_diff,na.rm=TRUE),12) & what_change=="annual" ~ 1,
                       near(mean(month_diff,na.rm=TRUE),3) & what_change=="annual" ~ 4,
                       what_change=="from_last" ~ 1,
                       TRUE ~ NA_real_),
          level={{  var  }},
          change=log(level)-log(lag(level, mean(what_lag,na.rm=TRUE))),
          description=case_when(what_change=="annual" ~ "annual",
                          what_change=="from_last" & mean(month_diff,na.rm=TRUE)==1 ~ "monthly",
                          what_change=="from_last" & mean(month_diff,na.rm=TRUE)==3 ~ "quarterly")
          )%>%
    na.omit()
}
