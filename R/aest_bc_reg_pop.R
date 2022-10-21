#' @title Gets the most recent measure of population for BCs economic geographic_areas
#' @rdname aest_bc_reg_pop
#' @return tibble
#' @import cansim
#' @import janitor
#' @import stringr
#' @import dplyr
#' @export

aest_bc_reg_pop <- function(){
  cansim::get_cansim("17-10-0137-01")%>%
    janitor::clean_names()%>%
    filter(grepl('British Columbia', geo),
           ref_date==max(ref_date),
           sex=="Both sexes",
           age_group=="All ages")%>%
    select(geographic_area=geo, value)%>%
    mutate(geographic_area=word(geographic_area, 1, sep = ","),
           geographic_area = janitor::make_clean_names(geographic_area),
           geographic_area = case_when(
             geographic_area == "nechako" ~ "north_coast_&_nechako",
             geographic_area == "north_coast" ~ "north_coast_&_nechako",
             TRUE ~ geographic_area),
           geographic_area = str_replace_all(geographic_area, "vancouver_island_and_coast", "vancouver_island_coast"),
           geographic_area = str_replace_all(geographic_area, "lower_mainland_southwest", "mainland_south_west"),
           geographic_area = str_replace_all(geographic_area, "northeast", "north_east"))%>%
    group_by(geographic_area)%>%
    summarize(value=sum(value))%>%
    mutate(name="population")
}
