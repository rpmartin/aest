#' @title Gets a simple features dataframe for BCs economic regions (for mapping)
#' @return sf tibble
#' @rdname aest_bc_reg_sf
#' @export
#' @import bcmaps
#' @import sf
#' @import janitor
#' @import dplyr
#' @import stringr

aest_bc_reg_sf <- function(){
  bcmaps::census_economic() %>%
    sf::st_transform("+proj=longlat +datum=WGS84") %>%
    janitor::clean_names() %>%
    select(region=economic_region_name, geometry) %>%
    mutate(
      region = stringr::word(region, 1, sep = "/"),
      region = janitor::make_clean_names(region),
      region = case_when(
        region == "nechako" ~ "north_coast_&_nechako",
        region == "north_coast" ~ "north_coast_&_nechako",
        TRUE ~ region),
      region = stringr::str_replace_all(region, "vancouver_island_and_coast", "vancouver_island_coast"),
      region = stringr::str_replace_all(region, "lower_mainland_southwest", "mainland_south_west"),
      region = stringr::str_replace_all(region, "northeast", "north_east")
    )
}
