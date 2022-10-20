#' @title Make a choropleth map of BC economic regions
#' @param tbbl a tibble that contains (at least) measure and region
#' @param measure the column of the tbbl that contains the measure you want to plot
#' @param region the column of the tbbl that contains the economic regions: these MUST be camel_case i.e. "vancouver_island_and_coast","lower_mainland_southwest","thompson_okanagan","kootenay","cariboo","north_coast_&_nechako","northeast"
#' @rdname aest_choropleth
#' @import dplyr
#' @import leaflet
#' @import stringr
#' @import bcmaps
#' @import sf
#' @export

aest_choropleth <- function(tbbl, measure, region) {
  variable_plotted <- deparse(substitute(measure))
  bc <- bcmaps::census_economic() %>%
    sf::st_transform("+proj=longlat +datum=WGS84") %>%
    janitor::clean_names() %>%
    select(economic_region_name, geometry) %>%
    mutate(
      name = stringr::word(economic_region_name, 1, sep = "/"),
      name = janitor::make_clean_names(name),
      name = case_when(
        name == "nechako" ~ "north_coast_&_nechako",
        name == "north_coast" ~ "north_coast_&_nechako",
        TRUE ~ name
      )
    ) %>%
    select(-economic_region_name)

  tbbl <- tbbl%>%
    rename(value = {{  measure  }},
           name =  {{  region  }})
  bc <- left_join(bc, tbbl)

  pal <- colorNumeric("viridis", domain = bc$value)
  pal_rev <- colorNumeric("viridis", domain = bc$value, reverse = TRUE)

  mytext <- paste(
    "Region: ", str_to_title(str_replace_all(bc$name,"_"," ")),"<br/>",
    str_to_title(str_replace_all(variable_plotted,"_"," ")), ": ", scales::comma(bc$value, accuracy = .1), "<br/>",
     sep="") %>%
    lapply(htmltools::HTML)


  leaflet(bc,
          options = leafletOptions(
            attributionControl = FALSE
          )
  ) %>%
    setView(lng = -125, lat = 55, zoom = 4) %>%
    addProviderTiles("Esri.NatGeoWorldMap") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels") %>%
    addPolygons(
      fillColor = ~ pal(value),
      color = "black",
      label=mytext,
      fillOpacity = .3,
      weight = 1
    ) %>%
    addLegend("topright",
              pal = pal_rev,
              values = ~value,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
              title = str_to_title(str_replace_all(variable_plotted,"_"," "))
    )
}
