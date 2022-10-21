#' @title Make a choropleth map of BC economic regions
#' @param tbbl a tibble that contains (at least) the following 3 columns:
#' @param region the column of the tbbl that contains the economic regions: these MUST be camel_case i.e. mainland_south_west, vancouver_island_coast, north_coast_&_nechako, cariboo, kootenay, north_east, thompson_okanagan
#' @param thingy the column of the tbbl that contains the name of what is being plotted.
#' @param value the column of the tbble that contains the value of what is being plotted.
#' @rdname aest_choropleth
#' @import dplyr
#' @import leaflet
#' @import stringr
#' @import bcmaps
#' @import sf
#' @import htmltools
#' @export

aest_choropleth <- function(tbbl, region, thingy, value) {
  tbbl <- tbbl%>%
    rename(region =  {{  region  }},
           thingy = {{  thingy  }},
           value = {{  value  }}
    )%>%
    ungroup()

  shape <- aest::aest_bc_reg_sf()

  tbbl <- shape%>%
    left_join(tbbl, multiple = "all")

  variable_plotted <- tbbl$thingy[1]

  pal <- colorNumeric("viridis", domain = tbbl$value)
  pal_rev <- colorNumeric("viridis", domain = tbbl$value, reverse = TRUE)

  mytext <- paste(
    "Region: ", str_to_title(str_replace_all(tbbl$region,"_"," ")),"<br/>",
    str_to_title(str_replace_all(variable_plotted,"_"," ")), ": ", scales::percent(tbbl$value, accuracy = .1), "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  # browser()
  leaflet(tbbl,
          options = leafletOptions(
            attributionControl = FALSE
          )
  ) %>%
    setView(lng = -125, lat = 55, zoom = 5) %>%
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
