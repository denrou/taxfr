#' @export
#'
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dropdownButton(
      icon = icon("gear"),
      shinyWidgets::materialSwitch(ns("background"), label = "Background", value = FALSE)
    ),
    leaflet::leafletOutput(ns("map"), height = 800)
  )
}

#' @export
#'
mod_map <- function(input, output, session, map_data) {
  id_val <- reactiveVal(0)

  data_map <- reactive({
    req(id_val())
    df_name <- id_val()
    if (df_name == 0 || is.null(input$map_shape_click$id)) {
      df <- map_data[["dept"]]
      list(
        layer_id = df$department_code,
        label = glue("{df$nom_dept} ({round(df$gini, 2)})"),
        data = df
      )
    } else {
      df <- map_data[["insee"]] %>%
        filter(code_dept == input$map_shape_click$id)
      list(
        layer_id = df$code_insee,
        label = glue("{df$nom_com} ({round(df$gini, 2)})"),
        data = df
      )
    }
  })

  output$map <- leaflet::renderLeaflet({
    req(data_map())
    pal <- colorQuantile("BrBG", data_map()[["data"]]$gini, reverse = TRUE, n = 3)
    map <- data_map()[["data"]] %>%
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addPolygons(color = "black", opacity = 0.4, fillColor = ~pal(gini), weight = 1, label = data_map()[["label"]], layerId = data_map()[["layer_id"]]) %>%
      addLegend(pal = pal, values = ~gini, labFormat = function(x, y, type) c("Low inequalities", "Medium inequalities", "High inequalities", "Unknown"))
  })

  observe({
    req(input$map_shape_click)
    print(input$map_shape_click)
    isolate(id_val((id_val() + 1) %% length(map_data)))
  })

  observeEvent(input$background, {
    proxy <- leafletProxy("map", session = session)
    proxy %>% clearTiles()
    if (input$background) {
      proxy %>%
        addProviderTiles("CartoDB.Positron")
    }
  })
}
