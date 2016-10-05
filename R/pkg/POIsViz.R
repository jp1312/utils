#' Plot the Point Of Interest on a leaflet map
#'
#' Shiny gadget to vizualize the ouput of the getPOIs function
#'
#' @param data: getPOIs() function output; Data table with the coordinates of points as well as a keyword
#'
#' @importFrom magrittr %>%
#' @export
POIsViz <- function(data)
{
  data = data.table::setDT(data)
  data = data[!is.na(lat),]
  if(!(sum(colnames(data) %in% c('lat','lng', 'vicinity', 'name')) == 4))
    stop('You must include a dataframe with the lat, lng, vicinity and name of the POIs.')
  ui <- miniUI::miniPage(miniUI::gadgetTitleBar("Point of Interest"),
                         miniUI::miniTabstripPanel(
                           miniUI::miniTabPanel("Map",
                                                icon = shiny::icon("map-o"),
                                                miniUI::miniContentPanel(padding = 0,
                                                                         leaflet::leafletOutput("map", height = "100%")),
                                                miniUI::miniButtonBlock(
                                                  shiny::actionButton("resetMarker", "Remove Marker")
                                                )),
                           miniUI::miniTabPanel("Selector", icon = shiny::icon("table"),
                                                miniUI::miniContentPanel(
                                                  shiny::fillCol(
                                                    shiny::h2("Add marker", align="center", color = 'red'),
                                                    shiny::fillRow(
                                                      shiny::textInput(inputId = "lat", label = "Latitude",
                                                                       value = "", width = NULL, placeholder = NULL),
                                                      shiny::textInput(inputId = "lng", label = "Longitude",
                                                                       value = "", width = NULL, placeholder = NULL)
                                                    ),
                                                    shiny::fillRow(
                                                      shiny::textInput(inputId = "rad", label = "Radius of interest (in meters)",
                                                                       value = "5000", width = NULL, placeholder = NULL),
                                                      shiny::selectizeInput(inputId = "type",
                                                                            label = "Type (Leave empty to select all)",
                                                                            choices = unique(as.character(data$keyword)),
                                                                            selected = NULL, multiple = TRUE, options = NULL)
                                                    )
                                                  )),
                                                miniUI::miniButtonBlock(
                                                  shiny::actionButton("subMarker", "Add Marker")
                                                )),
                           miniUI::miniTabPanel("Data", icon = shiny::icon("table"),
                                                miniUI::miniContentPanel(DT::dataTableOutput("table")))))


  server <- function(input, output, session)
  {

    map_data <- shiny::reactiveValues(data = data)
    observe({
      if (length(input$type) == 0)
      {
        map_data$data = data
      } else
      {
        map_data$data = data[keyword %in% input$type, ]
      }
    })

    output$map <- leaflet::renderLeaflet({
      tmp_data <- map_data$data

      col = colorRampPalette(c("blue", "red", "green"))(length(unique(as.character(tmp_data$keyword))))
      pal <- leaflet::colorFactor(col, domain = unique(as.character(tmp_data$keyword)))

      tmp_data$desc <- paste(sep = "<b>", tmp_data$name,"</b>",
                             "<br>",tmp_data$vicinity,
                             "<br>", tmp_data$keyword)

      leaflet::leaflet(data = tmp_data, height = "100%") %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(color = ~pal(keyword),
                                  radius = 2, popup = tmp_data$desc) %>%
        leaflet::addLegend("bottomright",
                           colors = col, labels = unique(as.character(tmp_data$keyword)),
                           title = "POIs Label")
    })

    addCirclesCustom <-  function(data, mapId, click_data, radius=5000){
      ## Get the click info like had been doing
      click <- click_data
      clat <- as.numeric(as.character(click$lat))
      clng <- as.numeric(as.character(click$lng))
      radius <- as.numeric(as.character(radius))

      if (is.na(radius)) stop('Your radius is not in the appropriate format.')
      if (is.na(clat) | is.na(clng)) stop('Your coordinates are not in the appropriate format.')

      diff <- circleBounds(clat = clat, clng = clng, radius = radius)
      bounds <- c(clng - diff[2]*1.2, clat - diff[1]*1.2, clng + diff[2]*1.2, clat + diff[1]*1.2)


      address  <-  try(ggmap::revgeocode(c(clng,clat)), silent = T)
      if(!class(address)=="try-error")
      {
        desc <- paste("<p>" ,"Address: ","<b>", address,"</b>",
                      "<br>","Address latitude: ", "<b>", round(clat, 2), "</b>",
                      "<br>","Address longitude: ", "<b>", round(clng, 2), "</b>",
                      "<br>", 'Radius: ', "<b>", radius,'m', "</b>","</p>", sep = '')
      } else
      {
        desc <- paste("<p>",'Coordinates',
                      "<br>","Center latitude: ", "<b>", round(clat, 2), "</b>",
                      "<br>","Center longitude: ", "<b>", round(clng, 2), "</b>",
                      "<br>", 'Radius: ', "<b>", radius,'m', "</b>", "</p>", sep = '')
      }
      ## Add the circle to the map proxy
      ## so you dont need to re-render the whole thing
      ## I also give the circles a group, 'circles', so you can
      ## then do something like hide all the circles with  hideGroup('circles')
      leaflet::leafletProxy(mapId = mapId, data = data) %>% # use the proxy to save computation
        leaflet::clearGroup(group =  'circles') %>%
        leaflet::addMarkers(lng=clng,
                            lat=clat,group = 'circles',
                            popup = desc) %>%
        leaflet::addCircles(lng=clng,
                            lat=clat, weight=1, group = 'circles',
                            radius=radius, color='black', fillColor='orange',
                            fillOpacity=0.5, opacity=1) %>%
        leaflet::fitBounds(lng1 = bounds[1], lat1 = bounds[2],
                           lng2 = bounds[3], lat2 = bounds[4])
    }
    ## Observe mouse clicks and add circles
    shiny::observeEvent(input$map_click,{
      addCirclesCustom(data = map_data$data, mapId = 'map',
                       click_data = input$map_click,
                       radius = ifelse(input$rad == '', 5000, input$rad))
    })

    shiny::observeEvent(input$resetMarker, {
      leaflet::leafletProxy(mapId = "map", data = map_data$data) %>%
        leaflet::clearGroup(group = 'circles') %>%
        leaflet::fitBounds(~min(lng), ~min(lat),
                           ~max(lng), ~max(lat))
    })

    shiny::observeEvent(input$subMarker, {
      # browser()
      addCirclesCustom(data = map_data$data, mapId = 'map',
                       click_data = data.frame(lat = input$lat, lng = input$lng),
                       radius = ifelse(input$rad == '', 5000, input$rad))
    })

    output$table <- DT::renderDataTable({
      DT::datatable(data, filter = 'top', options = list(
        pageLength = 5, autoWidth = TRUE
      ))

    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(TRUE)
    })
  }
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::paneViewer())
}
