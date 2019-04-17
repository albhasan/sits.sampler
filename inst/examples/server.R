# server.R

map <- leaflet::leaflet()

ts.server <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
ts.coverages <- ts.server %>% wtss::listCoverages()
ts.cov_description <- lapply(ts.coverages, function(x){wtss::describeCoverage(ts.server, x)})

#ts.cov <- ""
ts.attributes <- ""
ts.resolution <- ""

ts.rgb <- ""
ts_data <- sits::sits_tibble()

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$mapui <- leaflet::renderLeaflet(
        map %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                      options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
            leaflet::setView(lng = input$nip_lon, lat = input$nip_lat, zoom = 17) %>%
            #flyTo(lng = input$nip_lon, lat = input$nip_lat, zoom = map$x$setView[[2]]) %>%
            addCircleMarkers(input$nip_lon, input$nip_lat, color = "#00ff00", radius = 10)
    )

    observeEvent(input$mapui_center, {
        event <- input$mapui_center
        output$debug <- shiny::renderText({
            sprintf(
                "x: %s y: %s time_steps: %s resolution: %s x: %s y: %s",
                input$nip_lon, input$nip_lat,
                length(unique(dataInput()$Date)),
                paste(unlist(ts.resolution), collapse = " "),
                class(event$lng), event$lat
            )
        })
        shiny::isolate({
            shiny::updateNumericInput(session, "nip_lon", value = event$lng)
            shiny::updateNumericInput(session, "nip_lat", value = event$lat)
        })

    })

    dataInput <- shiny::reactive({
        # input <- list()
        # input$nip_lon <- -74
        # input$nip_lat <- 4
        # input$dip_from <- "2015-01-01"
        # input$dip_to <- "2015-12-31"
        # input$nip_pixwin <- 3

        ts.cov <- ts.coverages[1] # TODO: get from GUI
        ts.attributes <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["attributes"]]$name
        ts.resolution <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["spatial_resolution"]]

        # get sample points
        s_points <- get_neighbors(lon = input$nip_lon, lat = input$nip_lat,
                                  win_size = input$nip_pixwin,
                                  pix_res = ts.resolution)
        # get time series
        ts_ls <- wtss::listTimeSeries(ts.server, coverages = ts.cov,
                                      attributes = ts.attributes,
                                      coordinates = s_points[c("lon", "lat")],
                                      start_date = input$dip_from,
                                      end_date = input$dip_to)

        # format the time series
        process_ts(ts_ls = ts_ls, s_points = s_points) %>%
            return()
    })

    output$plot_ts <- shiny::renderPlot({
        input$newplot
        dataInput <- dataInput()
        dataInput %>%
            dplyr::select(-c(lon, lat, x_pix, y_pix)) %>%
            tidyr::gather("key", "value", -c(id_pix, Date)) %>%
            ggplot2::ggplot(ggplot2::aes(x = Date, y = value, group = interaction(id_pix, key), colour = key)) +
            ggplot2::geom_line()
    })
    output$plot_img <- shiny::renderPlot({
        input$newplot
        dataInput <- dataInput()


        ts.cov <- ts.coverages[1] # TODO: get from GUI
        ts.attributes <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["attributes"]]$name
        #ts.resolution <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["spatial_resolution"]]



        ts.rgb <- ts.attributes[1:3] # TODO: get from GUI
        rb_dates <- ts2raster(dataInput)
        plot_rc <- rb_dates %>% length() %>% sqrt() %>% ceiling()
        graphics::layout(matrix(1:plot_rc^2, ncol = plot_rc, nrow = plot_rc, byrow = TRUE),
                         respect = TRUE)
        for (i in seq_along(rb_dates)) {
            raster::plotRGB(rb_dates[[i]], r = ts.rgb[1], g = ts.rgb[2],
                            b = ts.rgb[3], asp = 1, stretch = "hist", mai = rep(1, 4))
            box("figure")
        }
    })

    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        # TODO: validate data

        if (nrow(ts_data) > 0)
            sits:::.sits_test_tibble(ts_data)

        input$abt_add



        # if (input$man != "All") {
        #     data <- data[data$manufacturer == input$man,]
        #}
        # if (input$cyl != "All") {
        #     data <- data[data$cyl == input$cyl,]
        # }
        # if (input$trans != "All") {
        #     data <- data[data$trans == input$trans,]
        # }
        ts_data
    }))
}
