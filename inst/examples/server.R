# server.R

map <- leaflet::leaflet()

ts.server <- wtss::WTSS("http://www.esensing.dpi.inpe.br/wtss/")
ts.coverages <- ts.server %>% wtss::listCoverages()
ts.cov_description <- lapply(ts.coverages, function(x){wtss::describeCoverage(ts.server, x)})
ts.rgb <- ""

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$llt_map <- leaflet::renderLeaflet({
        map %>%
            leaflet::addProviderTiles(providers$Esri.WorldImagery,
                                      options = providerTileOptions(noWrap = TRUE)) %>%
            leaflet::setView(lng = input$nip_lon, lat = input$nip_lat, zoom = 17)
    })

    output$debug <- shiny::renderText({
        sprintf("x: %s y: %s time_steps: %s", input$nip_lon, input$nip_lat, length(unique(dataInput()$Date)))
        #dataInput() %>% dplyr::select(mir,blue,nir,red,evi,ndvi) %>% lapply(range) %>% unlist() %>% print()
    })


    dataInput <- shiny::reactive({
        # input <- list()
        # input$nip_lon <- -74
        # input$nip_lat <- 4
        # input$dip_from <- "2015-01-01"
        # input$dip_to <- "2015-12-31"
        # input$nip_pixwin <- 1

        ts.cov <- ts.coverages[1] # TODO: get from GUI
        ts.attributes <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["attributes"]]$name
        ts.resolution <- ts.cov_description[[match(ts.cov,ts.coverages)]][[1]][["spatial_resolution"]]
        ts.rgb <- ts.attributes[1:3] # TODO: get from GUI
        ts.rgb <- c("nir", "red", "blue")

        # build list of neighbor points
        lon_vec <- seq(from = input$nip_lon - floor(abs(input$nip_pixwin/2)) * ts.resolution$x,
                       by = ts.resolution$x, length.out = input$nip_pixwin)
        lat_vec <- seq(from = input$nip_lat - floor(abs(input$nip_pixwin/2)) * ts.resolution$y,
                       by = ts.resolution$y, length.out = input$nip_pixwin)
        points <- expand.grid(lon_vec, lat_vec, stringsAsFactors = FALSE) %>%
            dplyr::bind_cols(expand.grid(1:input$nip_pixwin, 1:input$nip_pixwin,
                                         stringsAsFactors = FALSE)) %>%
            within(id_rel <- paste(Var11, Var21, sep = "_"))
        names(points) <- c("lon", "lat", "x_pix", "y_pix", "id_pix")

        # get time series
        ts_ls <- wtss::listTimeSeries(ts.server, coverages = ts.cov,
                                      attributes = ts.attributes,
                                      coordinates = points[c("lon", "lat")],
                                      start_date = input$dip_from,
                                      end_date = input$dip_to)
        names(ts_ls) <- points$id_pix

        lapply(names(ts_ls), function(x){
            ts_ls[[x]] %>% .[[1]] %>% .[["attributes"]] %>%
                as.data.frame(stringsAsFactors = FALSE) %>%
                dplyr::mutate(Date = as.Date(rownames(.)), id_pix = x) %>%
                return()
        }) %>%
            dplyr::bind_rows() %>%
            dplyr::left_join(points, by = "id_pix") %>%
            dplyr::select(id_pix, lon, lat, x_pix, y_pix, Date, everything()) %>%
            return()
    })

    output$plot_ts <- shiny::renderPlot({
        input$newplot
        dataInput <- dataInput()
        dataInput %>%
            dplyr::select(-c(id_pix, lon, lat, x_pix, y_pix)) %>%
            tidyr::gather("key", "value", -Date) %>%
            ggplot2::ggplot(ggplot2::aes(x = Date, y = value, group = key, colour = key)) +
            ggplot2::geom_line()
        # # time series of RasterBrick
        # rb_dates <- unique(dataInput$Date)
        # raster_ts <- lapply(rb_dates, function(x){
        #     dataInput %>%
        #         dplyr::filter(Date == x) %>%
        #         dplyr::select(-c(id_pix, lon, lat, Date)) %>%
        #         raster::rasterFromXYZ() %>%
        #         return()
        # })
        # names(raster_ts) <- rb_dates

        #plot_rc <- rb_dates %>% length() %>% sqrt() %>% ceiling()
        #graphics::layout(matrix(1:plot_rc^2, ncol = plot_rc, nrow = plot_rc, byrow = TRUE),
        #                 respect = TRUE)
        #for (i in seq_along(rb_dates)) {
        #    raster::plotRGB(raster_ts[[i]], r = ts.rgb[1], g = ts.rgb[2],
        #                    b = ts.rgb[3], asp = 1, stretch = "hist", mai = rep(1, 4))
        #    box("figure")
        #}
    })

    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- sits::sits_tibble()
        if (input$man != "All") {
            data <- data[data$manufacturer == input$man,]
        }
        if (input$cyl != "All") {
            data <- data[data$cyl == input$cyl,]
        }
        if (input$trans != "All") {
            data <- data[data$trans == input$trans,]
        }
        data
    }))
}
