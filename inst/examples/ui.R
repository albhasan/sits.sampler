ui <- fluidPage(

    # Application title
    titlePanel("Validate time series samples"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Query"),
            numericInput("nip_lon", h5("Longitude"), value = -60),
            numericInput("nip_lat", h5("Latitude"), value = -16),
            dateInput("dip_from", h5("From date"), value = "2015-01-01"),
            dateInput("dip_to", h5("To date"), value = "2015-12-31"),
            actionButton("abt_go", "go"),
            h4("Configuration"),
            numericInput("nip_pixwin", h5("Window size"), min = 1, max = 31, value = 1, step = 2),
            h4("Bounding box"),
            sliderInput("sip_lon", h5("Longitude"), min = -16, max = 6,   value = -10, step = 1),
            sliderInput("sip_lat", h5("Latitude"),  min = -74, max = -45, value = -60, step = 1),
            actionButton("abt_rpoint", "Random point")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            #selectInput("sip_map_provider", h5("Map provider"), choices = list("Esri.WorldImagery" = 1, "NASAGIBS.ModisTerraTrueColorCR" = 2, "OpenTopoMap" = 3), selected = 1),
            textOutput("debug"),
            leaflet::leafletOutput("llt_map", width = "50%", height = 150),
            plotOutput("plot_ts", width = "50%", height = 150),







            fluidRow(
                column(4,
                       selectInput("man",
                                   "Label:",
                                   c("All",
                                     unique(as.character(mpg$manufacturer))))
                ),
                column(4,
                       selectInput("trans",
                                   "Transmission:",
                                   c("All",
                                     unique(as.character(mpg$trans))))
                ),
                column(4,
                       selectInput("cyl",
                                   "Cylinders:",
                                   c("All",
                                     unique(as.character(mpg$cyl))))
                )
            ),
            # Create a new row for the table.
            DT::dataTableOutput("table")






        )
    )
)
