ui <- fluidPage(
    # Application title
    titlePanel("Sample time series for SITS"),

    shiny::sidebarLayout(
        shiny::sidebarPanel(
            h4("Setup"),
            numericInput("nip_pixwin", h5("Window size"), min = 1, max = 15, value = 3, step = 2),
            h4("Bounding box"),
            sliderInput("sip_lon", h5("Longitude"), min = -16, max = 6,   value = -10, step = 1),
            sliderInput("sip_lat", h5("Latitude"),  min = -74, max = -45, value = -60, step = 1),
            width = 3
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Time series",
                    shiny::fluidRow(
                        shiny::column(4, leaflet::leafletOutput("mapui", width = "100%", height = 250)),
                        shiny::column(8, plotOutput("plot_ts", width = "100%", height = 250))
                    ),
                    shiny::fluidRow(
                        shiny::column(2, numericInput("nip_lon", h6("Longitude"), value = -60)),
                        shiny::column(2, numericInput("nip_lat", h6("Latitude"),  value = -16)),
                        shiny::column(1, dateInput("dip_from", h6("Start"), value = "2015-01-01")),
                        shiny::column(1, dateInput("dip_to", h6("End"), value = "2015-12-31")),
                        shiny::column(1, textInput("tip_lab", h6("Label"), value = NA)),
                        shiny::column(1, textInput("tip_cov", h6("Coverage"), value = NA)),
                        shiny::column(1, actionButton("abt_add", "add"))
                    ),
                    DT::dataTableOutput("table"),
                    shiny::textOutput("debug")
                ),
                tabPanel(
                    "Images",
                    shiny::plotOutput("plot_img", width = "100%")
                )
            )
        )
    )
)
