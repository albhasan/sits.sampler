library(sits.sampler)

library(devtools)
devtools::load_all()

# Run the application
shinyApp(ui = ui, server = server)
