
if (interactive()) {
  library(shiny)

  ui <- bootstrapPage(
    nav(tabPanel("Tab1", "You selected Tab1"),
        tabPanel("Tab2", "You selected Tab2"),
        justified = TRUE),

    nav(tabPanel("Tab1", "You selected Tab1"),
        tabPanel("Tab2", "You selected Tab2"),
        type = "pills")
  )

  server <- function(input, output, session) {}

  shinyApp(ui, server)
}
