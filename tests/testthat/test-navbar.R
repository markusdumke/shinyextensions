
if (interactive()) {
  library(shiny)
  library(leaflet)

  ui <- bootstrapPage(
    navbar(
      navbarHeader(navbarBrand("Title")),
      navbarHeader(nav(navbarButton("Logout")),
                   # navTab("Tab3", button = TRUE),
                   navbarCollapseButton(),
                   pull = "right"),
      div(class = "visible-xs-block clearfix"),
      navbarCollapse(navbarNav(navTab("Tab1"),
                               navTab("Tab2"),
                               navTab("Tab3")))
    ),
    div(
      class = "container-fluid",
      div(
        class = "tab-content", `data-tabsetid` = "1",
        div(
          class = "tab-pane", `data-value` = "Tab1", id = "tab-1-1",
          leafletOutput("map")
        ),
        div(
          class = "tab-pane active", `data-value` = "Title", id = "Title",
          h1("Homepage")
        ),
        div(
          class = "tab-pane", `data-value` = "Tab2", id = "tab-1-2",
          actionButton("a", "a")
        ),
        div(
          class = "tab-pane", `data-value` = "Tab3", id = "tab-1-3",
          sliderInput("slider", "slider", 0, 10, 1)
        ),
        div(
          class = "tab-pane", `data-value` = "Logout", id = "Logout",
          h1("Logged out")
        )
      )
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$Logout, {
      print("Clicked on button")
    })

    output$map <- renderLeaflet({
      leaflet() %>% addTiles() %>% setView(11, 48, 8)
    })

  }

  shinyApp(ui, server)
}

