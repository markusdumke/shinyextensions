#' Navbar
#'
#' Like [shiny::navbarPage()] but with more flexibility. Elements can be aligned
#' to the right, buttons, text and other inputs included.
#'
#' Specify the same `id` in a `navTab` and a `divTabPane` match
#' navigation and content.
#'
#' @param ... HTML elements to include.
#' @param id Id of element. Can be accessed via `input$id` in server.
#' @param title Title to show.
#' @param pull Alignment of element, left or right?
#' @param active Set to `TRUE` to add `active` class to tab.
#' @param position Determines whether the navbar should be displayed at the top
#'                 of the page with normal scrolling behavior ("static-top"),
#'                 pinned at the top ("fixed-top"),
#'                 or pinned at the bottom ("fixed-bottom").
#' @inheritParams shiny::navbarPage
#'
#' @rdname navbar
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'
#'  ui <- bootstrapPage(
#'    tags$head(tags$style(".navbar-brand {padding: 0 0 !important}")),
#'    navbar(position = "static-top",
#'           navbarHeader(navbarBrand("Brand")),
#'           ## these elements appear on the right
#'           navbarHeader(navbarButton("Logout"),
#'                        navbarCollapseButton(),
#'                        pull = "right"),
#'           ## the classical navbar with a text form
#'           navbarCollapse(navbarNav(navTab("Tab1"),
#'                                    navTab("Tab2"),
#'                                    navTab("Tab3")),
#'                          navbarForm(textInputNoLabel("text")))
#'    ),
#'    div(
#'      class = "container-fluid",
#'      divTabContent(
#'        divTabPane("Tab1", h2("This is tab1")),
#'        divTabPane("Tab2", h2("This is tab2")),
#'        divTabPane("Tab3", h2("This is tab3"))
#'      )
#'    )
#'  )
#'
#'  server <- function(input, output, session) {
#'    session$onSessionEnded(stopApp)
#'
#'    observeEvent(input$navbar, {
#'      print(input$navbar)
#'    })
#'
#'    observeEvent(input$Logout, {
#'      print("Logged out")
#'    })
#'
#'    # clicked on brand image
#'    observeEvent(input$Brand, {
#'      print("Brand")
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'
#' }

navbar <- function(...,
                   position = c("static-top", "fixed-top", "static-bottom", "static-top"),
                   inverse = FALSE,
                   fluid = TRUE) {

  position <- match.arg(position)
  if (fluid) {
    class <- "fluid"
  }

  navbar.content <- tags$div(class = paste0("container ", class),
                             ...)

  style <- ""
  if (position == "fixed-top") {
    style <- "body {padding-top: 70px;}"
  }

  if (inverse) {
    inverse <- " navbar-inverse"
  } else {
    inverse <- NULL
  }

  tagList(
    tags$head(tags$style(style)),
    tags$nav(class = paste0("navbar navbar-default navbar-", position, inverse),
             role = "navigation",
             navbar.content)
  )
}


#' @rdname navbar
#' @export

navbarHeader <- function(..., pull = c("left", "right")) {
  pull <- match.arg(pull)
  tags$div(class = paste0("navbar-header pull-", pull),
           ...)
}


#' @rdname navbar
#' @export

navbarBrand <- function(id, title = id) {
  tags$a(class = "navbar-brand action-button",
         href = "#",
         id = id,
         title)
}


#' @rdname navbar
#' @export

navbarForm <- function(..., pull = c("left", "right")) {
  pull <- match.arg(pull)
  tags$form(class = paste0("navbar-form navbar-", pull), ...)
}


#' @rdname navbar
#' @export

nav <- function(..., pull = c("left", "right")) {
  pull <- match.arg(pull)
  tags$ul(class = paste0("nav pull-", pull), ...)
}


#' @rdname navbar
#' @export

navbarText <- function(title, pull = c("left", "right")) {
  pull <- match.arg(pull)
  tags$p(class = paste0("navbar-text pull-", pull), title)
}


#' @rdname navbar
#' @export

navbarButton <- function(id, title = id) {
  tags$button(type = "button",
              id = id,
              class = "btn btn-default navbar-btn action-button",
              style = "margin-right: 10px;",
              title)
}

#' @rdname navbar
#' @export

navbarNav <- function(..., pull = c("left", "right")) {
  pull <- match.arg(pull)
  tags$ul(class = paste0("nav navbar-nav shiny-tab-input pull-", pull),
          id = "navbar",
          ...)
}


#' @rdname navbar
#' @export

navTab <- function(id, title = id, ..., active = FALSE) {
  class <- NULL
  if (active) {
    class <- "active"
  }
  tags$li(tags$a(href = paste0("#", id), title, `data-toggle` = "tab", `data-value` = id),
          class = class)
}


#' @rdname navbar
#' @export

navbarCollapse <- function(...) {
  tagList(
    tags$div(class = "visible-xs-block clearfix"),
    tags$div(class = "collapse navbar-collapse navbar-left", ...)
  )
}


#' @rdname navbar
#' @export

navbarCollapseButton <- function() {
  tags$button(type = "button",
              class = "navbar-toggle collapsed",
              `data-toggle` = "collapse",
              `data-target` = ".navbar-collapse",
              span(class = "sr-only", "Toggle navigation"),
              span(class = "icon-bar"),
              span(class = "icon-bar"),
              span(class = "icon-bar"))
}
