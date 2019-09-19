#' Tabset
#'
#' Like [shiny::tabsetPanel] but with additional options, e.g. `justified`.
#'
#' @inheritParams navbar
#' @param selected Name of the tab which is selected at start.
#' @param type One of "tabs" or "pills".
#' @param stacked Stacked tabset?
#' @param justified If `TRUE` tabs will use the full width.
#'
#' @import shiny
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- bootstrapPage(
#'    tabset(tabPanel("Tab1", "You selected Tab1"),
#'           tabPanel("Tab2", "You selected Tab2"),
#'           justified = TRUE),
#'
#'    tabset(tabPanel("Tab1", "You selected Tab1"),
#'           tabPanel("Tab2", "You selected Tab2"),
#'           type = "pills")
#'   )
#'
#'   server <- function(input, output, session) {}
#'   shinyApp(ui, server)
#' }

tabset <- function(...,
                id = NULL,
                selected = NULL,
                type = c("tabs", "pills"),
                stacked = FALSE,
                justified = FALSE) {

  if (!is.null(id))
    selected <- shiny::restoreInput(id = id, default = selected)
  tabs <- list(...)
  type <- match.arg(type)

  navclasses <- paste0("nav nav-", type)

  if (stacked) {
    navclasses <- paste(navclasses, "nav-stacked")
  }

  if (justified) {
    navclasses <- paste(navclasses, "nav-justified")
  }

  tabset <- shiny:::buildTabset(tabs, navclasses, NULL, id, selected)
  tags$div(class = "tabbable", tabset$navList, tabset$content)
}
