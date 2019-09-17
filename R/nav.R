# https://getbootstrap.com/docs/3.4/components/#nav-tabs

# To implement: Dropdowns

nav <- function(...,
                id = NULL,
                selected = NULL,
                type = c("tabs", "pills"),
                stacked = FALSE,
                justified = FALSE) {

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)
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
