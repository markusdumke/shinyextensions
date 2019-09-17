
navbarHeader <- function(..., pull = "left") {
  div(class = paste0("navbar-header pull-", pull),
      ...)
}

navbarBrand <- function(title) {
  span(class = "navbar-brand", title)
}

navbarCollapseButton <- function() {
  tags$button(type = "button",
              class = "navbar-toggle collapsed",
              `data-toggle` = "collapse",
              `data-target` = ".navbar-collapse", #paste0("#", navId),
              span(class = "sr-only", "Toggle navigation"),
              span(class = "icon-bar"),
              span(class = "icon-bar"),
              span(class = "icon-bar"))
}

nav <- function(..., pull = "left") {
  tags$ul(class = paste0("nav pull-", pull), ...)
}

navbarText <- function(text, pull = "left") {
  tags$li(class = paste0("navbar-text pull-", pull), text)
}

navbarButton <- function(id, label = id) {
  tags$button(type = "button",
              id = id,
              class = "btn btn-default navbar-btn action-button",
              style = "padding-right: 10px;",
              label)
}

navbarNav <- function(..., pull = "left") {
  id <- 1 # sample(1:1000, 1)
  tabs <- list(...)
  for (i in seq_along(tabs)) {
    tabs[[i]]$children[[1]]$attribs$href <- paste0("#tab-", id, "-", i)
  }
  tags$ul(class = paste0("nav navbar-nav pull-", pull),
          `data-tabsetid` = id,
          tabs)
}

navTab <- function(title, ..., active = FALSE) {
  if (active) {
    tags$li(tags$a(href = title, title, `data-toggle` = "tab", `data-value` = title), class = "active")
  } else {
    tags$li(tags$a(href = title, title, `data-toggle` = "tab", `data-value` = title))
  }
}

navbarCollapse <- function(...) {
  div(class = "collapse navbar-collapse navbar-left", ...)
}

#' @import shiny
#' @export
navbar <- function(..., collapse = TRUE, position = "fixed-top") {

  navbar.content <- div(class = "container", # container-fluid
                        ...)

  tagList(
    tags$head(tags$style(HTML("body {padding-top: 70px;}"))),
    tags$nav(class = paste0("navbar navbar-default navbar-", position),
             role = "navigation",
             navbar.content)
  )
}
