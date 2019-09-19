
#' @rdname navbar
#' @export

divTabContent <- function(...) {
  tags$div(
    class = "tab-content",
    ...
  )
}


#' @rdname navbar
#' @export

divTabPane <- function(id, ..., active = FALSE) {
  class <- "tab-pane"
  if (active) {
    class <- paste0(class, " active")
  }
  tags$div(
    class = class, `data-value` = id, id = id,
    ...
  )
}
