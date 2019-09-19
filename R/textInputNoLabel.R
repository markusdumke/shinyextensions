#' Text input without label
#'
#' Same as [shiny::textInput()] but without label.
#'
#' @inheritParams shiny::textInput
#' @param id Id of element. Can be accessed via `input$id` in server.
#'
#' @export

textInputNoLabel <- function(id, value = "", placeholder = "") {
  tags$div(class = "form-group shiny-input-container",
           tags$input(id = id,
                      type = "text",
                      class = "form-control",
                      value = value,
                      placeholder = placeholder))
}