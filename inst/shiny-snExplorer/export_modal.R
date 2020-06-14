modal_export_data <- function() {
  showModal(
    modalDialog(
      box(
        tags$p("something will go here"),
        width = 12,
        tags$style("<center>"),
        selectInput(
          inputId = "export_data",
          label = tags$h4("Data export format:"),
          choices = list(
            "",
            "edgelist",
            "matrix"
          ),
          width = "100%"
        )
      )
    )
  )
}
