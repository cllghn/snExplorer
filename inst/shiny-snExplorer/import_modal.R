modal_import_data <- function() {
  showModal(
    modalDialog(
        box(
            width = 12,
            tags$style("<center>"),
            selectInput(
                inputId = "input_type",
                label   = tags$h4("Select data importing option"),
                choices = list(
                    "",
                    "Use internal data" = "internal_data",
                    "Use external data" = "external_data"
                )
            ),
            tags$br(),
            uiOutput("data_importation")
        )
    )
  )
}
