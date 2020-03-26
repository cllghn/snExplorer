modal_import_data <- function() {
  showModal(
    modalDialog(
      box(
        width = 12,
        tags$style("<center>"),
        selectInput(
          inputId = "example_data",
          label   = tags$h4("Use example data"),
          choices = list(
            "",
            "Australian Embassy Bombing (2004)" = "australian_embassy_bombing_2004",
            "Bali Bombing (2005)"               = "bali_bombings_2005",
            "Ciel"                              = "ciel",
            "Drugnet"                           = "drugnet",
            "Koschade"                          = "Koschade",
            "London Gang"                       = "london_gang",
            "Noordin Operations"                = "noordin_operational",
            "Operation Acero"                   = "acero",
            "Operation Jake"                    = "jake",
            "Operation Juanes"                  = "juanes",
            "Operation Mambo"                   = "mambo",
            "Siren"                             = "siren"
            ),
          width   = "100%" 
        ),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$style("<center>"),
        selectInput(
          inputId = "input_format",
          label   = tags$h4("Select an input format"),
          choices = list(
            "",
            "Edge list" = "el"
          ),
          width = "100%"
        ),
        tags$br(),
        uiOutput("reactive_input_file")
      )
    )
  )
}
