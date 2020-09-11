
# dashboardHeader ==============================================================
header <- dashboardHeader(
  title = "snExplorer",
  titleWidth = 200
)

# dashboardSidebar =============================================================
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem(
      text = "Network Explorer",
      tabName = "explorer",
      icon = icon("bezier-curve")
    ),
    menuItem(
      text = "Network Measures",
      tabName = "measures",
      icon = icon("chart-line")
    ),
    # menuItem(text    = "Geospatial Explorer",
    #          tabName = "map",
    #          icon    = icon("globe")),
    menuItem(
      text = "About",
      tabName = "about",
      icon = icon("info-circle")
    ),
    tags$br(),
    # Buttons:
    column(
      12,
      align = "center",
      actionButton(
        inputId = "call_import_modal",
        label = "Import Data"
      )#,
      # actionButton(inputId = "call_export_modal",
      #              label   = "Export Data"
      # ),
      # Call shinyjs
      # useShinyjs(),
      # extendShinyjs(text = jsResetCode),
      # actionButton(
      #   inputId = "restart_app",
      #   label = "Restart App"
      # )
    )
  )
)

# dashboardBody ================================================================

## network explorer  ===========================================================
tab_explorer <- tabItem(
  tabName = "explorer",
  fluidRow(
    box(
      title = "Context",
      width = 12,
      collapsible = FALSE,
      column(
        width = 12,
        tags$h4(textOutput("file_path"))
      ),
      tags$br(),
      fluidRow(
        infoBoxOutput("context_box_graph"),
        infoBoxOutput("context_box_nodes"),
        infoBoxOutput("context_box_edges")
      )
    )
  ),
  fluidRow(
    box(
      title = "Network Visualization",
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      visNetworkOutput(
        outputId = "network_visualization",
        width = "100%",
        height = "400px"
      )
    )
  ),
  fluidRow(
    box(
      title = "Modify Graph",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(
          width = 12,
          selectInput(
            inputId = "graph_layout",
            label = "Layout Algorithm",
            choices = list(
              "Fruchterman-Reingold" = "layout_with_fr",
              "Kamada-Kawai"         = "layout_with_kk",
              "Circle"               = "layout_in_circle",
              "Nicely"               = "layout_nicely",
              "MD"                   = "layout_with_mds"
            ),
            selected = "layout_with_fr"
          )
        ),
        uiOutput("modify_ui")
      )
    )
  )
)

## network measures ============================================================
tab_measures <- tabItem(
  tabName = "measures",
  fluidRow(
    # Network level metrics box ------------------------------------------------
    box(
      title = "Network Level Metrics",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      column(
        width = 12,
        tags$p("Network-level measures are calculated on the whole network and provide indicators of network structure."),
        fluidRow(
          column(
            width = 12,
            DT::dataTableOutput("metrics_topography")
          )
        )
      )
    ),
    # Group level metrics box --------------------------------------------------
    box(
      title = "Group Level Metrics",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      column(
        width = 12,
        tags$p("Subgrouping metrics leverage regularities in the network's structure to identify cohesive subcommunties."),
        fluidRow(
          DT::dataTableOutput("metrics_subgroup")
        )
      )
    ),
    # Vertex level metrics box -------------------------------------------------
    box(
      title = "Vertex Level Metrics",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      column(
        width = 12,
        tags$p("The metrics below are designed to determine which nodes occupy the center of a network."),
        fluidRow(
          DT::dataTableOutput("metrics_vertex")
        )
      )
    ),
    # Report box ---------------------------------------------------------------
    box(
      title = "Report",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      column(
        width = 4,
        tags$p(
          tags$b("Instructions: ")
        ),
        tags$p(
          tags$small("First, provide a name for name for export. Then, download the file.")
        )
      ),
      column(
        width = 8,
        fluidRow(
          textInput(
            inputId = "report_name",
            label = "Report identifier:",
            value = "",
            placeholder = "Identifier without extension..."
          )
        ),
        column(
          width = 12,
          align = "center",
          fluidRow(
            downloadButton("download_report",
              label = "Download Report"
            )
          )
        )
      )
    )
  )
)

## geospatial explorer =========================================================
tab_map <- tabItem(
  tabName = "map",
  fluidRow(
    box(
      title = "Set Up",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE
    ),
    box(
      title = "Map",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE
    )
  )
)

## about =======================================================================
tab_about <- tabItem(
  tabName = "about",
  fluidRow(
    box(
      width = 12,
      includeMarkdown("markdown/about.md")
    )
  )
)

## body
body <- dashboardBody(
  tabItems(
    tab_explorer,
    tab_measures,
    tab_map,
    tab_about
  )
)

# dashboardPage ================================================================
dashboardPage(
  header,
  sidebar,
  body
)
