shinyServer(function(input, output, session) {
  # Launch the data import modal ===============================================
  modal_import_data()
  
  ## Reactive UI for different import options (e.g., edgelist, matrix, etc.) ---
  output$reactive_input_file <- renderUI({
    validate(
      need(
        input$input_format != "",
        "Please select input format."
      )
    )
    if (input$input_format == "el") {
      column(12,
             tags$b("DISCLAIMER:"), 
             tags$p("This import functionality expects an edge list in which the first two columns represent the actors to be connected, each row defines one edge."),
             fileInput(
               inputId     = "in_edges",
               label       = tags$h4("Import an edge list"),
               buttonLabel = "Browse",
               accept      = c(".csv") 
             ),
             tags$br(),
             tags$style("<center>"),
             radioButtons(
               inputId = "input_directed_undirected",
               label   = tags$h4("Is this network directed or undirected?"),
               choices = list(
                 "Directed",
                 "Undirected"
               ),
               selected = "Directed",
               inline = TRUE
             )
      )}
  })
  
  # Set up observers ===========================================================
  observeEvent(input$call_import_modal, {
    modal_import_data()
  })
  
  observeEvent(input$call_export_modal, {
    modal_export_data()
  })
  
  observeEvent(input$restart_app, {
    js$reset()
  })
  
  # Importing files ============================================================
  FILES <- reactiveValues()
  
  observeEvent(input$in_edges$datapath, {
    validate(
      need(input$in_edges != "",
           "Please select example data.")
    )
    FILES$file_to_import <- input$in_edges$datapath
    FILES$type           <- input$input_format
    FILES$directed       <- input$input_directed_undirected == "Directed"
  })
  
  observeEvent(input$example_data, {
    validate(
      need(input$example_data != "",
         "Please select example data.")
      )
    FILES$file_to_import <- paste0("example-data/", input$example_data, ".csv")
    FILES$type           <- "example"
    FILES$directed       <- FALSE
  })
  
  # Get edges ==================================================================
  # The goal of this data ingestion pipeline is to take different types of
  # network formats and return them as an edge list. This was kept as an 
  # independant function for flexibility, but it can be removed.
  get_edges_table <- eventReactive(FILES$file_to_import, {
    read_csv(FILES$file_to_import, na = "")
  })
  
  # Get graph  =================================================================
  # Since various reactive output rely on the graph, this function lists them
  # all and passes them in one consistent format.
  get_graph <- reactive({
    
    out <- graph_from_data_frame(d = get_edges_table(), 
                                 directed = FILES$directed)

    # Set network level attributes -----------------------------------------
    graph_attr(out) <- c(
      graph_attr(out) %{}% NULL,
      #* Calculate topological metrics -------------------------------------
      list(
        density = edge_density(out),
        clustering = transitivity(out, type = "average"),
        size = vcount(out),
        edges = ecount(out),
        #* Calculate basic subgrouping metrics ------------------------------
        components = components(out, mode = "weak")$no,
        cliques = length(igraph::cliques(out, min = 3)),
        kcore = max(coreness(out))
      )
    )
    # Set vertex attributes --------------------------------------------------
    vertex_attr(out) <- c(
      vertex_attr(out) %{}% NULL,
      list(
        #* Metrics -------------------------------------------------------------
        total_degree = degree(out, mode = "total"),
        in_degree = degree(out, mode = "in"),
        out_degree = degree(out, mode = "out"),
        betweenness = round(
          betweenness(out, directed = is_directed(out), 
                      normalized = TRUE, weights = NULL),
          digits = 3
        ),
        eigenvector = round(
          eigen_centrality(out, directed = is_directed(out), weights = NULL)$vector,
          digits = 3
        ),
        constraint = round(1.125 - constraint(out), digits = 3),
        #* Other ---------------------------------------------------------------
        size = rep(25, length = vcount(out)),
        id = vertex_attr(out, "name") %||% seq_along(V(out))
      )
    )
    
    out
    
  })
  
  # Context information ========================================================
  output$file_path <- eventReactive(FILES$file_to_import, {
    validate(
      need(!is.null(FILES$file_to_import),
           "The input is NULL"
           )
    )
    paste0(
      "Currently loaded: ",
      basename(
        file.path(
          FILES$file_to_import
          )
        )
      )
  })
  
  output$context_box_graph <- renderInfoBox({
    infoBox(title = tags$h4("Graph"),
            value = if (is_directed(get_graph())) "directed" else "undirected",
            icon  = icon("bezier-curve"),
            fill  = TRUE
    )
  })
  
  output$context_box_edges <- renderInfoBox({
    infoBox(title = tags$h4("Edges"),
            value = ecount(get_graph()),
            icon  = icon("arrows-h"),
            fill  = TRUE
    )
  })
  
  output$context_box_nodes <-  renderInfoBox({
    infoBox(title = tags$h4("Nodes"),
            value = vcount(get_graph()),
            icon  = icon("dot-circle"),
            fill  = TRUE
    )
  })
  
  # Network Visualization ======================================================
  output$network_visualization <- renderVisNetwork({

    visIgraph(get_graph()) %>%
      visIgraphLayout(layout = input$graph_layout) %>%
      visNodes(color = list(
        background = "lightblue",
        border     = "slategrey",
        highlight  = list(
          background = "orange",
          border     = "darkred"
        )
      )) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection=TRUE) %>%
      visInteraction(navigationButtons = TRUE,
                     keyboard=TRUE)
  })
  
  observeEvent(input$node_sizing, {
    get_sizes <- reactive({
      if ("none" %in% input$node_sizing) return(
        rep(
          25,
          vcount(
            get_graph()
            )
          )
        )
      if ("total_degree" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "total_degree")
        )
      if ("in_degree" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "in_degree")
        )
      if ("out_degree" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "out_degree")
        )
      if ("betweenness" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "betweenness")
        )
      if ("eigenvector" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "eigenvector")
        )
      if ("constraint" %in% input$node_sizing) return(
        vertex_attr(
          get_graph(),
          name = "constraint")
        )
    })
    
    nodes <- get.data.frame(get_graph(), "vertices")
    nodes[['size']] <- rescale(get_sizes(), to = c(10, 30))
    
    visNetworkProxy("network_visualization") %>%
      visUpdateNodes(nodes = nodes)
  })
  
  # Network Modifiers ==========================================================
  output$modify_ui <- renderUI({
    if (is_directed(get_graph())) {
      column(
        width = 12,
        selectInput(inputId = "node_sizing",
                    label   = "Node Sizing Metrics",
                    choices = list(
                      "None" = "none",
                      "In-degree" = "in_degree",
                      "Out-degree" = "out_degree",
                      "Total-degree" = "total_degree",
                      "Betweenness" = "betweenness",
                      "Eigenvector" = "eigenvector",
                      "Inverse Constraint" = "constraint"
                    )
        )
      )
    }
    column(
      width = 12,
      selectInput(inputId = "node_sizing",
                  label   = "Node Sizing Metrics",
                  choices = list(
                    "None" = "none",
                    "Total-degree" = "total_degree",
                    "Betweenness" = "betweenness",
                    "Eigenvector" = "eigenvector",
                    "Inverse Constraint" = "constraint"
                  )
      )
    )
  })
  
  # Generate network measures ==================================================
  ## Network level metrics -----------------------------------------------------
  output$metrics_topography <- DT::renderDataTable({
    data.frame(
      Variable = c("Density",
                   "Local Clustering Coefficient",
                   "Size",
                   "Number of Edges"),
      Score = c(round(graph_attr(get_graph(), name = "density"), digits = 3),
                round(graph_attr(get_graph(), name = "clustering"), digits = 3),
                graph_attr(get_graph(), name = "size"),
                graph_attr(get_graph(), name = "edges")),
      Explanation = c("Density is formally defined as the total number of observed ties in a network divided by the number of possible ties.",
                      "The sum of each actor's clustering coefficient divided by the number of actors within the network.",
                      "A count of the number of actors in a network.",
                      "The number of edges in the network."),
      stringsAsFactors = FALSE
    ) %>%
      DT::datatable(rownames = FALSE,
                escape   = FALSE,
                width    = "100%",
                options  = list(
                  dom          = "ti",
                  scrollX      = TRUE,
                  ordering     = FALSE,
                  pageLength   = 4,
                  autoWidth    = FALSE,
                  lengthChange = FALSE,
                  searching    = FALSE,
                  bInfo        = FALSE,
                  bPaginate    = TRUE,
                  bFilter      = FALSE
                  )
                )
  })
  
  ## Group level metrics -------------------------------------------------------
  output$metrics_subgroup <- DT::renderDataTable({
    data.frame(
      Variable = c("Weak Components", "Number of Cliques", "Max K-Core"),
      Score = c(graph_attr(get_graph(), "components"), 
                graph_attr(get_graph(), "cliques"),
                graph_attr(get_graph(), "kcore")),
      Explanation = c("Subgroups of actors who can reach each other directly.",
                      "Maximal number of subsets of three or more where each actor is directly connected to all others.",
                      "A maximal group of actors, all of whom are connected to some number (k) of other group members."),
      stringsAsFactors = FALSE
    ) %>%
      DT::datatable(rownames = FALSE,
                escape   = FALSE,
                width    = "100%",
                options  = list(
                  dom          = "ti",
                  scrollX      = TRUE,
                  ordering     = FALSE,
                  pageLength   = 3,
                  autoWidth    = FALSE,
                  lengthChange = FALSE,
                  searching    = FALSE,
                  bInfo        = FALSE,
                  bPaginate    = TRUE,
                  bFilter      = FALSE
                ))
  })
  
  ## Vertex level metrics ------------------------------------------------------
  output$metrics_vertex <- DT::renderDataTable({
    if (FILES$directed == TRUE) {
      data.frame(
        ID = vertex_attr(get_graph(), "name"),
        `In Degree` = vertex_attr(get_graph(), "in_degree"),
        `Out Degree` = vertex_attr(get_graph(), "out_degree"),
        `Total Degree` = vertex_attr(get_graph(), "total_degree"),
        Betweenness    = vertex_attr(get_graph(), "betweenness"),
        Eigenvector    = vertex_attr(get_graph(), "eigenvector"),
        `Inverse Constraint` = vertex_attr(get_graph(), "constraint"),
        stringsAsFactors = FALSE
      ) %>%
        DT::datatable(rownames = FALSE,
                      escape   = FALSE,
                      width    = "100%",
                      options  = list(
                        dom          = "tilfpr",
                        scrollX      = TRUE,
                        ordering     = TRUE,
                        pageLength   = 10,
                        autoWidth    = FALSE,
                        lengthChange = FALSE,
                        searching    = FALSE,
                        bInfo        = TRUE,
                        bPaginate    = TRUE,
                        bFilter      = FALSE
                        ))
    }
    data.frame(
      ID = vertex_attr(get_graph(), "name"),
      `Total Degree` = vertex_attr(get_graph(), "total_degree"),
      Betweenness    = vertex_attr(get_graph(), "betweenness"),
      Eigenvector    = vertex_attr(get_graph(), "eigenvector"),
      `Inverse Constraint` = vertex_attr(get_graph(),
                                                  "constraint"),
               stringsAsFactors = FALSE
    ) %>%
      DT::datatable(rownames = FALSE,
                    escape   = FALSE,
                    width    = "100%",
                    options  = list(
                      dom          = "tilfpr",
                      scrollX      = TRUE,
                      ordering     = TRUE,
                      pageLength   = 10,
                      autoWidth    = FALSE,
                      lengthChange = FALSE,
                      searching    = FALSE,
                      bInfo        = TRUE,
                      bPaginate    = TRUE,
                      bFilter      = FALSE
                    )) 
  })
  
  ## Generate report -----------------------------------------------------------
  output$download_report <- downloadHandler(
    validate(
      need(!is.null(input$report_name) || input$report_name != "",
           "The input is NULL or blank"
      )
    ),
    filename = function() paste0(input$report_name, ".html"),
    content  = function(file) {
      report <- file.path(tempdir(), 'report.Rmd')
      file.copy('markdown/report.Rmd', report, overwrite = TRUE)
      params <- list(
        graph = get_graph(),
        gname = FILES$file_to_import,
        gdir  = FILES$directed
        )
      render(input       = report,
             output_file = file,
             params      = params,
             envir       = new.env(parent = globalenv()))
    }
  )
})
