


#' Launch item bank explorer
#'
#' @param item_bank
#'
#' @return
#' @export
#'
#' @examples
item_bank_explorer <- function(item_bank) {

  nms <- lapply(item_bank, class)
  nms <- nms[which(nms == "numeric" | nms == "integer")]

  ui <- shiny::fluidPage(


    musicassessr::include_musicassessr_js(record_audio = FALSE,
                                          visual_notation = TRUE),

    shiny::tags$h2("Corpus Explorer"),

    # shiny::selectInput(inputId = "itemBank",
    #                    label = "Item Bank",
    #                    choices = c("Berkowitz: ngram",
    #                                "Berkowitz: phrases")),

    shiny::tags$br(),

    shiny::downloadButton("dl",
                          "Download Corpus"),

    shiny::tags$br(),
    shiny::tags$br(),

    shiny::tabsetPanel(type = "tabs",
                filter_item_bank_tab(nms, item_bank),
                similarity_tab())
    )


  server <- function(input, output) {

    tmp_df <- reactive({
      item_bank
    })

    filtered_df <- reactive({
      purrr::reduce(names(nms), sliderServer, .init = tmp_df())
    })

    output$melodyNotation <- shiny::renderUI({

      if (is.null(input$df_rows_selected)) {
        print("No Melody Selected.")
      }
      else {
        melody <- filtered_df() %>% grab_mel(input$df_rows_selected)
        abs_melody <- cumsum(melody) + 60
        musicassessr::present_stimuli_midi_notes_both(stimuli = abs_melody, note_length = 0.5)
      }
    })

    output$network <- visNetwork::renderVisNetwork({

      if (is.null(input$df_rows_selected)) {
        NULL
      }
      else {
        melody <- filtered_df() %>% grab_mel(input$df_rows_selected)
        sim_matrix_to_graph(melody)
      }
    })


    output$df <- DT::renderDT(filtered_df(), selection = 'single', escape = FALSE,
                                    options = list(searching = TRUE, pageLength = 20))

    output$histogram <- renderPlot({
      filtered_df() %>% itembankr::hist_item_bank()
    })

    output$dl <- shiny::downloadHandler(
      filename = function() { paste0("Berkowitz", Sys.Date(), ".xlsx") },
      content = function(file) {
        writexl::write_xlsx(filtered_df(), path = file)
      })

  }

  shiny::shinyApp(ui, server)
}



sim_matrix_to_graph <- function(melody) {
  print('sim_matrix')
  print(melody)
  ngrams <- dplyr::bind_rows(lapply(3:length(melody), function(x) itembankr::get_all_ngrams(melody, N = x)))

  sim.matrix <- combn(ngrams$value, 2, FUN = function(x) musicassessr::ngrukkon(itembankr::str_mel_to_vector(x[1], sep = ","),
                                                                                itembankr::str_mel_to_vector(x[2], sep = ",")))

  sim.matrix <- matrix(sim.matrix, ncol = length(ngrams$value), nrow = length(ngrams$value))

  nodes <- data.frame(id = ngrams$value, label = "\uf286 <div id = 'sheet_music'></div> <b>This</b> is an\n<i>html</i> <b><i>multi-</i>font</b> <code>label</code>'")

  # # put row names to col names
  row.names(sim.matrix) <- ngrams$value
  colnames(sim.matrix) <- ngrams$value
  sim.matrix
  # hm, where are the diagonal ones?

  # create adjacency matrix
  threshold <- 0.2
  g <- igraph::graph.adjacency(sim.matrix > threshold)
  print(g)

  # create edges
  edges <- as.data.frame(igraph::get.edgelist(g))
  colnames(edges) <- c("from","to")

  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visPhysics(stabilization = FALSE) %>%
    visNetwork::visEdges(smooth = FALSE) %>%
    visNetwork::visLayout(randomSeed = 12, improvedLayout = FALSE) %>%
    visNetwork::visNodes(font = list(multi = TRUE, face = 'FontAwesome', ital = list(mod = ''),
                                     bold = list(mod = ''))) %>%
    visNetwork::addFontAwesome()
  network

}


similarity_tab <- function() {

  shiny::tabPanel("Similarity",

                  musicassessr::present_stimuli_midi_notes_visual(stimuli = 60:65, present_div = FALSE),

                  visNetwork::visNetworkOutput("network")

  )
}

filter_item_bank_tab <- function(nms, item_bank) {

  shiny::tabPanel("View and Filter",
                  shiny::tags$br(),
                  shiny::htmlOutput('melodyNotation'),
                    shiny::plotOutput("histogram") %>%
                      shinycssloaders::withSpinner(color="#8bcf42"),

                  purrr::map(names(nms), function(n) {
                    sliderUI(id = n, item_bank = item_bank)
                  }),

                  DT::DTOutput("df")
  )
}


grab_mel <- function(df, row) {
  df %>%
    dplyr::slice(row) %>%
    dplyr::pull(melody) %>%
    itembankr::str_mel_to_vector()
}

sliderUI <- function(id, item_bank) {

  ns <- shiny::NS(id)

  min <- min(item_bank[[id]], na.rm = TRUE)
  max <- max(item_bank[[id]], na.rm = TRUE)

  shiny::tags$div(
    shiny::sliderInput(inputId = ns('slider'),
              label = id,
              min = min,
              max = max,
              value = c(min, max)),
              style = "display:inline-block")
}

sliderServer <- function(df, id) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      id <- as.name(id)
      df %>% dplyr::filter(dplyr::between(!!id, input$slider[1], input$slider[2]))
    }
  )
}


# item_bank_explorer(Berkowitz::Berkowitz("main"))
# item_bank_explorer(Berkowitz::Berkowitz("phrases"))


# t <- lapply(Berkowitz::Berkowitz("phrases"), class)


