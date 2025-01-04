# HEADER --------------------------------------------
#
# Author:     Ardan Patwardhan
# Copyright     Copyright 2025 - Ardan Patwardhan
# Email:      ardan@ebi.ac.uk
#
# Date:     2025-01-02
#
# Script Name:    app.R
#
# Script Description: A Shiny web application that displays a scatter plot 
#                     generated using the Plotly library. The plot is inter-
#                     active and when the user clicks on a point, it is added
#                     to a table generated using the DataTables library. This 
#                     app can be viewed as a demo or template for implementing
#                     the workflow of viewing data in a plot and adding data 
#                     from the plot interactively to a table.
#
#
# SETUP ------------------------------------
cat("\014")                 # Clears the console
rm(list = ls())             # Remove all variables of the work space

#-------------------------------------------------------------------------------
# Libraries - keep in alphabetical order!
#-------------------------------------------------------------------------------

if(!require('bslib')) {
  install.packages('bslib')
  library('bslib')
}

if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

if(!require('DT')) {
  install.packages('DT')
  library('DT')
}

if(!require('fs')) {
  install.packages('fs')
  library('fs')
}

if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}

if(!require('htmlwidgets')) {
  install.packages('htmlwidgets')
  library('htmlwidgets')
}

if(!require('plotly')) {
  install.packages('plotly')
  library('plotly')
}

if(!require('purrr')) {
  install.packages('purrr')
  library('purrr')
}

if(!require('shiny')) {
  install.packages('shiny')
  library('shiny')
}

if(!require('shinyFeedback')) {
  install.packages('shinyFeedback')
  library('shinyFeedback')
}

if(!require('shinythemes')) {
  install.packages('shinythemes')
  library('shinythemes')
}

if(!require('vroom')) {
  install.packages('vroom')
  library('vroom')
}

#-------------------------------------------------------------------------------
# Front end user interface
#-------------------------------------------------------------------------------

ui <- page_sidebar(
  useShinyFeedback(),
  theme = bs_theme(version = 5, bootswatch = "united"),
  title = "Point selection tool",
  
  sidebar = sidebar(
    selectInput("dataset", "Select dataset to plot:", choice = NULL),
    actionButton("loadDataset", "Load dataset!"),
    tags$h4(HTML("... or generate a new dataset")),
    sliderInput("numPoints",
                "Number of points:",
                min = 10,
                max = 1000,
                value = 100),
    actionButton("newDataset", "Generate dataset!")
  ),
  
  plotlyOutput("pointPlot", fill=FALSE),
  DTOutput("pointTable", fill=FALSE),
  div(textOutput("fireOutput"), style="color:black;background:WhiteSmoke;font-size: 30px;text-align:center;")
)

#-------------------------------------------------------------------------------
# Server-side utility functions
#-------------------------------------------------------------------------------

#' Programmatically create a vector of Shiny inputs
#' 
#' @param FUN function to create the input
#' @param ids vector of unique suffixes to add to idPrefix to make unique IDs
#' @param idPrefix ID prefix for each input
shinyInput <- function(FUN, ids, idPrefix, ...) {
  
  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(ids, function(i){
    as.character(FUN(paste0(idPrefix, i), ...))
  }, character(1))
  
}

#-------------------------------------------------------------------------------
# Back-end server
#-------------------------------------------------------------------------------

server <- function(input, output) {
  dataDir <- "data"
  emptyTable <- list(x=numeric(0), y=numeric(0), ID=numeric(0))
  values <- reactiveValues(points = data.frame(emptyTable),
                           selectedPoints = data.frame(emptyTable),
                           fireText = character(),
                           plot = NULL)
  
  #-----------------------------------------------------------------------------
  # Event observers. Keep in alphabetical order!
  #-----------------------------------------------------------------------------
  
  # Initial load of list of available datasets in selectInput
  observeEvent(input$dataset, {
    print("Dataset changed")
    if(!isTruthy(input$dataset)) { # Set up the choices of the select widget
      filePaths <- dir_ls(dataDir, glob="*.csv")
      labels <- sapply(filePaths, function(x) { path_ext_remove(path_file(x)) })
      updateSelectInput(inputId = "dataset", choices = setNames(filePaths, labels))
    }
  })
  
  # Delete button pressed on table. Delete selected rows from table.
  observeEvent(input$deleteButton, {
    print("Delete button pressed!")
    if (!is.null(input$pointTable_rows_selected)) {
      values$selectedPoints <- values$selectedPoints[-as.numeric(input$pointTable_rows_selected),]
    }
  })
  
  # Handle Fire button being pressed in table.
  observeEvent(input$fireButton, {
    if (isTruthy((input$fireButton))) {
      values$fireText <- substring(as.character(input$fireButton), 12)
    }
  })
  
  # Load a dataset when loadDataset is clicked
  observeEvent(input$loadDataset, {
    req(input$dataset)
    data <- vroom(input$dataset, delim = "," , col_types = c(x="d", y="d", ID="i"))
    
    # Check that data has the required columns
    colNames <- colnames(data)
    reqColNames <- names(emptyTable)
    colsNotFound <- reduce(reqColNames, function (out, colName) { if (colName %in% colNames) return(out) else return(c(out,colName));}, .init = list())
    allColsFound <- is_empty(colsNotFound)
    feedbackText <- ""
    if (!allColsFound) {
      feedbackText <- paste0("Required column(s) not found: ", paste0(colsNotFound, collapse = ", "))
    }
    feedbackDanger("dataset", !allColsFound, feedbackText)
    req(allColsFound, cancelOutput = TRUE)
    
    values$points <- as.data.frame(data)
    print("File read!")
  })
  
  # If newDataset button is clicked, generate a new random set of plot
  # coordinates. The number of points is taken from the numPoints slider.
  observeEvent(input$newDataset, {
    values$points <- data.frame( x = rnorm(input$numPoints),
                                 y = rnorm(input$numPoints),
                                 ID = seq(1:input$numPoints))
  })
  
  # Handle selection of point on plot
  observeEvent(input$plotly_click, {
    print("Plotly clicked!")
    if(is.null(input$plotly_click)) return()
    # res <- points() %>% filter(ID == input$plotly_click)
    res <- values$points %>% filter(ID == input$plotly_click)
    if(nrow(res) == 0) return ()
    if (res[1,]$ID %in% values$selectedPoints$ID) return ()
    values$selectedPoints <- rbind(res[1,], values$selectedPoints)
  })
  
  # Reset selectedPoints and fireText when points changes
  observeEvent(values$points, {
    values$selectedPoints <- data.frame(emptyTable)
    values$fireText <- ""
  })
  
  #-----------------------------------------------------------------------------
  # Render functions - keep in alphabetical order!
  #-----------------------------------------------------------------------------

  # Show the number of the fire button pressed
  output$fireOutput <- renderText({
    print("Fire button pressed!")
    req(values$fireText)
    return(paste0("Fire button: ", values$fireText))
  })

  # Render plot
  output$pointPlot <- renderPlotly({
    req(nrow(values$points) > 0)
    # plot1 <- ggplot(points()) +
      # geom_point(aes(x, y, customdata = ID), colour = "blue" )
    plot1 <- ggplot(values$points) +
      geom_point(aes(x, y, customdata = ID), colour = "blue" ) +
      labs(title = "Random Point Distribution", x = "Coordinate A [m]", y = "Coordinate B [m]") 
    values$plot <- ggplotly(plot1, source = "pointPlot") %>% 
      onRender("
  function(el) {
    el.on('plotly_click', function(d) {
      Shiny.setInputValue(\"plotly_click\", d.points[0].customdata, {priority: \"event\"});
    });
  }
")
    values$plot
  })
  
  # Render table with selected points
  output$pointTable <- renderDT(server = FALSE, # In order to download full table we cannot use server-side processing
                                {
                                  print("renderTable called\n")
                                  req(nrow(values$selectedPoints) > 0)
                                  datatable(values$selectedPoints %>%
                                              mutate(Action = shinyInput(
                                                FUN = actionButton,
                                                ids = ID, # Vector of unique suffixes to add to all buttons
                                                idPrefix = 'fireButton_',
                                                label = "Fire",
                                                # We need to handle onmousedown istead of onclick because select uses onmousedown and we need to override the default functionality
                                                onmousedown = 'function handleClick(e, id) {e.stopImmediatePropagation(); Shiny.setInputValue(\"fireButton\", id, {priority: \"event\"});  }; handleClick(event, this.id);'
                                              )) %>%
                                              mutate(ID = vapply(ID, function(id){paste0('<a href=\"https://www.bbc.co.uk\" target=\"_blank\">', as.character(id), '</a>')},character(1))),
                                            extensions = 'Buttons',
                                            escape=FALSE, # Needs to be FALSE, otherwise HTML is not handled correctly
                                            # selection='none', # Needed if buttons are in the rows?
                                            options = list(
                                              pageLength = 5,
                                              dom = 'Bfrtip',
                                              buttons = list(
                                                list(
                                                  extend = 'collection',
                                                  text = 'Download',
                                                  buttons = c(mapply(function(x,y) { list('extend'= x, 'text' = y, exportOptions = list(columns = c(0:2)))}, 
                                                                     c("csv", "excel", "pdf", "copy", "print"), 
                                                                     c("CSV", "Excel", "PDF", "Copy", "Print"), 
                                                                     SIMPLIFY = FALSE), 
                                                              use.names = FALSE)
                                                ),
                                                list(
                                                  extend = '',
                                                  text = 'Delete',
                                                  action = DT::JS("function ( e, dt, node, config ) {
                  Shiny.setInputValue('deleteButton', 'pressed', {priority: 'event'});
                                }")
                                                ))
                                            ),
                                            rowname = FALSE) %>%
                                    formatRound(columns=c('x', 'y'), digits=3)
                                })
}

# Run the application 
shinyApp(ui = ui, server = server)
