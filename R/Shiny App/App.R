library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("SST Correction"),

  sidebarLayout(
    sidebarPanel(
      textInput("search_width", "Search Width:", ""),
      textInput("search_height", "Search Height:", ""),
      textInput("SST_folder", "SST Folder:", ""),
      actionButton("select_folder", "Select Folder"),
      textInput("Delta_T", "Delta T:", ""),
      textInput("Min_Surface_Time", "Min Surface Time:", ""),
      actionButton("close", "Close SST Correction"),
      actionButton("pushbutton7", "Close Application")
    ),

    mainPanel(
      # Add additional UI elements here as needed
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  options <- reactiveValues(
    search_width = "",
    search_height = "",
    SST_folder = "",
    Delta_T = "",
    Min_Surface_Time = ""
  )

  observeEvent(input$search_width, {
    options$search_width <- input$search_width
  })

  observeEvent(input$search_height, {
    options$search_height <- input$search_height
  })

  observeEvent(input$SST_folder, {
    options$SST_folder <- input$SST_folder
  })

  observeEvent(input$select_folder, {
    # Custom folder selection logic
    # This part needs additional implementation as Shiny doesn't support native folder selection
  })

  observeEvent(input$Delta_T, {
    options$Delta_T <- input$Delta_T
  })

  observeEvent(input$Min_Surface_Time, {
    options$Min_Surface_Time <- input$Min_Surface_Time
  })

  observeEvent(input$close, {
    showModal(modalDialog(
      title = "Close SST Correction?",
      "Are you sure you want to close SST Correction?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_close", "Yes")
      )
    ))
  })

  observeEvent(input$confirm_close, {
    removeModal()
    # Save options logic here
    stopApp()
  })

  observeEvent(input$pushbutton7, {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
