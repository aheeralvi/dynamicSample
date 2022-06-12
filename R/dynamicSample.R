library(shiny)
library(miniUI)

# Gadget
dynamicSample <-function() {

  # UI
  ui <- miniPage(

    # See utils.R
    includeHighlightJs(),

    # Test inline css
    tags$head(
      tags$style(
        # Styles for buttons
        HTML("

             .btn-overload {
                background-color: #75aadb;
             }
             .btn-overload:hover {
                background-color: #75aadb;
                opacity: 0.7;
             }
             .btn-overload:focus {
                background-color: #75aadb;
                opacity: 0.9;
             }
             .btn-overload:active {
                background-color: #75aadb !important;
                opacity: 0.7 !important;
             }

             ")
      )),

    # Standard Submit/Cancel Buttons on the bottom
    miniContentPanel(
      HTML("<button id = 'submit'
           type ='button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Submit</button>"),
      HTML("<button id = 'cancel'
           type = 'button'
           class = 'btn-overload btn btn-default action-button shiny-bound-input'>
           Cancel</button>")
    )
  ) # End UI

  # Server
  server <- function(input, output, session) {

    # Df mimics table read in from another file
    df <- read.table(header = TRUE, text = "

    ID Label InputType DefaultValue Width

    datecheck 'Input Date' DateInput '2000-01-01' 200
    boolean 'Check/Uncheck box' Checkbox FALSE 100
    cylinder 'Input Cylinders' TextBox 6 100
    radio 'True/False' RadioTF True 100")


    # On-load event, handles insertion of elements described by table
    # Run on init, ignore input$submit=NULL, destroy event after first run
    observeEvent(input$submit, ignoreNULL = FALSE, ignoreInit = FALSE, once = TRUE, {

      # Iterate by row
      for(i in 1:nrow(df)) {
        inputType <- df[i,3]

        # Determine input type and insert
        if(inputType == "TextBox") {
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = textInput(df[i,1], df[i,2], df[i,4], paste(sep="", df[i,5], "px")))
        }
        else if(inputType == "DateInput") {
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = dateInput(df[i,1], df[i,2],
                                  value = df[i,4],
                                  width = paste(sep="", df[i,5], "px")))
        }
        else if(inputType == "Checkbox") {
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = checkboxInput(df[i,1], df[i,2],
                                      value = as.logical(df[i,4]),
                                      width = paste(sep="", df[i,5], "px")))
        }
        else if(grepl("Radio.*", inputType)) {
          # Cut "Radio" off of string to collect ID, send to lookup
          lookupID <- strsplit(inputType, "Radio")[[1]][2]
          htmlInsert <- lookup(lookupID, df[i,])
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = radioButtons(df[i,1], df[i,2],
                   choices = htmlInsert,
                   selected = df[i,4],
                   width = paste(sep="", df[i,5], "px")))
        }
      }
    }) # End onload
  } # End server

  viewer <- dialogViewer("Enter Number of __________", width = 1000)
  runGadget(ui, server, viewer = viewer)
} # End gadget


# Return vector of choices for a control widget requiring multiple options
lookup <- function(lookupID, values) {

  # Mimic lookup table read in from file
  lookupTable <- read.table(header = TRUE, text = "
    ID Options
    TF 'True False'
    ABCD 'A B C D'")

  # By row
  for(i in 1:nrow(lookupTable)) {
    # Identify correct id
    if(lookupTable[i,1] == lookupID){
      # Split string of all choices into vector containing choices
      radioChoices <- strsplit(lookupTable[i,2], " ")
    }
  }
  return(radioChoices[[1]])
}


