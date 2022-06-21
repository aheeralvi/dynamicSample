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

    # Df table read in from another file

    df <- read.table("/Users/aheeralvi/witchcraft/dynamicSample/inst/extdata/programs/program1/metadata/metadata.txt", header = TRUE)
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
          lookupID <- df[i,6]
          choicesInsert <- lookup(lookupID)
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = radioButtons(df[i,1], df[i,2],
                   choices = choicesInsert,
                   selected = df[i,4],
                   width = paste(sep="", df[i,5], "px")))
        }
        else if(grepl("CheckboxG.*", inputType)) {
          # Same cut-off process for checkbox group input
          lookupID <- df[i,6]
          choicesInsert <- lookup(lookupID)
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = checkboxGroupInput(df[i,1], df[i,2],
                   choices = choicesInsert,
                   selected = df[i,4],
                   width = paste(sep="", df[i,5], "px")))
        }
        else if(inputType == "SingleSelect") {
          lookupID <- df[i,6]
          # include option for collection example
          choicesInsert <- lookup(lookupID, df[i,7])
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = selectInput(df[i,1], df[i,2],
                   choices = choicesInsert, selected = df[i,4], width = paste(sep="",df[i,5],"px")))
        }
        else if(inputType == "MultiSelect") {
          lookupID <- df[i,6]
          choicesInsert <- lookup(lookupID)
          insertUI(selector = "#submit",
                   where = "beforeBegin",
                   ui = selectInput(df[i,1], df[i,2],
                                    choices = choicesInsert, multiple = TRUE, selected = df[i,4], width = paste(sep="",df[i,5],"px")))
        }
      }
    }) # End onload

    # When submit is pressed,
    observeEvent(input$submit, ignoreNULL = TRUE, ignoreInit = TRUE, {

      # Copy sample program
      path <- getwd()
      path <- paste(path, "/inst/extdata/programs/program1/SampleReportProgram2.R", sep="")
      fileText <- readLines(path)


      # list_inputs <- list(input$datecheck, input$boolean, input$cylinder, input$radio, input$multchoice)
      # names(list_inputs) <- df[1]
      # print(list_inputs)


      # Replace tag string with proper input value
      speciesName <- input$selectbox
      speciesName <- paste(sep="","\"",speciesName,"\"")

      fileText <- gsub("\"{InputSelect}\"", fixed = TRUE, tolower(speciesName), fileText)


      # Create temporary directory if doesn't exist
      dir <- tempdir()
      if (!dir.exists(dir))
        dir.create(dir)

      # Else cleanup directory
      else {
        ls <- list.files(
          dir,
          full.names = TRUE,
          pattern = "file_....-..-.._........_temp.R")
        unlink(ls, recursive = TRUE)
      }


      # Create a filename based on date/time
      timeSave <- toString(Sys.time())

      dateSplit1 <- strsplit(timeSave, " ")
      date <- dateSplit1[[1]][1]
      time <- dateSplit1[[1]][2]
      time <- gsub(":", ".", time)

      customName <- paste(sep="" ,"file_", date, "_", time, "_temp.R")
      customName <- paste(sep="/", tempdir(), customName)

      # Add the modified text into file in temp directory
      file.create(customName)
      writeLines(fileText, customName, sep = "\n")

      # Run modified program
      source(customName)

      stopApp()
    })

  } # End server

  viewer <- dialogViewer("Enter Number of __________", width = 1000)
  runGadget(ui, server, viewer = viewer)
} # End gadget


# Return vector of choices for a control widget requiring multiple options
lookup <- function(lookupID, collectID) {

  # Lookup table read in from file

  lookupTable <- read.table("/Users/aheeralvi/witchcraft/dynamicSample/inst/extdata/common/lookupTable.txt")

  # By row
  for(i in 1:nrow(lookupTable)) {
    # Identify correct id
    if(lookupTable[i,1] == lookupID) {
      # Split string of all choices into vector containing choices
      radioChoices <- strsplit(lookupTable[i,2], " ")
    }
  }

  # Collect options from data if requested, otherwise return values from lookup table
  if(lookupID == "Collect") {
    radioChoices <- collectChoices(collectID)
    return(radioChoices)
  }
  else {
    return(radioChoices[[1]])
    }

}


# Collect choices for buttons from data dynamically
collectChoices <- function(collectID) {

  # Copy in data
  dataCopy <- read.csv("/Users/aheeralvi/witchcraft/dynamicSample/inst/extdata/programs/program1/data/iris.csv")

  # Search for column name on which to block choices
  columnNames <- names(dataCopy)
  searchCol <- match(collectID, columnNames)

  # return unique values only
  return(unique(dataCopy[[searchCol]]))
}



