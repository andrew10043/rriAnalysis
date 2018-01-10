# Load libraries
library(shiny)
library(shinyjs)
library(jpeg)
library(RMySQL)

# Source authentication information for MySQL database ----
source("db.R")

# Source functions ----
source("functions.R")

# Formatting Parameters ----
cols <- c("#FF0000", "#000CFF", "#00FF28", "#F7FF00",
          "#FF6900", "#FF00EB", "#00F7FF", "#FFFFFF",
          "#000000", "#8B888A")

ui <- shinyUI(

  # Initialize navbar page ---- 
  navbarPage(
    "Renal Resistive Index Analysis", id = "tabs",
    
    # File upload interface ----
    tabPanel("Step 1",
             tags$head(tags$style(
               HTML('body, label, input, button, select {
                    font-family: "Avenir";
                    font-size:10px;
                    }')
               )
               ),
             
             # Entrance Message ----
             fluidRow(column(12,
                             h5("Welcome to the renal resistive index analysis 
                                platform. Please enter your information and 
                                upload images below.")
                             )
                      ),
             
             # Line break ----
             hr(),
             
             # Sidebar ----
             sidebarPanel(
                             # Select a file ----
                             fileInput(inputId = "files", 
                                       label = "Select Images to Analyze",
                                       multiple = TRUE,
                                       accept = c("image/jpeg")),
                             
                             # Conditional panel showing image IDs uploaded ----
                             conditionalPanel(condition = "output.fileUploaded",
                                              htmlOutput("files_uploaded")),
                             
                             # Line break ----
                             hr(),
                          
                             # Input: Select reader ----
                             radioButtons("reader", "Anesthesiologist Reading",
                                          choices = c('Anne Cherry, MD' = "ac",
                                                      'Mark Stafford-Smith, MD' = "mss"),
                                          selected = "ac"),
                             
                             # Line break ----
                             hr(),

                             # Passcode input to enable remote file upload ----
                             passwordInput(inputId = "passcode",
                                           label = "Data Upload Passcode",
                                           value = ""),
                             
                             # Action button to verify passcode ----
                             actionButton(inputId = "check_pass",
                                          label = "Verify Passcode"),
                             
                             # Breaks ----
                             br(),
                             br(),
                             
                             # Text of passcode verification result ----
                             htmlOutput(outputId = "pass_text"),
                             
                             # Line break ----
                             hr(),
                             
                             # Button to advance to analysisinterface ----
                             actionButton(inputId = "go_to_read",
                                          label = "Analyze Images")

                             ),

             # Logo Panel ----
             mainPanel(
               imageOutput("logo"),
               hr(),
               h4(uiOutput(outputId = "link"))
             )
             ),
    
    # Analysis interface ----
    tabPanel("Step 2", id = "analysis_tab",
             
             # Enable javascript ----
             useShinyjs(),
             
             tags$head(tags$style(
               HTML('body, label, input, button, select { 
                    font-family: "Avenir";
                    font-size:10px;
                    }')
   )),
   
   # Slider Colors ----
   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000000; border-top-color: #000000; border-bottom-color: #000000; border-color: #000000}")),
   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #8B888A; border-top-color: #8B888A; border-bottom-color: #8B888A; border-color: #8B888A}")),
   
   # Remove Slider Numbers ----
   tags$style(HTML(".js-irs-0 .irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important}")),
   
   # Sidebar panel ----
   sidebarPanel(width = 3,
                
            # Image measurable? ----
            radioButtons(inputId = "can_read",
                         label = "Is the image measurable?",
                         choices = c("Yes" = 0,
                                     "No" = 1),
                         selected = 0,
                         inline = TRUE),
                
            # Number of beats ----
            radioButtons(inputId = "num_beats",
                         label = "How many beats will be measured?",
                         choices = c("1" = 1,
                                     "2" = 2,
                                     "3" = 3),
                         selected = 3, 
                         inline = TRUE),
            
            # Numeric value of velocity marker ----
            numericInput(inputId = "velo_num", 
                         label = "What velocity (cm/s) is marked by the scale icon?", 
                         value = NA),
            
            # Heart rate ----
            numericInput(inputId = "heart_rate", 
                         label = "What is the heart rate?", 
                         value = NA),
            
            # Strip speed ----
            radioButtons(inputId = "strip_speed",
                         label = "What is the speed (mm/s)?",
                         choices = c("50" = 50,
                                     "75" = 75,
                                     "100" = 100,
                                     "125" = 125),
                         selected = character(0), 
                         inline = TRUE),
            
        
            # Metric selection dropdown menu ----
            selectInput(inputId = "metric_select", 
                        label = "Select a Metric to Move",
                        choices = c("Baseline" = "bl_select",
                                    "Scale" = "velo_select",
                                    "Peak 1" = "p1_select",
                                    "Trough 1" = "t1_select",
                                    "Peak 2" = "p2_select",
                                    "Trough 2" = "t2_select",
                                    "Peak 3" = "p3_select",
                                    "Trough 3" = "t3_select"),
                        selected = "bl_select"),

            # Line break ----
            tags$hr(),
            
            # Image submit button ----
            actionButton(inputId = "submit", label = "Submit Image")
     ),
   
   # Main image panel ----
   mainPanel(width = 9,
     
     column(12,
            
            # Text showing current image ID and image progress ----
            fluidRow(
              column(4,
                     htmlOutput("status")
                     )
            ),

            # Conditional buttons for download / return to input ----
            fluidRow(
            
              column(2,
                     offset = 0, 
                     br(),
                     actionButton(inputId = "go_to_entry", 
                                  label = "Upload Files")
                     ),
              
              column(2,
                     offset = 0,
                     br(),
                     downloadButton(outputId = "download_data",
                                    label = "Download Data")
              )
            ),
            
            # Line break ----
            tags$hr(),
            
            # Image-related questions ----
            fluidRow(
              column(2,
                     align = "center",
                     radioButtons("dicrotic", "Dicrotic Notch Present?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = character(),
                                  inline = TRUE),
                     br()
              ),
              column(2,
                     align = "center",
                     radioButtons("rounded", "Envelopes Rounded?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = character(0),
                                  inline = TRUE),
                     br()
              ),
              column(2,
                     align = "center",
                     radioButtons("flat_diastole", "Diastole Relatively Flat?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = character(0),
                                  inline = TRUE),
                     br()
              ),
              
              # Rhythym ----
              
              column(3,
                     align = "center",
                     radioButtons(inputId = "rhythm",
                                  label = "Regular Rhythm?",
                                  choices = c("Yes" = 1,
                                              "No" = 2,
                                              "Unclear" = 3),
                                  selected = character(0), 
                                  inline = TRUE),
                     br()
              ),
              
              
              column(3,
                     align = "center",

                     radioButtons(inputId = "paced",
                                  label = "Paced Rhythm?",
                                  choices = c("Yes" = 1,
                                              "No" = 2,
                                              "Unclear" = 3),
                                  selected = character(0), 
                                  inline = TRUE),
                     br()
              )
            ),
            
            # Conditional output of image / plot ----
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       style = "overflow-y:scroll; max-height: 100%;
                                overflow-x:scroll; max-width: 100%",
                       plotOutput(outputId = "image",
                                  click = "click"
                       )
                     )
              ),
              
              # Conditional sliders for marker adjustment / plot zoom ----
              column(12, align = "center",
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       
                       # Line break ----
                       hr(),
                       
                       column(4, align = "center",
                              sliderInput(inputId = "nudge", label = "Adjust Marker",
                                          min = 0, max = 1000, value = 500,
                                          ticks = FALSE)
                              ),
                       column(4, align = "center", offset = 0,
                              br(),
                              column(3, align = "center",
                                     actionButton(inputId = "prior_metric", label = "",
                                                  icon = icon(name = "arrow-left"))),
                              column(6, align = "center", offset = 0,
                                     htmlOutput(outputId = "current_metric",
                                                align = "center")),
                              column(3, align = "center",
                                     actionButton(inputId = "next_metric", label = "", 
                                                  icon = icon(name = "arrow-right")))),

                       column(4, align = "center",
                              sliderInput(inputId = "plot_size", label = "Plot Zoom",
                                          min = 0.01, max = 2, value = 1, step = 0.01,
                                          ticks = FALSE))
                       )
                     )
              )
            )
     )
   )
   )
  )

server <- function(input, output, session) {
  
  # Logo output ----
  output$logo <- renderImage({
    
    list(src = "DUSOM_anesthesiology.jpg",
         width = "95%")
    
  }, deleteFile = FALSE)
  
  # Basic Reactive Values ----
  rv <- reactiveValues(seq = 1,
                       data = data.frame("image_id" = NA,
                                         "date_time_submit" = NA,
                                         "anesthesiologist_measuring" = NA,
                                         "dicrotic_notch" = NA,
                                         "rounded_envelope" = NA,
                                         "flat_diastole" = NA,
                                         "baseline" = NA,
                                         "scale" = NA,
                                         "peak_1" = NA,
                                         "peak_2" = NA,
                                         "peak_3" = NA,
                                         "trough_1" = NA,
                                         "trough_2" = NA,
                                         "trough_3" = NA,
                                         "rri_1" = NA,
                                         "rri_2" = NA,
                                         "rri_3" = NA))
  
  # User inputs new files ----
  observeEvent(input$files, {
    
    # Reset data frame ----
    rv$data <- data.frame("image_id" = NA,
                          "date_time_submit" = NA,
                          "anesthesiologist_measuring" = NA,
                          "image_unmeasurable" = NA,
                          "num_beats" = NA,
                          "dicrotic_notch" = NA,
                          "rounded_envelope" = NA,
                          "flat_diastole" = NA,
                          "baseline" = NA,
                          "scale" = NA,
                          "velo_num" = NA,
                          "peak_1" = NA,
                          "peak_2" = NA,
                          "peak_3" = NA,
                          "trough_1" = NA,
                          "trough_2" = NA,
                          "trough_3" = NA,
                          "rri_1" = NA,
                          "rri_2" = NA,
                          "rri_3" = NA)
    
    # Reset seq counter
    rv$seq <- 1
    
    # Reset structure reactive values
    structures$bl <- img_dim()[2] - 10
    structures$velo <- img_dim()[2] - 10
    structures$peaks <- c(img_dim()[2] - 10, img_dim()[2] - 10, img_dim()[2] - 30)
    structures$troughs <- rep(img_dim()[2] - 30, 3)
    structures$bl_x <- 50
    structures$velo_x <- 125
    structures$peaks_x <- c(200, 275, 50)
    structures$troughs_x <- c(125, 200, 275)
    structures$click <- 50
    
  })
  
  # Define input files ----
  inFile <- reactive({
    
    if (is.null(input$files))
      return(NULL)
    
    return(input$files)
    
  })
  
  # Calculate total number of images ----
  image_total <- reactive({

    if (is.null(inFile()))
      return(0)
    
    return(nrow(inFile()))
  })
  
  # Grab file name ----
  file_name <- reactive({
  
    if (is.null(inFile()))
      return(NULL)
    
    return(stringi::stri_extract_first(str = inFile()$name, regex = ".*(?=\\.)"))
  })
  
  # Create file name output for input screen ----
  output$files_uploaded <- renderText({
    
    if (is.null(inFile)){
      return("")
    } else if (!is.null(inFile)){
      return(paste("<b><i>Image IDs uploaded: ", paste(file_name(), collapse = ", "), ".</b></i>", sep = ""))
    }
    
  })
  
  # Status display output ----
  output$status <- renderText({
    
    if (is.null(inFile()))
      return("<b><i><font color = red>No images have been uploaded.  Please click below to enter your information and upload files.</b></i></font>")
    
    if (rv$seq <= nrow(inFile()))
      return(paste("<b><i><font color = red>Current Image ID: ", file_name()[rv$seq], "<br>Progress: Image ", 
            rv$seq, " of ", image_total(), "</b></i></font>", sep = ""))
    
    if (rv$seq > nrow(inFile()))
      return("<b><i><font color = red>All images have been analyzed.  Please download the data file by clicking below prior to uploading additional images.</b></i><font color = red>")
    
  })
  
  # Define reactive values for image analysis ----
  structures <- reactiveValues(bl = NA,
                               velo = NA,
                               peak1 = NA,
                               peak2 = NA,
                               peak3 = NA,
                               trough1 = NA,
                               trough2 = NA,
                               trough3 = NA,
                               bl_x = 50,
                               velo_x = 125,
                               peak1_x = 200,
                               peak2_x = 275,
                               peak3_x = 50,
                               trough1_x = 125,
                               trough2_x = 200,
                               trough3_x = 275,
                               click = 50,
                               image_dim = c(NA, NA))

  # Define Current Metric ----
  metric <- reactive({

      mets <- c("bl_select" = "Baseline",
                "velo_select" = "Scale",
                "p1_select" = "Peak 1",
                "t1_select" = "Trough 1",
                "p2_select" = "Peak 2",
                "t2_select" = "Trough 2",
                "p3_select" = "Peak 3",
                "t3_select" = "Trough 3")
      
      return(as.character(mets[input$metric_select]))
    
  })
  
  # Current metric output for toggle ----
  
  output$current_metric <- renderText({
    
    paste("<b><font size = 2 px><font color = black>",
          "Metric Toggle:<br></font><font size = 2><font color = red>", 
          metric(), "</b></font>",
          sep = "")
    
  })
  
  # Submit button pressed ----
  observeEvent(input$submit, {
    
    errorMessasges <- c("Numeric Velocity not Identified",
                        "Baseline Not Marked",
                        "Velocity Not Marked",
                        "One or More Peaks Not Marked",
                        "One or More Troughs Not Marked"
                        )
    
    errors <- c(identical(input$velo_num, 0),
                identical(structures$bl, img_dim()[2] - 10),
                identical(structures$velo, img_dim()[2] - 10),
                identical(structures$peaks[1], img_dim()[2] - 10) |
                  identical(structures$peaks[2], img_dim()[2] - 10) |
                  identical(structures$peaks[3], img_dim()[2] - 30),
                identical(structures$troughs[1], img_dim()[2] - 30) |
                  identical(structures$troughs[2], img_dim()[2] - 30) |
                  identical(structures$troughs[3], img_dim()[2] - 30)
                )
    
    errorDisplay <- paste(errorMessages[errors], collapse = "/n")
    
    # Prevent submission process if data is not filled out properly ----
    if (length(errorMessages[errors]) != 0){
      showModal(modalDialog(
        h5("Your submission has the following errors:/n"),
        HTML(errorDisplay),
        title = "Invalid Submission",
        easyClose = TRUE,
        footer = NULL,
        fade = TRUE,
        size = "m"
      ))
    }

    else {
      
    # Reset velo_num ----
    updateNumericInput(session,
                       inputId = "velo_num", 
                       label = "What velocity (cm/s) is marked by the scale icon?", 
                       value = 0)
    
    # Reset radio buttons ----
    updateRadioButtons(session, inputId = "can_read",
                       label = "Is the image un-measurable?",
                       choices = c("No" = 0,
                                   "Yes" = 1),
                       selected = 0,
                       inline = TRUE
                       )
    
    updateRadioButtons(session, inputId = "num_beats",
                       label = "How many beats will be measured?",
                       choices = c("1" = 1,
                                   "2" = 2,
                                   "3" = 3),
                       selected = 3, 
                       inline = TRUE
                       )
    
    # Reset metric selection ----
    updateSelectInput(session,
                      inputId = "metric_select", 
                      label = "Select a Metric to Move",
                      choices = c("Baseline" = "bl_select",
                                  "Scale" = "velo_select",
                                  "Peak 1" = "p1_select",
                                  "Trough 1" = "t1_select",
                                  "Peak 2" = "p2_select",
                                  "Trough 2" = "t2_select",
                                  "Peak 3" = "p3_select",
                                  "Trough 3" = "t3_select"),
                      selected = "bl_select"
    ) 

    # Update dataframe with new values ----
    if (input$can_read == 1){
      
      rv$data[rv$seq, ] <- c("image_id" = file_name()[rv$seq], 
                             "date_time_submit" = format(Sys.time(), "%Y_%m_%d_%H%M"),
                             "anesthesiologist_measuring" = input$reader,
                             "image_unmeasurable" = input$can_read,
                             "num_beats" = input$num_beats,
                             "dicrotic_notch" = input$dicrotic,
                             "rounded_envelope" = input$rounded,
                             "flat_diastole" = input$flat_diastole,
                             "baseline" = structures$bl,
                             "scale" = structures$velo,
                             "velo_num" = input$velo_num,
                             "peak_1" = structures$peaks[1],
                             "peak_2" = structures$peaks[2],
                             "peak_3" = structures$peaks[3],
                             "trough_1" = structures$troughs[1],
                             "trough_2" = structures$troughs[2],
                             "trough_3" = structures$troughs[3],
                             "rri_1" = calculateRRI(structures, 1),
                             "rri_2" = calculateRRI(structures, 2),
                             "rri_3" = calculateRRI(structures, 3))
    }
    
    else if (input$can_read == 0){
      
      rv$data[rv$seq, ] <- c("image_id" = file_name()[rv$seq], 
                             "date_time_submit" = format(Sys.time(), "%Y_%m_%d_%H%M"),
                             "anesthesiologist_measuring" = input$reader,
                             "image_unmeasurable" = input$can_read,
                             "num_beats" = NA,
                             "dicrotic_notch" = NA,
                             "rounded_envelope" = NA,
                             "flat_diastole" = NA,
                             "baseline" = structures$bl,
                             "scale" = structures$velo,
                             "velo_num" = NA,
                             "peak_1" = structures$peaks[1],
                             "peak_2" = structures$peaks[2],
                             "peak_3" = structures$peaks[3],
                             "trough_1" = structures$troughs[1],
                             "trough_2" = structures$troughs[2],
                             "trough_3" = structures$troughs[3],
                             "rri_1" = NA,
                             "rri_2" = NA,
                             "rri_3" = NA)
    }
    
    
    if (input$passcode == dbMasterPassword){
      
      db <- dbConnect(RMySQL::MySQL(), dbname = dbName, host = dbHost, 
                      port = dbPort, user = dbUser, 
                      password = dbMasterPassword)
      
      query <-  sprintf(
        "INSERT INTO rri_reads (%s) VALUES ('%s')",
        paste(names(rv$data), collapse = ", "),
        paste(rv$data[rv$seq, ], collapse = "', '")
        )
      
      dbGetQuery(db, query)
      dbDisconnect(db)
      
    }
    
    # Increase seq reactive value by 1 ----
    rv$seq <- rv$seq + 1
  
    # Show submission modal dialog ----
    showModal(modalDialog(
      title = "Data Submitted",
      "Data for this Image Has Been Submitted. Continue by Clicking Outside this Box.",
      easyClose = TRUE,
      footer = NULL,
      fade = TRUE,
      size = "m"
    ))
      
    # Reset zoom slider ----
    updateSliderInput(session, "plot_size", val = 1)
  
    # Reset structure reactive values ----
    structures$bl <- img_dim()[2] - 10
    structures$velo <- img_dim()[2] - 10
    structures$peaks <- c(img_dim()[2] - 10, img_dim()[2] - 10, img_dim()[2] - 30)
    structures$troughs <- rep(img_dim()[2] - 30, 3)
    structures$bl_x <- 50
    structures$velo_x <- 125
    structures$peaks_x <- c(200, 275, 50)
    structures$troughs_x <- c(125, 200, 275)
    structures$click <- 50

    }
  })
  
  # Toggle status of sliders and radiobuttons based on can_read ---
  
  observe({
    
    toggleState(id = "num_beats", condition = input$can_read == 0)
    toggleState(id = "metric_select", condition = input$can_read == 0)
    toggleState(id = "velo_num", condition = input$can_read == 0)
    toggleState(id = "heart_rate", condition = input$can_read == 0)
    toggleState(id = "strip_speed", condition = input$can_read == 0)
    toggleState(id = "dicrotic", condition = input$can_read == 0)
    toggleState(id = "rhythm", condition = input$can_read == 0)
    toggleState(id = "paced", condition = input$can_read == 0)
    toggleState(id = "flat_diastole", condition = input$can_read == 0)
    toggleState(id = "rounded", condition = input$can_read == 0)
    
  })
  
  # Toggle allowable status of inputs ----
  observe({
    
    # Next metric available after image uploaded; deactivate after all are read
    toggleState(id = "next_metric", condition = all(c(!is.null(input$files), 
                                                 rv$seq <= nrow(input$files))))
    
    # Prior metric available after image uploaded; deactivate after all are read
    toggleState(id = "prior_metric", condition = all(c(!is.null(input$files), 
                                                      rv$seq <= nrow(input$files))))
    
    # Submit available after images uploaded; deactivate after all are read
    toggleState(id = "submit", condition = all(c(!is.null(input$files), 
                                                 rv$seq <= nrow(input$files))))
    
    ## Download available after at least one image is read
    toggleState(id = "download_data", condition = all(c(!is.null(input$files),
                                                        rv$seq > 1)))
    
    ## Analyze Image Button available after files uploaded
    toggleState(id = "go_to_read", condition = !is.null(inFile()))
    
    ## Upload images button available only if no files uploaded or done
    toggleState(id = "go_to_entry", condition = (is.null(inFile()) |
                                                 rv$seq > nrow(input$files)))
    
  })
  
  # Move to analysis tab panel with button click ---
  observeEvent(input$go_to_entry, {
    
    updateNavbarPage(session, inputId = "tabs", selected = "Step 1")
    
  })
  
  # Move to upload tab panel with button click ----
  observeEvent(input$go_to_read, {
    
    updateNavbarPage(session, inputId = "tabs", selected = "Step 2")
    
  })
  
  
  # User clicks next metric ----
  observeEvent(input$next_metric, {

    if (input$num_beats == 3){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p1_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p2_select"
        ) 
        
      } else if (input$metric_select == "p2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t2_select"
        ) 
        
      } else if (input$metric_select == "t2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p3_select"
        ) 
        
      } else if (input$metric_select == "p3_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t3_select"
        ) 
        
      } else if (input$metric_select == "t3_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "bl_select"
        ) 
        
      }
      
    } else if (input$num_beats == 2){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "p1_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "p2_select"
        ) 
        
      } else if (input$metric_select == "p2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "t2_select"
        ) 
        
      } else if (input$metric_select == "t2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "bl_select"
        ) 
        
      }
      
    } else if (input$num_beats == 1){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "p1_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "bl_select"
        ) 
        
      } 
      
    }

  })
  
  # User clicks prior metric ----
  
  observeEvent(input$prior_metric, {
    
    if (input$num_beats == 3){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t3_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "bl_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p1_select"
        ) 
        
      } else if (input$metric_select == "p2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "t2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p2_select"
        ) 
        
      } else if (input$metric_select == "p3_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "t2_select"
        ) 
        
      } else if (input$metric_select == "t3_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select",
                                      "Peak 3" = "p3_select",
                                      "Trough 3" = "t3_select"),
                          selected = "p3_select"
        ) 
        
      }
      
    } else if (input$num_beats == 2){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "t2_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "bl_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "p1_select"
        ) 
        
      } else if (input$metric_select == "p2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "t2_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select",
                                      "Peak 2" = "p2_select",
                                      "Trough 2" = "t2_select"),
                          selected = "p2_select"
        ) 
        
      }
      
    } else if (input$num_beats == 1){
      
      if (input$metric_select == "bl_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "t1_select"
        ) 
        
      } else if (input$metric_select == "velo_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "bl_select"
        ) 
        
      } else if (input$metric_select == "p1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "velo_select"
        ) 
        
      } else if (input$metric_select == "t1_select"){
        
        updateSelectInput(session,
                          inputId = "metric_select", 
                          label = "Select a Metric to Move",
                          choices = c("Baseline" = "bl_select",
                                      "Scale" = "velo_select",
                                      "Peak 1" = "p1_select",
                                      "Trough 1" = "t1_select"),
                          selected = "p1_select"
        ) 
        
      } 
      
    }
    
  })
  
  # Define data download output ----
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$reader, "_", format(Sys.time(), "%Y_%m_%d_%H%M"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    })
  
  # Define image dimensions ----
  img_dim <- reactive({
    
    if(is.null(inFile())){
      return(NULL)
    } else if (rv$seq > nrow(inFile())){
      return(NULL)
    } else {
      img <- readJPEG(inFile()$datapath[rv$seq], native = TRUE) 
      x <- dim(img)[2]
      y <- dim(img)[1]
      return(c(x, y))
    }
      
  })
  
  # Update slider max and position based on img_dim ----
  observeEvent(img_dim(), {

    updateSliderInput(session, "nudge", max = img_dim()[2],
                      value = img_dim()[2] - 10)
    
  })
  
  # Image generation ----
  observe({
    
    output$image <- renderPlot({

      
      if (is.null(inFile())){
        
        return(NULL)
        
      } else if (rv$seq > nrow(inFile())){
        
        return(NULL)
        
      } else {
        
        return({
          img <- readJPEG(inFile()$datapath[rv$seq], native = TRUE)
          par(mar = c(0,0,0,0))
          plot(x = seq(0, dim(img)[2], length.out = 1000), 
               y = seq(0, dim(img)[1], length.out = 1000), 
               type='n',
               xlab = "",
               ylab = "",
               axes = FALSE,
               frame.plot = TRUE)
          
          rasterImage(img,0, 0, dim(img)[2], dim(img)[1])
          
          ## Baseline
          segments(x0 = structures$bl_x - 20, x1 = structures$bl_x + 20, y0 = structures$bl, y1 = structures$bl,
                   col = cols[1],
                   lwd = 3)
          
          text(x = structures$bl_x - 35, y = structures$bl, labels = "BL", col = cols[1],
               font = 2)
          
          ## Velo
          segments(x0 = structures$velo_x - 20, x1 = structures$velo_x + 20, y0 = structures$velo, y1 = structures$velo,
                   col = cols[5],
                   lwd = 3)
          
          text(x = structures$velo_x - 35, y = structures$velo, labels = "SC", col = cols[5],
               font = 2)
          
          ## Peak 1
          segments(structures$peaks_x[1] - 20, x1 = structures$peaks_x[1] + 20, y0 = structures$peaks[1], y1 = structures$peaks[1],
                   col = cols[2],
                   lwd = 3)
          
          text(x = structures$peaks_x[1] - 35, y = structures$peaks[1], labels = "P1", col = cols[2],
               font = 2)
          
          ## Peak 2
          segments(structures$peaks_x[2] - 20, x1 = structures$peaks_x[2] + 20, y0 = structures$peaks[2], y1 = structures$peaks[2],
                   col = cols[3],
                   lwd = 3)
          
          text(x = structures$peaks_x[2] - 35, y = structures$peaks[2], labels = "P2", col = cols[3],
               font = 2)
          
          ## Peak 3
          segments(structures$peaks_x[3] - 20, x1 = structures$peaks_x[3] + 20, y0 = structures$peaks[3], y1 = structures$peaks[3],
                   col = cols[4],
                   lwd = 3)
          
          text(x = structures$peaks_x[3] - 35, y = structures$peaks[3], labels = "P3", col = cols[4],
               font = 2)
          
          ## Trough 1
          segments(structures$troughs_x[1] - 20, x1 = structures$troughs_x[1] + 20, y0 = structures$troughs[1], y1 = structures$troughs[1],
                   col = cols[6],
                   lwd = 3)
          
          text(x = structures$troughs_x[1] - 35, y = structures$troughs[1], labels = "T1", col = cols[6],
               font = 2)
          
          ## Trough 2
          segments(structures$troughs_x[2] - 20, x1 = structures$troughs_x[2] + 20, y0 = structures$troughs[2], y1 = structures$troughs[2],
                   col = cols[7],
                   lwd = 3)
          
          text(x = structures$troughs_x[2] - 35, y = structures$troughs[2], labels = "T2", col = cols[7],
               font = 2)
          
          ## Trough 3
          segments(structures$troughs_x[3] - 20, x1 = structures$troughs_x[3] + 20, y0 = structures$troughs[3], y1 = structures$troughs[3],
                   col = cols[8],
                   lwd = 3)
          
          text(x = structures$troughs_x[3] - 35, y = structures$troughs[3], labels = "T3", col = cols[8],
               font = 2)
        })
        
      }
 
  }, width = if(is.null(img_dim())){100} else{img_dim()[1]*input$plot_size},
     height = if(is.null(img_dim())){100} else{img_dim()[2]*input$plot_size})
    
  })
  
  # Select metric - switch position of nudge slider ----
  observeEvent(input$metric_select, {
    
    if (input$metric_select == "bl_select"){

      updateSliderInput(session, "nudge", val = structures$bl)
      
    } else if (input$metric_select == "velo_select"){
      
      updateSliderInput(session, "nudge", val = structures$velo)
      
    } else if (input$metric_select == "p1_select"){
      
      updateSliderInput(session, "nudge", val = structures$peaks[1])
      
    } else if (input$metric_select == "p2_select"){
      
      updateSliderInput(session, "nudge", val = structures$peaks[2])
      
    } else if (input$metric_select == "p3_select"){
      
      updateSliderInput(session, "nudge", val = structures$peaks[3])
      
    } else if (input$metric_select == "t1_select"){
      
      updateSliderInput(session, "nudge", val = structures$troughs[1])
      
    } else if (input$metric_select == "t2_select"){
      
      updateSliderInput(session, "nudge", val = structures$troughs[2])
      
    } else if (input$metric_select == "t3_select"){
      
      updateSliderInput(session, "nudge", val = structures$troughs[3])  
      
    }
    
  })
  
  
  # User clicks on image ----
  observeEvent(input$click, {
    
    updateSliderInput(session, "nudge", val = input$click$y)
    
    # Movement of lines depends on selected radio button
    # Slider values are updated with click
    if (input$metric_select == "bl_select"){
      
      structures$bl <- input$click$y
      structures$bl_x <- input$click$x

    } else if (input$metric_select == "velo_select"){
      
      structures$velo <- input$click$y
      structures$velo_x <- input$click$x
    
    } else if (input$metric_select == "p1_select"){
      
      structures$peaks[1] <- input$click$y
      structures$peaks_x[1] <- input$click$x
        
    } else if (input$metric_select == "p2_select"){
      
      structures$peaks[2] <- input$click$y
      structures$peaks_x[2] <- input$click$x
      
    } else if (input$metric_select == "p3_select"){
      
      structures$peaks[3] <- input$click$y
      structures$peaks_x[3] <- input$click$x

    } else if (input$metric_select == "t1_select"){
      
      structures$troughs[1] <- input$click$y
      structures$troughs_x[1] <- input$click$x

    } else if (input$metric_select == "t2_select"){
      
      structures$troughs[2] <- input$click$y
      structures$troughs_x[2] <- input$click$x

    } else if (input$metric_select == "t3_select"){
      
      structures$troughs[3] <- input$click$y
      structures$troughs_x[3] <- input$click$x

    }
      
  })

  # Update options for metric selection based on beats ----
  # Also update values of data to NA (removes from plot, and makes NA for data)
  observeEvent(input$num_beats, {
    
    if (input$num_beats == 1){
      
      updateSelectInput(session,
                        inputId = "metric_select", 
                        label = "Select a Metric to Move",
                        choices = c("Baseline" = "bl_select",
                                    "Scale" = "velo_select",
                                    "Peak 1" = "p1_select",
                                    "Trough 1" = "t1_select"),
                        selected = "bl_select"
      ) 
      
      structures$peaks[2] <- NA
      structures$peaks[3] <- NA
      structures$troughs[2] <- NA
      structures$troughs[3] <- NA
      
    } else if (input$num_beats == 2){
      
      updateSelectInput(session,
                        inputId = "metric_select", 
                        label = "Select a Metric to Move",
                        choices = c("Baseline" = "bl_select",
                                    "Scale" = "velo_select",
                                    "Peak 1" = "p1_select",
                                    "Peak 2" = "p2_select",
                                    "Trough 1" = "t1_select",
                                    "Trough 2" = "t2_select"),
                        selected = "bl_select"
      ) 
      
      structures$peaks[3] <- NA
      structures$troughs[3] <- NA
      
    } else if (input$num_beats == 3){
      
      updateSelectInput(session,
                        inputId = "metric_select", 
                        label = "Select a Metric to Move",
                        choices = c("Baseline" = "bl_select",
                                    "Scale" = "velo_select",
                                    "Peak 1" = "p1_select",
                                    "Peak 2" = "p2_select",
                                    "Peak 3" = "p3_select",
                                    "Trough 1" = "t1_select",
                                    "Trough 2" = "t2_select",
                                    "Trough 3" = "t3_select"),
                        selected = "bl_select"
      ) 
    }
    
  })
  
  # Set data values to NA if image is unmeasurable ----
  observeEvent(input$can_read, {
    
    if (input$can_read == 1){
      
      structures$bl <- NA
      structures$velo <- NA
      structures$peaks <- rep(NA, 3)
      structures$troughs <- rep(NA, 3)
      
      rv$data[rv$seq, ] <- c("image_id" = file_name()[rv$seq], 
                             "date_time_submit" = format(Sys.time(), "%Y_%m_%d_%H%M"),
                             "anesthesiologist_measuring" = input$reader,
                             "image_unmeasurable" = input$can_read,
                             "num_beats" = NA,
                             "dicrotic_notch" = NA,
                             "rounded_envelope" = NA,
                             "flat_diastole" = NA,
                             "baseline" = structures$bl,
                             "scale" = structures$velo,
                             "velo_num" = input$velo_num,
                             "peak_1" = structures$peaks[1],
                             "peak_2" = structures$peaks[2],
                             "peak_3" = structures$peaks[3],
                             "trough_1" = structures$troughs[1],
                             "trough_2" = structures$troughs[2],
                             "trough_3" = structures$troughs[3],
                             "rri_1" = NA,
                             "rri_2" = NA,
                             "rri_3" = NA)
      
    }
    
  })

  # User moves marker nudge slider ----
  observeEvent(input$nudge, {
    
    ##Movement of lines depends on selected radio button
    # Slider values are updated with click
    if (input$metric_select == "bl_select"){
      
      structures$bl <- input$nudge

    } else if (input$metric_select == "velo_select"){
      
      structures$velo <- input$nudge

    } else if (input$metric_select == "p1_select"){
      
      structures$peaks[1] <- input$nudge

    } else if (input$metric_select == "p2_select"){
      
      structures$peaks[2] <- input$nudge

    } else if (input$metric_select == "p3_select"){
      
      structures$peaks[3] <- input$nudge

    } else if (input$metric_select == "t1_select"){
      
      structures$troughs[1] <- input$nudge

    } else if (input$metric_select == "t2_select"){
      
      structures$troughs[2] <- input$nudge

    } else if (input$metric_select == "t3_select"){
      
      structures$troughs[3] <- input$nudge
      
    }
    
  })
  
  # Conditional Panel for uploaded file ----
  output$fileUploaded <- reactive({
    
    return(!is.null(inFile()) & (!rv$seq > image_total()))
    
  })
  
  # Conditional Panel for action button when finished ----
  output$done <- reactive({
    
    return(!is.null(inFile()) & (rv$seq > image_total()))
    
  })
  
  # Conditional Panel for action button when no files uploaded ----
  output$no_upload <- reactive({
    
    return(is.null(inFile()))
    
  })
  
  # Check passcode for text output ----
  pass <- reactiveValues(x = "<b><i><font color = red>Please enter a passcode.</b></i></font>")
  
  output$pass_text <- renderText(pass$x)

  observeEvent(input$check_pass, {
    
    if (input$passcode == dbMasterPassword){
      pass$x <- "<b><i><font color = red>Passcode verified.<br>Data will be uploaded to the database after each image submission.</b></i></font>"
    } else if (input$passcode != dbMasterPassword){
      pass$x <- "<b><i><font color = red>Incorrect passcode.<br>Data will not be uploaded to the database.</b></i></font>"
    }
  })
  
  # Pull data from database to compare uploaded file IDs ----
  observeEvent(input$files, {
    
    db <- dbConnect(RMySQL::MySQL(), dbname = dbName, host = dbHost, 
                    port = dbPort, user = dbUser, 
                    password = dbMasterPassword)
    
    dbData <- dbGetQuery(db, "SELECT image_id FROM rri_reads")
    
    dbDisconnect(db)
    
    if (any(is.na(as.numeric(file_name())))) {
      
      wrong <- file_name()[is.na(as.numeric(file_name()))]

      title = "Non-Numeric Image ID"
      message = HTML(paste("<b><i><font color = red>The following uploaded ", 
                           "images are named improperly: ", 
                           paste(wrong, collapse = ", "),
                           "<br>Please reupload images with a valid study ",
                           "image ID.</b></i></font>"))
      
      showModal(modalDialog(
        title = "Non-Numeric Image ID",
        message,
        easyClose = FALSE,
        fade = TRUE,
        size = "m"
      ))
      
    } else if (any(as.numeric(file_name()) %in% as.numeric(dbData$image_id))){
    
      dups <- file_name()[as.numeric(file_name()) %in% as.numeric(dbData$image_id)]

      if (length(dups) > 1){
        title <- "Possible Duplicate Images"
        message <- HTML(paste("<b><i><font color = red>The following images already have data ",
                              "in the central database: ",
                              paste(dups, collapse = ", "), 
                              ".</b></i></font>", sep = ""))
      } else {
        title <- "Possible Duplicate Image" 
        message <- HTML(paste("<b><i><font color = red>The following image already has data ",
                              "in the central database: ",
                              paste(dups, collapse = ", "), 
                              ".</b></i></font>", sep = ""))
      }
      
      showModal(modalDialog(
        title = title,
        message,
        easyClose = FALSE,
        fade = TRUE,
        size = "m"
      ))
      
    }
    
  })
  
  # Conditional Panel for download button when read one or more images ----
  output$downloadReady <- reactive({
    
    return(!is.null(inFile()) & (rv$seq > 1))
    
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'done', suspendWhenHidden=FALSE)
  outputOptions(output, 'downloadReady', suspendWhenHidden=FALSE)
  outputOptions(output, 'no_upload', suspendWhenHidden=FALSE)
  
  # Generate url for link to index
  return_link <- a("Return to RRI Dashboard",
                   href="http://ec2-54-208-135-117.compute-1.amazonaws.com:3838")
  
  output$link <- renderUI({return_link})
  output$link_2 <- renderUI({return_link})
  
}

# Run the application ----
shinyApp(ui = ui, server = server)

