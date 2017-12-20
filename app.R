library(shiny)
library(shinyjs)
library(jpeg)

cols <- c("#FF0000", "#000CFF", "#00FF28", "#F7FF00",
          "#FF6900", "#FF00EB", "#00F7FF", "#8C174C",
          "#000000", "#8B888A")

ui <- shinyUI(
  navbarPage(
    "Renal Resistive Index Analysis", id = "tabs",
    
    tabPanel("Step 1",
             
             tags$head(tags$style(
               HTML('body, label, input, button, select {
                    font-family: "Avenir";
                    font-size:10px;
                    }')
               )
               ),
             
             fluidRow(column(12,
                             
                             h5("Welcome to the renal resistive index analysis platform.
                                Please enter your information and upload images below.")
                             
                             )
                      ),
             
             hr(),
             
             sidebarPanel(
                             # Input: Select a file ----
                             fileInput(inputId = "files", 
                                       label = "Select Images to Analyze",
                                       multiple = TRUE,
                                       accept = c("image/jpeg")),
                             
                             # Input: Select reader ----
                             radioButtons("reader", "Anesthesiologist Reading",
                                          choices = c('Anne Cherry, MD' = "ac",
                                                      'Mark Stafford-Smith, MD' = "mss"),
                                          selected = "ac"),
                             
                             ## Button to advance to next tab
                             actionButton(inputId = "go_to_read",
                                          label = "Analyze Images")
                             
                             ),
             
             mainPanel(
               
               imageOutput("logo")
               
             )
             
             ),
    
    tabPanel("Step 2", id = "analysis_tab",
             
             useShinyjs(),
             
             tags$head(tags$style(
               HTML('body, label, input, button, select { 
                    font-family: "Avenir";
                    font-size:10px;
                    }')
   )),
   
   ## Slider Colors
   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FF0000; border-top-color: #FF0000; border-bottom-color: #FF0000; border-color: #FF0000}")),
   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000CFF; border-top-color: #000CFF; border-bottom-color: #000CFF; border-color: #000CFF}")),
   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #00FF28; border-top-color: #00FF28; border-bottom-color: #00FF28; border-color: #00FF28}")),
   tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #F7FF00; border-top-color: #F7FF00; border-bottom-color: #F7FF00; border-color: #F7FF00}")),
   tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #FF6900; border-top-color: #FF6900; border-bottom-color: #FF6900; border-color: #FF6900}")),
   tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #FF00EB; border-top-color: yelow; border-bottom-color: #FF00EB; border-color: #FF00EB}")),
   tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #00F7FF; border-top-color: #00F7FF; border-bottom-color: #00F7FF; border-color: #00F7FF}")),
   tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #8C174C; border-top-color: #8C174C; border-bottom-color: #8C174C; border-color: #8C174C}")),
   tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #000000; border-top-color: #000000; border-bottom-color: #000000; border-color: #000000}")),
   tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: #8B888A; border-top-color: #8B888A; border-bottom-color: #8B888A; border-color: #8B888A}")),
   
   
   ## Remove Slider Numbers
   tags$style(HTML(".js-irs-0 .irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important}")),
   
   sidebarPanel(width = 3,
                
            ## Image measurable?
            radioButtons(inputId = "can_read",
                         label = "Is the image measurable?",
                         choices = c("Yes" = 0,
                                     "No" = 1),
                         selected = 0,
                         inline = TRUE),
                
            ## Define how many beats will be measured
            radioButtons(inputId = "num_beats",
                         label = "How many beats will be measured?",
                         choices = c("1" = 1,
                                     "2" = 2,
                                     "3" = 3),
                         selected = 3, 
                         inline = TRUE),
            
            ## Baseline value
            
            numericInput(inputId = "velo_num", 
                         label = "What velocity (cm/s) is marked by the scale icon?", 
                         value = NA),

            ## Slider on/off boxes
            selectInput(inputId = "metric_select", 
                        label = "Select a Metric to Move",
                        choices = c("Baseline" = "bl_select",
                                    "Scale" = "velo_select",
                                    "Peak 1" = "p1_select",
                                    "Peak 2" = "p2_select",
                                    "Peak 3" = "p3_select",
                                    "Trough 1" = "t1_select",
                                    "Trough 2" = "t2_select",
                                    "Trough 3" = "t3_select"),
                        selected = "bl_select"),
            
            ## Sliders
            fluidRow(
              column(6,
                     sliderInput("bl_slider", "Baseline",
                                 min = 1, max = 640, value = 525,
                                 ticks = FALSE),
                     
                     sliderInput("p1_slider", "Peak 1",
                                 min = 1, max = 640, value = 525,
                                 ticks = FALSE),
                     
                     sliderInput("p2_slider", "Peak 2",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE),
                     
                     sliderInput("p3_slider", "Peak 3",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE)
                     
              ),
              
              column(6, 
                     
                     sliderInput("velo_slider", "Scale",
                                 min = 1, max = 640, value = 525,
                                 ticks = FALSE),
                     
                     sliderInput("t1_slider", "Trough 1",
                                 min = 1, max = 640, value = 525,
                                 ticks = FALSE),
                     
                     sliderInput("t2_slider", "Trough 2",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE),
                     
                     sliderInput("t3_slider", "Trough 3",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE)
                     
              )
            ),
            
            
            
            # Horizontal line ----
            tags$hr(),
            
            actionButton(inputId = "submit", label = "Submit Image")
     ),
   
   mainPanel(width = 9,
     
     column(12,
            
            htmlOutput("status"),
            
            # Conditional action button to return to tab step 1 after finished
            fluidRow(
              
              column(2,
        
            conditionalPanel(
              condition = "output.downloadReady",
              br(),
              downloadButton(outputId = "download_data",
                             label = "Download Data")
              
            )
            ),
            
            column(2,
            conditionalPanel(
              condition = "output.done",
              offset = 0, 
              br(),
              actionButton(inputId = "go_to_entry", 
                           label = "Upload Files")
              
              
            ))),
            
            # Horizontal line ----
            tags$hr(),
            
            fluidRow(
              column(4,
                     radioButtons("dicrotic", "Is a Dicrotic Notch Present?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 0,
                                  inline = TRUE),
                     br()
              ),
              column(4,
                     radioButtons("rounded", "Are the Envelopes Rounded?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 0,
                                  inline = TRUE),
                     br()
              ),
              column(4,
                     radioButtons("flat_diastole", "Is a Diastole Relatively Flat?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 1,
                                  inline = TRUE),
                     br()
              )
            ),
            
            
            # Output: Data file ----
            
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
              
              column(12,
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       hr(),
                       column(6,
                              sliderInput(inputId = "plot_size", label = "Plot Zoom",
                                   min = 0.01, max = 2, value = 1, step = 0.01,
                                   ticks = FALSE)),
                       column(6,
                              sliderInput(inputId = "nudge", label = "Adjust Marker",
                                          min = 0, max = 1000, value = 500,
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
  
  ## Logo output
  
  output$logo <- renderImage({
    
    list(src = "DUSOM_anesthesiology.jpg",
         width = "95%")
    
  }, deleteFile = FALSE)
  
  # Basic Reactive Values
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
                                         "trough_3" = NA))
  
  ## User inputs new files
  ## Reset data frame
  ## Reset seq reactive value
  
  observeEvent(input$files, {
    
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
                          "trough_3" = NA)
    
    rv$seq <- 1
    
    ## Reset structure reactive values
    structures$bl <- img_dim()[2] - 10
    structures$velo <- img_dim()[2] - 10
    structures$peak1 <- img_dim()[2] - 10
    structures$peak2 <- img_dim()[2] - 10
    structures$peak3 <- img_dim()[2] - 20
    structures$trough1 <- img_dim()[2] - 20
    structures$trough2 <- img_dim()[2] - 20
    structures$trough3 <- img_dim()[2] - 20
    structures$bl_x <- 50
    structures$velo_x <- 100
    structures$peak1_x <- 150
    structures$peak2_x <- 200
    structures$peak3_x <- 50
    structures$trough1_x <- 100
    structures$trough2_x <- 150
    structures$trough3_x <- 200
    structures$click <- 50
    
  })
  
  ## Define input files
  
  inFile <- reactive({
    
    if (is.null(input$files))
      return(NULL)
    
    return(input$files)
    
  })
  
  ## Calculate total number of images
  image_total <- reactive({

    if (is.null(inFile()))
      return(0)
    
    return(nrow(inFile()))
  })
  
  ## Grab File Name to Define Image ID
  
  file_name <- reactive({
  
    if (is.null(inFile()))
      return(NULL)
    
    return(stringi::stri_extract_first(str = inFile()$name, regex = ".*(?=\\.)"))
  })
  
  ## Status Display
  
  output$status <- renderText({
    
    if (is.null(inFile()))
      return("<b><i><font color = red>No images have been uploaded.  Please click below to enter your information and upload files.</b></i></font>")
    
    if (rv$seq <= nrow(inFile()))
      return(paste("<b><i><font color = red>Current Image ID: ", file_name()[rv$seq], "<br>Progress: Image ", 
            rv$seq, " of ", image_total(), "</b></i></font>", sep = ""))
    
    if (rv$seq > nrow(inFile()))
      return("<b><i><font color = red>All images have been analyzed.  Please download the data file by clicking below prior to uploading additional images.</b></i><font color = red>")
    
  })
  
  ## Define reactive values for image analysis
  structures <- reactiveValues(bl = NA,
                               velo = NA,
                               peak1 = NA,
                               peak2 = NA,
                               peak3 = NA,
                               trough1 = NA,
                               trough2 = NA,
                               trough3 = NA,
                               bl_x = 50,
                               velo_x = 100,
                               peak1_x = 150,
                               peak2_x = 200,
                               peak3_x = 50,
                               trough1_x = 100,
                               trough2_x = 150,
                               trough3_x = 200,
                               click = 50,
                               image_dim = c(NA, NA))

  
  ## Submit button pressed
  observeEvent(input$submit, {
    
    ## Reset velo_num
    
    updateNumericInput(session,
                       inputId = "velo_num", 
                       label = "What velocity (cm/s) is marked by the scale icon?", 
                       value = NA)
    
    ## Reset radio buttons
    
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
    
    ## Reset metric selection
    
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
    
    
    ## Update dataframe with new values pending image measurable
    
    if (input$can_read == 0){
      
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
                           "peak_1" = structures$peak1,
                           "peak_2" = structures$peak2,
                           "peak_3" = structures$peak3,
                           "trough_1" = structures$trough1,
                           "trough_2" = structures$trough2,
                           "trough_3" = structures$trough3)
    
    } else if (input$can_read == 1){
      
      rv$data[rv$seq, ] <- c("image_id" = file_name()[rv$seq], 
                             "date_time_submit" = format(Sys.time(), "%Y_%m_%d_%H%M"),
                             "anesthesiologist_measuring" = input$reader,
                             "image_unmeasurable" = input$can_read,
                             "num_beats" = input$num_beats,
                             "dicrotic_notch" = NA,
                             "rounded_envelope" = NA,
                             "flat_diastole" = NA,
                             "baseline" = structures$bl,
                             "scale" = structures$velo,
                             "velo_num" = input$velo_num,
                             "peak_1" = structures$peak1,
                             "peak_2" = structures$peak2,
                             "peak_3" = structures$peak3,
                             "trough_1" = structures$trough1,
                             "trough_2" = structures$trough2,
                             "trough_3" = structures$trough3)
      
    }
    
    ## Increase seq reactive value by 1
    rv$seq <- rv$seq + 1
  
    ## Show pop-up message box
    showModal(modalDialog(
      title = "Data Submitted",
      "Data for this Image Has Been Submitted. Continue by Clicking Outside this Box.",
      easyClose = TRUE,
      footer = NULL,
      fade = TRUE,
      size = "m"
    ))
      
    ## Reset zoom slider
    updateSliderInput(session, "plot_size", val = 1)
  
    ## Reset structure reactive values
    structures$bl <- img_dim()[2] - 10
    structures$velo <- img_dim()[2] - 10
    structures$peak1 <- img_dim()[2] - 10
    structures$peak2 <- img_dim()[2] - 10
    structures$peak3 <- img_dim()[2] - 20
    structures$trough1 <- img_dim()[2] - 20
    structures$trough2 <- img_dim()[2] - 20
    structures$trough3 <- img_dim()[2] - 20
    structures$bl_x <- 50
    structures$velo_x <- 100
    structures$peak1_x <- 150
    structures$peak2_x <- 200
    structures$peak3_x <- 50
    structures$trough1_x <- 100
    structures$trough2_x <- 150
    structures$trough3_x <- 200
    structures$click <- 50

  })
  
  ## Toggle status of sliders and radiobuttons based on can_read
  
  observe({
    
    toggleState(id = "num_beats", condition = input$can_read == 0)
    
    toggleState(id = "metric_select", condition = input$can_read == 0)
    
  })
  
  ## Toggle status of inputs
  observe({
    
    ## Submit available after images uploaded; deactivate after all are read
    toggleState(id = "submit", condition = all(c(!is.null(input$files), 
                                                 rv$seq <= nrow(input$files))))
    
    ## Download available after at least one image is read
    toggleState(id = "download_data", condition = all(c(!is.null(input$files),
                                                        rv$seq > 1)))
    
    ## Analyze Image Button available after files uploaded
    toggleState(id = "go_to_read", condition = !is.null(inFile()))
    
  })
  
  # Move to analysis tab panel with button click
  observeEvent(input$go_to_entry, {
    
    updateNavbarPage(session, inputId = "tabs", selected = "Step 1")
    
  })
  
  # Move to upload tab panel with button click
  observeEvent(input$go_to_read, {
    
    updateNavbarPage(session, inputId = "tabs", selected = "Step 2")
    
  })
  
  ## Define data download output
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$reader, "_", format(Sys.time(), "%Y_%m_%d_%H%M"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    })
  
  ## Define image dimensions
  
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
  
  ## Update slider max and position based on img_dim
  
  observeEvent(img_dim(), {

    updateSliderInput(session, "nudge", max = img_dim()[2],
                      value = img_dim()[2] - 10)
    updateSliderInput(session, "velo_slider", max = img_dim()[2],
                      value = img_dim()[2] - 10)
    updateSliderInput(session, "p1_slider", max = img_dim()[2],
                      value = img_dim()[2] - 10)
    updateSliderInput(session, "p2_slider", max = img_dim()[2],
                      value = img_dim()[2] - 10)
    updateSliderInput(session, "p3_slider", max = img_dim()[2],
                      value = img_dim()[2] - 20)
    updateSliderInput(session, "t1_slider", max = img_dim()[2],
                      value = img_dim()[2] - 20)
    updateSliderInput(session, "t2_slider", max = img_dim()[2],
                      value = img_dim()[2] - 20)
    updateSliderInput(session, "t3_slider", max = img_dim()[2],
                      value = img_dim()[2] - 20)
    updateSliderInput(session, "bl_slider", max = img_dim()[2],
                      value = img_dim()[2] - 10)
  
  })
  
  ## Image generation
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
          
          ## Velo
          segments(x0 = structures$velo_x - 20, x1 = structures$velo_x + 20, y0 = structures$velo, y1 = structures$velo,
                   col = cols[5],
                   lwd = 3)
          
          ## Peak 1
          segments(structures$peak1_x - 20, x1 = structures$peak1_x + 20, y0 = structures$peak1, y1 = structures$peak1,
                   col = cols[2],
                   lwd = 3)
          
          ## Peak 2
          segments(structures$peak2_x - 20, x1 = structures$peak2_x + 20, y0 = structures$peak2, y1 = structures$peak2,
                   col = cols[3],
                   lwd = 3)
          
          ## Peak 3
          segments(structures$peak3_x - 20, x1 = structures$peak3_x + 20, y0 = structures$peak3, y1 = structures$peak3,
                   col = cols[4],
                   lwd = 3)
          
          ## Trough 1
          segments(structures$trough1_x - 20, x1 = structures$trough1_x + 20, y0 = structures$trough1, y1 = structures$trough1,
                   col = cols[6],
                   lwd = 3)
          
          ## Trough 2
          segments(structures$trough2_x - 20, x1 = structures$trough2_x + 20, y0 = structures$trough2, y1 = structures$trough2,
                   col = cols[7],
                   lwd = 3)
          
          ## Trough 3
          segments(structures$trough3_x - 20, x1 = structures$trough3_x + 20, y0 = structures$trough3, y1 = structures$trough3,
                   col = cols[8],
                   lwd = 3)
        })
        
      }
 
  }, width = if(is.null(img_dim())){100} else{img_dim()[1]*input$plot_size},
     height = if(is.null(img_dim())){100} else{img_dim()[2]*input$plot_size})
    
  })
  
  ## Select metric - switch position of nudge slider
  observeEvent(input$metric_select, {
    
    if (input$metric_select == "bl_select"){

      updateSliderInput(session, "nudge", val = input$bl_slider)
      
    } else if (input$metric_select == "velo_select"){
      
      updateSliderInput(session, "nudge", val = input$velo_slider)
      
    } else if (input$metric_select == "p1_select"){
      
      updateSliderInput(session, "nudge", val = input$p1_slider)
      
    } else if (input$metric_select == "p2_select"){
      
      updateSliderInput(session, "nudge", val = input$p2_slider)
      
    } else if (input$metric_select == "p3_select"){
      
      updateSliderInput(session, "nudge", val = input$p3_slider)
      
    } else if (input$metric_select == "t1_select"){
      
      updateSliderInput(session, "nudge", val = input$t1_slider)
      
    } else if (input$metric_select == "t2_select"){
      
      updateSliderInput(session, "nudge", val = input$t2_slider)
      
    } else if (input$metric_select == "t3_select"){
      
      updateSliderInput(session, "nudge", val = input$t3_slider)  
      
    }
    
  })
  
  
  ## User clicks on image
  observeEvent(input$click, {
    
    updateSliderInput(session, "nudge", val = input$click$y)
    
    ## Movement of lines depends on selected radio button
    ## Slider values are updated with click
    if (input$metric_select == "bl_select"){
      
      structures$bl <- input$click$y
      structures$bl_x <- input$click$x
      updateSliderInput(session, "bl_slider", val = input$click$y)

    } else if (input$metric_select == "velo_select"){
      
      structures$velo <- input$click$y
      structures$velo_x <- input$click$x
      updateSliderInput(session, "velo_slider", val = input$click$y)
    
    } else if (input$metric_select == "p1_select"){
      
      structures$peak1 <- input$click$y
      structures$peak1_x <- input$click$x
      updateSliderInput(session, "p1_slider", val = input$click$y)
        
    } else if (input$metric_select == "p2_select"){
      
      structures$peak2 <- input$click$y
      structures$peak2_x <- input$click$x
      updateSliderInput(session, "p2_slider", val = input$click$y)
      
    } else if (input$metric_select == "p3_select"){
      
      structures$peak3 <- input$click$y
      structures$peak3_x <- input$click$x
      updateSliderInput(session, "p3_slider", val = input$click$y)
      
    } else if (input$metric_select == "t1_select"){
      
      structures$trough1 <- input$click$y
      structures$trough1_x <- input$click$x
      updateSliderInput(session, "t1_slider", val = input$click$y)
      
    } else if (input$metric_select == "t2_select"){
      
      structures$trough2 <- input$click$y
      structures$trough2_x <- input$click$x
      updateSliderInput(session, "t2_slider", val = input$click$y)
      
    } else if (input$metric_select == "t3_select"){
      
      structures$trough3 <- input$click$y
      structures$trough3_x <- input$click$x
      updateSliderInput(session, "t3_slider", val = input$click$y)  
      
    }
      
  })

  ## Update options for metric selection based on beats
  ## Also update values of data to NA (removes from plot, and makes NA for data)
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
      
      structures$peak2 <- NA
      structures$peak3 <- NA
      structures$trough2 <- NA
      structures$trough3 <- NA
      
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
      
      structures$peak3 <- NA
      structures$trough3 <- NA
      
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
  
  ## Set data values to NA if image is unmeasurable
  
  observeEvent(input$can_read, {
    
    if (input$can_read == 1){
      
      structures$bl <- NA
      structures$velo <- NA
      structures$peak1 <- NA
      structures$peak2 <- NA
      structures$peak3 <- NA
      structures$trough1 <- NA
      structures$trough2 <- NA
      structures$trough3 <- NA
      
    }
    
  })
  
  ## Toggle state of sliders based on number of beats and measureable image
  
  observe ({
    
    toggleState("p2_slider", condition = (input$num_beats %in% c(2, 3) &
                                          input$can_read == 0))
    
    toggleState("t2_slider", condition = (input$num_beats %in% c(2, 3) & 
                                          input$can_read == 0))
    
    toggleState("p3_slider", condition = (input$num_beats == 3 & 
                                          input$can_read == 0))
    toggleState("t3_slider", condition = (input$num_beats == 3 & 
                                          input$can_read == 0))
    
    toggleState("bl_slider", condition = input$can_read == 0)
    toggleState("velo_slider", condition = input$can_read == 0)
    toggleState("p1_slider", condition = input$can_read == 0)
    toggleState("t1_slider", condition = input$can_read == 0)
    
  })
  
  ## User moves sliders
  ## Line moves based on slider choice
  ## Radio button updates
  observeEvent(input$p1_slider, {
    
    structures$peak1 <- input$p1_slider
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
                      selected = "p1_select"
    )
    
  })
  
  observeEvent(input$p2_slider, {
    
    structures$peak2 <- input$p2_slider
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
                      selected = "p2_select"
    )
    
  })
  
  observeEvent(input$p3_slider, {
    
    structures$peak3 <- input$p3_slider
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
                      selected = "p3_select"
    )
    
  })
  
  
  observeEvent(input$t1_slider, {
    
    structures$trough1 <- input$t1_slider
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
                      selected = "t1_select"
    )
    
  })
  
  observeEvent(input$t2_slider, {
    
    structures$trough2 <- input$t2_slider
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
                      selected = "t2_select"
    )
    
  })
  
  observeEvent(input$t3_slider, {
    
    structures$trough3 <- input$t3_slider
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
                      selected = "t3_select"
    )
    
  })
  
  observeEvent(input$velo_slider, {
    
    structures$velo <- input$velo_slider
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
                      selected = "velo_select"
    )
    
  })
  
  observeEvent(input$bl_slider, {
    
    structures$bl <- input$bl_slider
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
    
  })

  ## User moves marker slider
  
  observeEvent(input$nudge, {
    
    ## Movement of lines depends on selected radio button
    ## Slider values are updated with click
    if (input$metric_select == "bl_select"){
      
      structures$bl <- input$nudge
      updateSliderInput(session, "bl_slider", val = input$nudge)
      
    } else if (input$metric_select == "velo_select"){
      
      structures$velo <- input$nudge
      updateSliderInput(session, "velo_slider", val = input$nudge)
      
    } else if (input$metric_select == "p1_select"){
      
      structures$peak1 <- input$nudge
      updateSliderInput(session, "p1_slider", val = input$nudge)
      
    } else if (input$metric_select == "p2_select"){
      
      structures$peak2 <- input$nudge
      updateSliderInput(session, "p2_slider", val = input$nudge)
      
    } else if (input$metric_select == "p3_select"){
      
      structures$peak3 <- input$nudge
      updateSliderInput(session, "p3_slider", val = input$nudge)
      
    } else if (input$metric_select == "t1_select"){
      
      structures$trough1 <- input$nudge
      updateSliderInput(session, "t1_slider", val = input$nudge)
      
    } else if (input$metric_select == "t2_select"){
      
      structures$trough2 <- input$nudge
      updateSliderInput(session, "t2_slider", val = input$nudge)
      
    } else if (input$metric_select == "t3_select"){
      
      structures$trough3 <- input$nudge
      updateSliderInput(session, "t3_slider", val = input$nudge) 
      
    }
    
  })
  
  ## Conditional Panel for uploaded file

  output$fileUploaded <- reactive({
    
    return(!is.null(inFile()) & (!rv$seq > image_total()))
    
  })
  
  ## Conditional Panel for action button when finished
  
  output$done <- reactive({
    
    return(!is.null(inFile()) & (rv$seq > image_total()))
    
  })
  
  ## Conditional Panel for download button when read one+ images
  
  output$downloadReady <- reactive({
    
    return(!is.null(inFile()) & (rv$seq > 1))
    
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'done', suspendWhenHidden=FALSE)
  outputOptions(output, 'downloadReady', suspendWhenHidden=FALSE)
  
  
}






# Run the application 
shinyApp(ui = ui, server = server)

