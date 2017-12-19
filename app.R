library(shiny)
library(shinyjs)
library(jpeg)

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
             
             mainPanel(
               
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
   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red; border-top-color: red; border-bottom-color: red; border-color: red}")),
   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: blue; border-top-color: blue; border-bottom-color: blue; border-color: blue}")),
   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green; border-top-color: green; border-bottom-color: green; border-color: green}")),
   tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange; border-top-color: orange; border-bottom-color: orange; border-color: orange}")),
   tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: purple; border-top-color: purple; border-bottom-color: purple; border-color: purple}")),
   tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: yellow; border-top-color: yelow; border-bottom-color: yellow; border-color: yellow}")),
   tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: pink; border-top-color: pink; border-bottom-color: pink; border-color: pink}")),
   tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: tan; border-top-color: tan; border-bottom-color: tan; border-color: tan}")),
   
   
   ## Remove Slider Colors
   tags$style(HTML(".js-irs-0 .irs-from, .irs-to, .irs-min, .irs-max, .irs-single {visibility: hidden !important}")),
   
   fluidRow(
     
     column(2,

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
                                 min = 1, max = 640, value = 625,
                                 ticks = FALSE),
                     
                     sliderInput("p1_slider", "Peak 1",
                                 min = 1, max = 640, value = 625,
                                 ticks = FALSE),
                     
                     sliderInput("p3_slider", "Peak 3",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE),
                     
                     sliderInput("t2_slider", "Trough 2",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE)
                     
              ),
              
              column(6, 
                     
                     sliderInput("velo_slider", "Scale",
                                 min = 1, max = 640, value = 625,
                                 ticks = FALSE),
                     
                     sliderInput("p2_slider", "Peak 2",
                                 min = 1, max = 640, value = 625,
                                 ticks = FALSE),
                     
                     sliderInput("t1_slider", "Trough 1",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE),
                     
                     sliderInput("t3_slider", "Trough 3",
                                 min = 1, max = 640, value = 600,
                                 ticks = FALSE)
                     
              )
            ),
            
            
            
            # Horizontal line ----
            tags$hr(),
            
            actionButton(inputId = "submit", label = "Submit Image"),
            
            hr(),
            
            downloadButton(outputId = "download_data", label = "Download Data")
     ),
     
     column(10,
            
            htmlOutput("status"),
            
            # Horizontal line ----
            tags$hr(),
            
            fluidRow(
              column(4,
                     radioButtons("dicrotic", "Is a Dicrotic Notch Present?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 0,
                                  inline = TRUE)
              ),
              column(4,
                     radioButtons("rounded", "Are the Envelopes Rounded?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 0,
                                  inline = TRUE)
              ),
              column(4,
                     radioButtons("flat_diastole", "Is a Diastole Relatively Flat?",
                                  choices = c('No' = 0,
                                              'Yes' = 1),
                                  selected = 1,
                                  inline = TRUE)
              )
            ),
            
            
            # Output: Data file ----
            
            fluidRow(
              column(11,
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       plotOutput(outputId = "image",
                                  click = "click"
                       )
                     )
              )
   
             )
    
    )
   )
    )
  )
)
  
server <- function(input, output, session) {
  
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
                          "trough_3" = NA)
    
    rv$seq <- 1
    
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
      return("<b><i><font color = red>No Images have Been Uploaded.  Please Uploaded Files to Continue.</b></i></font>")
    
    if (rv$seq <= nrow(inFile()))
      return(paste("<b><i><font color = red>Current Image ID: ", file_name()[rv$seq], "<br>Progress: Image ", 
            rv$seq, " of ", image_total(), "</b></i></font>", sep = ""))
    
    if (rv$seq > nrow(inFile()))
      return("<b><i><font color = red>All images have been analyzed.  Please exit the browser, or upload additional images to continue</b></i><font color = red>")
    
  })
  
  ## Define reactive values for image analysis
  structures <- reactiveValues(bl = 625,
                               velo = 625,
                               peak1 = 625,
                               peak2 = 625,
                               peak3 = 600,
                               trough1 = 600,
                               trough2 = 600,
                               trough3 = 600,
                               bl_x = 100,
                               velo_x = 200,
                               peak1_x = 300,
                               peak2_x = 400,
                               peak3_x = 100,
                               trough1_x = 200,
                               trough2_x = 300,
                               trough3_x = 400,
                               click = 50,
                               image_dim = c(NA, NA))
  
  ## Submit button pressed
  observeEvent(input$submit, {
    
    ## Update dataframe with new values
    rv$data[rv$seq, ] <- c("image_id" = file_name()[rv$seq], 
                           "date_time_submit" = format(Sys.time(), "%Y_%m_%d_%H%M"),
                           "anesthesiologist_measuring" = input$reader,
                           "dicrotic_notch" = input$dicrotic,
                           "rounded_envelope" = input$rounded,
                           "flat_diastole" = input$flat_diastole,
                           "baseline" = structures$bl,
                           "scale" = structures$velo,
                           "peak_1" = structures$peak1,
                           "peak_2" = structures$peak2,
                           "peak_3" = structures$peak3,
                           "trough_1" = structures$trough1,
                           "trough_2" = structures$trough2,
                           "trough_3" = structures$trough3)
    
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
   
    ## Reset structure reactive values
    structures$bl <- 625
    structures$velo <- 625
    structures$peak1 <- 625
    structures$peak2 <- 625
    structures$peak3 <- 600
    structures$trough1 <- 600
    structures$trough2 <- 600
    structures$trough3 <- 600
    structures$bl_x <- 100
    structures$velo_x <- 200
    structures$peak1_x <- 300
    structures$peak2_x <- 400
    structures$peak3_x <- 100
    structures$trough1_x <- 200
    structures$trough2_x <- 300
    structures$trough3_x <- 400
    structures$click <- 50
    
    # Reset sliders
    updateSliderInput(session, "bl_slider", val = 625)
    updateSliderInput(session, "velo_slider", val = 625)
    updateSliderInput(session, "p1_slider", val = 625)
    updateSliderInput(session, "p2_slider", val = 625)
    updateSliderInput(session, "p3_slider", val = 600)
    updateSliderInput(session, "t1_slider", val = 600)
    updateSliderInput(session, "t2_slider", val = 600)
    updateSliderInput(session, "t3_slider", val = 600)
     
  })
  
  ## Toggle status of inputs
  observe({
    
    ## Submit available after images uploaded; deactivate after all are read
    toggleState(id = "submit", condition = all(c(!is.null(input$files), 
                                                 rv$seq <= nrow(input$files))))
    
    ## Download available after all images read
    toggleState(id = "download_data", condition = all(c(!is.null(input$files),
                                                        rv$seq > nrow(input$files))))
    
    ## Sliders available after images uploaded; deactivate after all are read
    toggleState(id = "y_pos", condition = all(c(!is.null(input$files),
                                                rv$seq <= nrow(input$files))))
    
    ## Analyze Image Button available after files uploaded
    toggleState(id = "go_to_read", condition = !is.null(inFile()))
    
  })
  
  # Move to analysis tab panel with button click
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
          par(mar = c(5,3,2,2))
          plot(x = seq(0, 720*1.2, length.out = 1000), 
               y = seq(0, 540*1.2, length.out = 1000), 
               type='n',
               xlab = "",
               ylab = "",
               axes = FALSE,
               frame.plot = TRUE)
          
          rasterImage(img,0, 0, 720*1.2, 540*1.2)
          
          ## Baseline
          segments(x0 = structures$bl_x - 50, x1 = structures$bl_x + 50, y0 = structures$bl, y1 = structures$bl,
                   col = "red",
                   lwd = 3)
          
          ## Velo
          segments(x0 = structures$velo_x - 50, x1 = structures$velo_x + 50, y0 = structures$velo, y1 = structures$velo,
                   col = "purple",
                   lwd = 3)
          
          ## Peak 1
          segments(structures$peak1_x - 50, x1 = structures$peak1_x + 50, y0 = structures$peak1, y1 = structures$peak1,
                   col = "blue",
                   lwd = 3)
          
          ## Peak 2
          segments(structures$peak2_x - 50, x1 = structures$peak2_x + 50, y0 = structures$peak2, y1 = structures$peak2,
                   col = "yellow",
                   lwd = 3)
          
          ## Peak 3
          segments(structures$peak3_x - 50, x1 = structures$peak3_x + 50, y0 = structures$peak3, y1 = structures$peak3,
                   col = "green",
                   lwd = 3)
          
          ## Trough 1
          segments(structures$trough1_x - 50, x1 = structures$trough1_x + 50, y0 = structures$trough1, y1 = structures$trough1,
                   col = "pink",
                   lwd = 3)
          
          ## Trough 2
          segments(structures$trough2_x - 50, x1 = structures$trough2_x + 50, y0 = structures$trough2, y1 = structures$trough2,
                   col = "orange",
                   lwd = 3)
          
          ## Trough 3
          segments(structures$trough3_x - 50, x1 = structures$trough3_x + 50, y0 = structures$trough3, y1 = structures$trough3,
                   col = "tan",
                   lwd = 3)
        })
        
      }
 
  }, width = if(is.null(inFile())){100} else if(rv$seq > nrow(inFile())){100} else{dim(readJPEG(inFile()$datapath[rv$seq]))[2]},
     height = if(is.null(inFile())){100} else if(rv$seq > nrow(inFile())){100} else{dim(readJPEG(inFile()$datapath[rv$seq]))[1]})
    
  })
  
  ## User clicks on image
  observeEvent(input$click, {
    
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

  ## Conditional Panel for uploaded file

  output$fileUploaded <- reactive({
    
    return(!is.null(inFile()) & (!rv$seq > image_total()))
    
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
}


# Run the application 
shinyApp(ui = ui, server = server)

