# Scleroderma Web Application
# Zachary Barnes, Peter Schulam, Suchi Saria, PhD.
# Dr. Suchi Saria's Lab @ Johns Hopkins University

# Code to define server operations as per user requests for web application
# Handles all model and database calls

# Using shiny for easy creation of web application in R (see http://shiny.rstudio.com)
library(shiny)
source("database.R")
source("nips_model.R")
library(shinyBS)
library(ggplot2)
#require(gridSVG)
require(xtable)
#library(ggvis)

shinyServer(function(input, output, session) {

  toggleModal <- function(modalId, toggle = "toggle") {
    session$sendInputMessage(modalId, list(toggle = toggle)) 
  }

  #toggleModal("welcomePage")

  #Initialize all values for render
  #numSub <- 2
  set <- NULL
  observeEvent(input$numSubSlider, {
      numSub <- input$numSubSlider
   })

  observeEvent(input$yearsEstSlider, {
      yearEst <- input$yearsEstSlider
   })

  input_size <- reactive(input$size)

  #xLim <- c(0,25)
  observeEvent(input$xLimSlider, {
      xLim <- input$xLimSlider
   })

  #yLim <- c(0,125)
  observeEvent(input$yLimSlider, {
      yLim <- input$yLimSlider
   })

  patientModel <- reactive({
    #input$subButton
    input$refresh
    if(!is.numeric(input$patientID))
      return
    # print(input$numSubSlider)
    # print(input$yearsEstSlider)
    # print(input$xLimSlider)
    # print(input$yLimSlider)

    xLim <- c(0,25)
    yLim <- c(0,125)
    numSub <- 2
    yearEst <- 15

    if(is.null(input$numSubSlider) | is.null(input$yearsEstSlider) | is.null(input$xLimSlider) | is.null(input$yLimSlider))
      plot<-predict_traj(data$search_model(input$patientID), yearEst, numSub, xLim, yLim, input$modelPoints, input$togMass, input$minMass)
    
#     t <- patientData()$t
#     y <- patientData()$y
    #predict_traj(data$searchNips(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints)
    #predict_traj(data$searchNips(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints, input$togMass, input$minMass)
    #plot<-predict_traj(data$search_model(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints, input$togMass, input$minMass)
    else
      plot<-predict_traj(data$search_model(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints, input$togMass, input$minMass)
    

    #plot<-predict_traj(data$search_model(input$patientID), years, subs, xlm,input$yLimSlider, input$modelPoints, input$togMass, input$minMass)
    
    if(input$toggle_meds)
      plot <- show_meds(plot,data$getMeds(input$patientID))

    return(plot)
    })

  observeEvent(input$nextPatient,{
    v<-input$dropID
    val <- as.numeric(input$seltd)

    if(val== 1)
      set <- as.vector(idData()$ptid)
    else if(val == 2)
      set<-as.numeric(strsplit(input$idSet, ",")[[1]])

    ind <- match(v,set)
    #print(v)
    #print(set)
    #print(ind)
    if(ind < length(set))
      ind <- ind + 1

    updateSelectInput(session,"dropID",selected = set[ind])
    #patientModel()
    })



  lapply(1:20, function(i) {
    output[[paste0('p', i)]] <- renderUI({
      if(i<=input$new_pfvc_num){
        #strong(paste0('Hi, this is output of ', i))
        tags$span(
          tags$input(type="text",id=paste0("time_",i),style="width:35%;",placeholder=paste0("Time ",i)),
          tags$input(type="text",id=paste0("pfvc_",i),style="width:35%;",placeholder=paste0("PFVC ",i))
          ,style="width:100%;")
      }
    })
  })


  observeEvent(input$plot_click,{
      session$sendCustomMessage(type = "removeDBLCLICK",TRUE)
      clk <- input$plot_click

      if(!is.null(clk)){

        d <- data$search_model(input$patientID)
        points <- data.frame(x=d$x, y=d$y, bias = d$bias)

        dist=sqrt((clk$x-points$x)^2+(clk$y-points$y)^2)
          if(min(dist) < 1){
            updateNumericInput(session, "modelPoints", value = which.min(dist))
          }

      }


    })

  output$dbl_clk_info <- renderUI({
    #clk <- input$plot_click
    dbl <- input$plot_dblclick
      if(!is.null(dbl)){
        # dbl$x_perc <- (dbl$x-.02)*1.12485
        # dbl$x <- dbl$x_perc*25
        # dbl$y_perc <- (dbl$y-.0329)*1.058166
        # dbl$y <- dbl$y_perc*125
        # height <- session$clientData$output_modelGraphics_height * input$plot_dblclick$y + 50
        # width <- session$clientData$output_modelGraphics_width * input$plot_dblclick$x*.93 + 65

        height <- isolate(input$mouse_y)
        width <- isolate(input$mouse_x)

        sty <- paste0("left:", width ,"px;", "top:", height ,"px;
                            padding:5px;
                            width: 165px;
                            display:block; 
                            position:fixed; 
                            overflow:hidden; 
                            z-index: 999;
                            background-color:#d6d6d6;
                            opacity:0.8;"
                )

        div(style= sty, id = "new_point_box",
            tags$p(style="font-size: 1.1em; font-weight: bold;width:100%;", "Enter New PFVC Point:"),
            tags$form(
              tags$span("Enter Time: ",style=""),tags$input(type="text",id="new_point_time",style="width:80px",placeholder="Time",value=sprintf("%1.2f",dbl$x)),tags$br(),tags$br(),
              tags$span("Enter PFVC: ",style=""),tags$input(type="text",id="new_point_pfvc",style="width:80px",placeholder="PFVC",value=sprintf("%1.2f",dbl$y))
              ),
            tags$br(),
          tags$button(id = "new_point_button", type = "action", class = "btn btn-primary action-button", style = "font-size: .9em; font-weight: bold;width:100%;", "Add Point")
          )

      }

    })



observeEvent(input$new_point_button,{
  date_pattern <- '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
  time <- input$new_point_time
  if(grepl(date_pattern, input$new_point_time)){
    d <- patientData()
    val <- paste(as.numeric(as.Date(input$new_point_time))/365 - d$bias[1])
    #updateTextInput(session,"new_point_time", val)
   time <- val
  }
  data$add_point(input$patientID, time, input$new_point_pfvc)
  session$sendCustomMessage(type = "removeDBLCLICK",TRUE)
  #updateSelectInput(session,"dropID",choices = idData(), selected =input$patientID)
  session$sendCustomMessage(type = "refresh",TRUE)
  })





  output$hover_info <- renderUI({
     #d <- data$search_model(input$patientID)
      d <- patientData()
     points <- data.frame(x=d$x, y=d$y, bias = d$bias)
      hv <- input$plot_hover
      if(!is.null(hv)){

      # hv$x_perc <- (hv$x-.02)*1.12485
      # hv$x <- hv$x_perc*25
      # hv$y_perc <- (hv$y-.0329)*1.058166
      # hv$y <- hv$y_perc*125
      # height <- session$clientData$output_modelGraphics_height * input$plot_hover$y
      # width <- session$clientData$output_modelGraphics_width * input$plot_hover$x*.93 + 65
      height <- isolate(input$mouse_y)
      width <- isolate(input$mouse_x)


      dist=sqrt((hv$x-points$x)^2+(hv$y-points$y)^2)
            if(min(dist) < 1){
              p <-points[which.min(dist),]
              sty <- paste0("left:", width ,"px;", "top:", height ,"px;
                            border: 1px solid black; 
                            display:block; 
                            position:fixed; 
                            overflow:hidden; 
                            z-index: 999;
                            background-color:#fff;
                            opacity:0.8;"
                )

              
              date <- as.Date((p$x + p$bias)*365,origin="1970-01-01")

              time_text <- paste0("Time: ", sprintf("%1.2f",p$x), " years")

              pfvc_text <- paste0("PFVC: ", sprintf("%1.2f", p$y))

              date_text <- paste0("Date: ", as.character(date))

              text_style <- "font-weight: bold;font-size:1.2em;color: #000000;opacity:1.0;"
              tool_text <- span(time_text,tags$br(pfvc_text), date_text, style=text_style)
              div(tool_text, id="tooltip", style=sty)
            }
            else{
              div("not tip", id="tooltip", style="display:none;")
            }
                
        }
    })

 observeEvent(input$new_patient_button, {

    points <- input$new_pfvc_num

    print(points)

    new_sub_feat <- as.numeric(c(input$new_female, input$new_afram, input$new_aca, input$new_scl))
    names(new_sub_feat) <- c("female","afram","aca","scl")

    new_pop_feat <- as.numeric(c(input$new_female, input$new_afram))
    names(new_pop_feat) <- c("female","afram")

    new_data <- list(

          ptid = 0-as.integer(input$new_id),
          x = as.numeric(sapply(1:points, function(i) {input[[paste0('time_', i)]]})),
          y = as.numeric(sapply(1:points, function(i) {input[[paste0('pfvc_', i)]]})),
          bias = rep(0,points),
          sub_feat = new_sub_feat,
          pop_feat = new_pop_feat
      )

    #print(str(new_data))

    data$update_ids(new_data)
    data$update_data(new_data)

    updateSelectInput(session,"dropID",choices = c(new_data$ptid,idData()))
    session$sendCustomMessage(type = "refresh",TRUE)

    #toggleModal("newPatient")
    #updateSelectInput(session,"dropID",selected = set[2])
   })


  observeEvent(input$subButton, {
    #d<-idData()
    #d<-d[d$ptid == c(1,2,3,4)]

    val <- as.numeric(input$seltd)

    if(val == 2){
      set<-as.numeric(strsplit(input$idSet, ",")[[1]])
      updateSelectInput(session, "dropID", choices = set)
    }


    #toggleModal("welcomePage")
    #updateSelectInput(session,"dropID",selected = set[2])
    #updateSelectInput(session,"dropID",selected = set[1])
   })
  
  #instatiate database to query against
  #data <- csvDatabase()
  #data <- rambo_database(set = NULL)
  data <- psdo_database()
  
  #fetch new marker info every time patientID is changed
  patientInfo <- reactive({
    data$search_info(input$patientID)
  })
  
  #fetch general patient info 
  patientData <- reactive({
    input$refresh
    data$search_model(input$patientID)
  })
  
  idData <- reactive({
    data$getIDs()
  })

  years <- reactive({
    y<-max(input$yearsEstSlider,lastSeen()+10)
    #print(y)
    y
    })

  subs <- reactive({
    max(input$numSubSlider,8)
    })

  xlm <- reactive({
    c(0,max(input$xLimSlider[2],lastSeen()+10))
    })
  
  #fit the model to the patient

  points <- reactive({
    length(patientData()$x)
    })

  lastSeen <- reactive({
    patientData()$x[input$modelPoints]
    })

  # gvis <- reactive(
  #   data <- predict_traj(data$search_model(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints, 2, input$minMass)
  # )

  # gvis %>% 
  #   ggvis(~X, ~value) %>%
  #   layer_points() %>%
  #   bind_shiny("ggvis", "ggvis_ui")
  

  #allow dropdown to control text input
    observe({
      updateTextInput(session, "patientID", value = input$dropID)
    }, priority = -1)
  
   observeEvent(input$up, {
     #print("up")
     updateNumericInput(session, "modelPoints", value = min(input$modelPoints + 1, length(patientData()$x)))
   })
   observeEvent(input$down, {
     #print("down")
     updateNumericInput(session, "modelPoints", value = max(input$modelPoints - 1,1))
   })

  
  
  #add label of which patientID is being searched
  output$numAvailablePatients <- renderUI({
    #input$submit
    br()
    if(length(input$patientID) == 0)
      return()
    if(input$patientID == "")
      return()
    if(input$patientID %in% idData()[[1]])
      span(
        style = "font-size:100%; color:green;", " Patient ", input$patientID, " found"
      )
    else
      span(
        style = "font-size:100%; color:red;", " Patient ", input$patientID, " not found"
      )
  })

  # output$rChart <- renderChart2({
  #   plot <- predict_traj(data$search_model(input$patientID), input$yearsEstSlider, input$numSubSlider, input$xLimSlider, input$yLimSlider, input$modelPoints, 2, input$minMass)
  #   return(plot)
  #   # p2 <- rPlot(value ~ year, color = 'gender', type = 'line', data = country)
  #   # p2$guides(y = list(min = 0, title = ""))
  #   # p2$guides(y = list(title = ""))
  #   # p2$addParams(height = 300, dom = 'chart2')
  #   # return(p2)
  # })

  output$xSlider <- renderUI({
    #input$subButton
    sliderInput("xLimSlider", "Region of years to show:",  0, max(lastSeen()+10,25), c(0,max(lastSeen()+10,30)))
    }) 

  output$ptNotes_local <- renderText({
    paste("NOTES:",input$patNotes)
    })
  
  output$modelGraphics <- renderPlot({
    input$refresh
    #input$subButton
    #input$subButton
    #render plot of results
    #input$submit
    patientModel()
  })

  # output$modelGraphics_svg <- renderImage({
  #   # Read myImage's width and height. These are reactive values, so this
  #   # expression will re-run whenever they change.
  #   width  <- session$clientData$output_myImage_width
  #   height <- session$clientData$output_myImage_height

  #   # For high-res displays, this will be greater than 1
  #   pixelratio <- session$clientData$pixelratio

  #   # A temp file to save the output.
  #   outfile <- tempfile(fileext='.svg')

  #   # gridsvg("temp.svg",exportJS="inline",addClasses=TRUE)
  #   # p <- patientModel()
  #   # p<-ggplotGrob(p)
  #   # grid.draw(p)
  #   # dev.off()

  #  this <- patientModel()

  #   # Return a list containing the filename
  #   list(src = "plot.svg",
  #        width = width,
  #        height = height,
  #        alt = "This is alternate text")
  # }, deleteFile = FALSE)

  # output$modelGraphics_svg_try1 <- renderUI({
  #   patientModel()
  #   return(tags$object(data="plot.svg", type="image/svg+xml", style="height:1000px;"))
  #   })


  
   output$pID <- renderPrint({
     textInput("pID", value = input$patientID)
   })
  
  output$patientDropdown <- renderUI({
    # input$subButton
    # input$new_patient_button
    #tags$select(id = "dropID", tags$option(value=idData()))
    selectInput("dropID", label=NULL, choices = idData(), selectize = TRUE)
  })
  
  output$modelPointsSlider <- renderUI({
    input$refresh
    # input$subButton
    #sliderInput("modelPoints", "Points to Use: ", 1, points(), points(), step = 1)
    numericInput("modelPoints", "Points to Use: ", value=1, min=1, max=length(patientData()$x))
  })
  
  output$numSubField <- renderUI({
    numericInput("numSubSlider","Subtypes to Use:",value=2,min=1, max=8)
  })
  
  output$yearSlider <- renderUI({
    # input$subButton
    #sliderInput("modelPoints", "Number of points for prediction: ", 1, length(patientData()$t[[1]]), 1, step = 1)
    #print(tail(patientData()$t[[1]],1))
    #sliderInput("yearsEstSlider", "Years to predict:", 0, 22, tail(patientData()$t[[1]],1)+5)
    sliderInput("yearsEstSlider", "Years to predict:", 0, lastSeen() + 10, lastSeen() + 10)
  })
#   output$xLimOutput <- renderUI({
#     if(input$patientID == "")
#       return()
#     #sliderInput("modelPoints", "Number of points for prediction: ", 1, length(patientData()$t[[1]]), 1, step = 1)
#     print(tail(patientData()$t[[1]],1))
#     sliderInput("yearsEstSlider", "Years to predict:", 0, 22, tail(patientData()$t[[1]],1))
#     #sliderInput("xLimSlider","Region of years to show:", 0, 22, c(0,tail(patientData()$t[[1]],1)))
#     sliderInput("xLimSlider","Region of years to show:", 0, 22, c(0,22))
#   })
  
  #,sliderInput("xLimSlider","Region of years to show:", 0, 22, c(0,10))
  
 # output$inputSet <- renderUI({
 #    if(input$setSelect == 1)
 #      return (textInput("idSet", "", value = "1,2,3,...."))
 #    if(input$setSelect == 2)
 #      return()
 #    if(input$setSelect == 3)
 #      return (textInput("idSet", "", value = "2"))
 #  })
  
  output$modelParameters <- renderUI({
    #input$submit
    #render information about the model
  })
  
  #output$patientInfo <- DT::renderDataTable({
  output$patientInfo <- renderTable({
    #input$subButton
    #input$submit
    #display patient information

    #colnames(patientInfo()) <- c("Pt ID", "Age", "Sex", "Race","Scl Sub.","GI","COPD","Muscle")
    d <- t(patientInfo())
    colnames(d) <- "Value"
    d
  })

  # output$patientInfo <- renderUI({
  #   input$subButton
  #   #input$submit
  #   #display patient information

  #   #colnames(patientInfo()) <- c("Pt ID", "Age", "Sex", "Race","Scl Sub.","GI","COPD","Muscle")
  #   M <- t(patientInfo())
  #   colnames(M) <- "Value"
  #   #print(M)
  #    M <- print(xtable(M, align=rep("c", ncol(M)+1)), type = "html", floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
  #    HTML(M)

  # #       html <- paste0("$$", M, "$$")
  # #       list(
  # #           withMathJax(HTML(html))
  # #       )
  # })

# output$patientInfo <- renderPlot({
#   pinf <- as.data.frame(patientInfo())
#   #print(pinf)

#   colnames(pinf) <- c("Value")
#   pinf$Property <- rownames(pinf)
#   pinf <- pinf[-1,]
#   pie <- ggplot(pinf, aes(x = Property, y=Value, fill = Property, stat = "identity")) + 
#          geom_bar(height = .5, width = .5, stat = "identity") + 
#          coord_flip() + 
#          theme_bw() +
#          theme(axis.line=element_blank(),axis.text.x=element_blank(),
#           axis.text.y=element_text(size=14),
#           axis.ticks=element_blank(), axis.title.x=element_blank(),
#           axis.title.y=element_blank(),legend.position="none",
#           panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#           panel.grid.minor=element_blank(),plot.background=element_blank())

#   pie
#   })
  
})