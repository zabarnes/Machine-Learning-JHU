# Scleroderma Web Application
# Zachary Barnes, Peter Schulam, Suchi Saria, PhD.
# Dr. Suchi Saria's Lab @ Johns Hopkins University

# Code to define User Interface for generating and displaying Scleroderma patient predictions

# Using shiny for easy creation of web application in R (see http://shiny.rstudio.com)
library(shiny)
library(shinydashboard)
source("interface/dashboardHeaderSearch.R")
source("interface/graphics.R")
source("interface/bsModal.R")
source("interface/deps.R")
#library(ggvis)
#library(rCharts)


  head <- headerSearchForm(
    title = "Scleroderma App"
  )

# head <- nav_header()

# head <- dashboardHeader(
#   title = "Sclerodata App - v1.0",
#   titleWidth = 250
# )

#radField <- c
#names(radField) <- list(tags$input(type="text",id="idSet", placeholder="1,2,3,..."), "All")
#$('#dbl_clk_info').css('display','none');
#$('#dbl_clk_info').remove();

body <- dashboardBody(
  #nav_header(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src="custom.js"),
    tags$script("
      Shiny.addCustomMessageHandler('removeDBLCLICK',
        function(val) {
          $('#new_point_box').css('display','none');
        });
    "),
    tags$script("
      Shiny.addCustomMessageHandler('refresh',
        function(val) {
          $('#refresh').trigger( 'click' );
        });
    "),
    tags$head(tags$style("#modelGraphics{height:85vh !important;}"))
  ),
  htmlOutput("hover_info",style="display:block;position:fixed;z-index:999;"),
  htmlOutput("dbl_clk_info",style="display:block;position:fixed;z-index:999;"),
  bsModal("modalExample"
          , paste("Input notes for patient  :", sep = "")
          , "inputInfo"
          , close.button = TRUE
          #, footer = tags$button(id = "subButton", type="active",class="btn btn-default","Submit")
          , size = "small"
          , tags$textarea(name = textOutput("pID"), cols = 35, rows = 10, id = "patNotes", placeholder = "Notes on patient....", required=TRUE)
          ,textOutput("ptNotes_local")
          ),
  # bsModal("modal_disp"
  #         , paste("Input notes for patient  :", sep = "")
  #         , "inputInfo"
  #         , close.button = TRUE
  #         , size = "large"
  #         , display_inf()
  # ),
bsModal("newPatient"
          , tags$h3("Input Details for New Patient:")
          , ""
          , close.button = FALSE
          , size = "large"
          , fluidRow(

            column(width = 6,
              textInput("new_id","ID (will be converted negative):")
            , selectInput("new_female", "Female:", c("No"=0,"Yes"=1))
            , selectInput("new_afram", "African American:", c("No"=0,"Yes"=1))
            , selectInput("new_aca", "ACA Positive:", c("No"=0,"Yes"=1))
            , selectInput("new_scl", "SCL 70 Positive:", c("No"=0,"Yes"=1))
            ),
            column(width = 6,
              numericInput("new_pfvc_num","Number of PFVC Points",value=1,min = 1,step=1),
              lapply(1:20, function(i) {
                uiOutput(paste0('p', i),style="padding-bottom:5px")
              })
              ))
          ,footer=tags$button(id = "new_patient_button", type = "action", class = "btn btn-success action-button btn-lg", style="float:left;font-size: 1.5em;font-weight:bold;", "Enter Patient")
          ),

bsModal("welcomePage"
          #, paste("About Scleroderma Prediction Application :", sep = "")
          , tags$p("Scleroderma Patient Lung Prediction App", style="font-size: 1.9em;font-weight:600;")
          , "c"
          , close.button = FALSE
          , tags$h3("About")
          , tags$p("This application assists clinicians in treating scleroderma patients by providing individualized predictions of lung function. 
                    The machine learning model and application were developed through collaboration between Suchi Saria's Lab 
                    at the Johns Hopkins Whiting School of Engineering and the Johns Hopkins Scleroderma Center.")
          , tags$h3("Instructions")
          , tags$ul(
              tags$li("Select a set of patients of interest below."), 
              tags$li("Use the search bar or the list of Patient IDs to select patients."),
              tags$li("Use the 'more' or 'less' buttons to observe how adding and removing data points change predictions."),
              tags$li("Use the 'Adv.' tab to customize model interaction."),
              tags$li("Save lives!")
            )
          , tags$hr()
          , tags$h3("Patients to View")
          #, span(radioButtons("setSelect", label = h3("Patients to View"), choices = list(tags$input(type="text",id="idSet", placeholder="1,2,3,..."), "All"), selected = 1, inline = FALSE))
          #, span(radioButtons("setSelect", label = h3("Patients to View"), choices = radField, selected = 1, inline = FALSE))
          
           ,tags$form(class="shiny-input-radiogroup", id="seltd",
             tags$input(type="radio",name = "seltd", id="setS",value="1", checked = TRUE, "All Patients", tags$br()),
             tags$input(type="radio",name = "seltd", id="setS",value="2", "Set of Clinic Patients: ", tags$input(type="text",id="idSet", placeholder="1,2,3,..."))
             )
          #, uiOutput("inputSet")
          #, textInput("idSet", "", value = "1,2,3,..."))
          #, actionButton("subButton", label = "Submit")
          ,tags$br()
          #,tags$button(id = "subButton", type = "action", class = "btn btn-success action-button btn-lg", "Start Application")
          , footer = span(
              tags$button(id = "subButton", type = "action", class = "btn btn-success action-button btn-lg", style="float:left;font-size: 1.5em;font-weight:bold;", "Start Application"),
              tags$img(src = "wse.png", align="middle", style="display: inline-block;padding-right:20px;"), 
              tags$img(src = "jhmi.png", align="middle", style="display: inline-block;padding-right:20px;"), 
              tags$img(src = "nsf.gif", align="middle", style="display: inline-block;")
              #tags$button(id = "sBut", type="action", class="btn btn-default","Submit", style="color:black;")
              )
          , size = "large"
          ),
  fluidRow(
    column(width = 10,
            # tabBox(
            #        #height = "250px",
            #        width = NULL,
            #        selected = "Static",
            #  tabPanel("Static",
            #           plotOutput("modelGraphics")
            #           ),
            #  tabPanel("Interactive",
            #      uiOutput("ggvis_ui"),
            #      ggvisOutput("ggvis")
            #      )
            #  )
            box( status = "primary", width = NULL,
                plotOutput("modelGraphics", height=650,
                  hover = hoverOpts(id ="plot_hover"),
                  dblclick = dblclickOpts(id = "plot_dblclick"),
                  click = clickOpts(id = "plot_click"))
                #uiOutput("hover_info")
                #htmlOutput("modelGraphics_svg_try1")
                #tags$object(data="plot.svg", type="image/svg+xml", style="height:1000px;")
              )
        
            
#            box( status = "primary", width = NULL,
#                fluidRow(
#                  column(width=3, uiOutput("yearSlider"),
#                  column(width=3, sliderInput("numSubSlider", "Number of subtypes to show:", 1, 9, 2)),
#                  column(width=3, sliderInput("xLimSlider", "Region of years to show:", 0, 22, c(0,10))),
#                  column(width=3, sliderInput("yLimSlider", "Region of PFVC to show:", 0, 125, c(0,125)))
#                ),
#                #uiOutput("modelParameters")
#            )
    ),
    column(width = 2,
#            box(title="Patient ID Search", width = NULL, solidHeader = TRUE, status = "warning",
#                uiOutput("patientDropdown"),
#                tags$form(
#                 textInput("patientID","Enter Patiend ID: "),
#                 #actionButton("submit", "Search"),
#                 uiOutput("numAvailablePatients")
#                )
#            ),
            tabBox(
                   #height = "250px",
                   width = NULL,
                   selected = "Model",
                   tabPanel("Model"
                       #,uiOutput("patientDropdown")
                      ,uiOutput("modelPointsSlider", style="")
                        #,actionButton("up",label="More", title="Points to use:",style="width:40%;",class="btn btn-danger")
                        #,actionButton("down",label="Less",style="width:40%;")
                      ,div(style = "width:100%",
                        tags$button(id = "up", type = "action", class = "btn btn-primary action-button", style = " font-size: .9em; font-weight: bold;width:48%;", "More Points")
                        ,tags$button(id = "down", type = "action", class = "btn btn-default action-button", style = " font-size: .9em; font-weight: bold;width:48%;", "Less Points")
                        )                                    
                                                     
    
                       ,checkboxInput("toggle_meds", label = "Display Medications", value = FALSE)
                       ,tags$div("Usefulness: ",class="rating", style="width:100%;",
                                tags$span(id="star_5",HTML("&#9734;")),
                                tags$span(id="star_4",HTML("&#9734;")),
                                tags$span(id="star_3",HTML("&#9734;")),
                                tags$span(id="star_2",HTML("&#9734;")),
                                tags$span(id="star_1",HTML("&#9734;"))
                        )
                       #,actionButton("up",label="+", title="Points to use:")
                        #,actionButton("down",label="-")
                       #,uiOutput("numSubField")
                       #,uiOutput("patientDropdown")
                       ),
              # tabPanel("Graph"
              #          ,sliderInput("xLimSlider","Region of years to show:", 0, 22, c(0,22))
                       
              #          #,uiOutput("xLimOutput")
              #          , sliderInput("yLimSlider", "Region of PFVC to show:", 0, 125, c(0,125))
              #          #,sliderInput("yearsEstSlider", "Years to predict:", 0, 22, 10)
              #          ,uiOutput("yearSlider")
              #          ),
              tabPanel("Adv."
                       #,uiOutput("modelPointsSlider")
                       #,checkboxInput("toggle_meds", label = "Display medications", value = FALSE)
                       
                       ,uiOutput("numSubField")
                       ,checkboxInput("togMass", label = "Toggle minimum mass", value = FALSE)
                       ,numericInput("minMass", label = "Set minimum mass for subtypes: ", min = 0, max = 1, step = .1, value = .8)
                       #,sliderInput("xLimSlider","Region of years to show:", 0, 22, c(0,22))
                       ,uiOutput("xSlider")
                       
                       #,uiOutput("xLimOutput")
                       , sliderInput("yLimSlider", "Region of PFVC to show:", 0, 125, c(0,125))
                       #,sliderInput("yearsEstSlider", "Years to predict:", 0, 22, 10)
                       ,uiOutput("yearSlider")
                       #,actionButton("inputInfo", "Enter Notes")
                       ,tags$button(id = "inputInfo", type = "action", class = "btn btn-warning action-button", style = "font-size: 1.2em; font-weight: bold;width:100%;", "Enter Patient Notes")
                         
                       
              )
            )
#             tabBox(title="Model Control",width = NULL, status = "warning"
#                 ,uiOutput("modelPointsSlider")
#                 ,sliderInput("numSubSlider", "# Subtypes to show:", 1, 9, 2)
#                , sliderInput("xLimSlider", "Region of years to show:", 0, 22, c(0,10))
#                 ,sliderInput("yLimSlider", "Region of PFVC to show:", 0, 125, c(0,125))
#                , sliderInput("yearsEstSlider", "Years to predict:", 0, 22, 10)
#             )
           ,box(title="Patient Information",width = NULL, status = "warning",
               #DT::dataTableOutput('patientInfo')
               tableOutput('patientInfo')
               #htmlOutput('patientInfo')
               #plotOutput('patientInfo')
          )
         #actionButton(inputId = "saveButton", label = "Download ", icon = icon("download"), size="large")
        
    )
  )
)


meta <- tags$head(
  tags$meta(name="apple-mobile-web-app-capable",content="yes")
  ,tags$meta(name="apple-mobile-web-app-status-bar-style",content="black-translucent")
  ,tags$meta(name="viewport", content="initial-scale=1")
  )

dashPage <- function(header, sidebar, body, title = NULL,
  skin = c("blue", "black", "purple", "green", "red", "yellow")) {
  tags$script()
  body <- div(body,class = "content-wrapper")
  sidebar <- tags$aside(sidebar, class = "main-sidebar")
  tagAssert(header, type = "header", class = "main-header")
  tagAssert(sidebar, type = "aside", class = "main-sidebar")
  tagAssert(body, type = "div", class = "content-wrapper")
  skin <- match.arg(skin)
  title <- "Scleroderma Patient Lung Prediction App"
  content <- div(class = "wrapper",
    header,
    sidebar,
    body
  )
  addDeps(
    tags$body(class = paste0("skin-", skin), style = "min-height: 611px;",
      shiny::bootstrapPage(meta,content, title = title)
   
    )
  )
}


dashPage(
  head,
  dashboardSidebar(disable = TRUE),
  body
)
# dashboardPage(
#   head,
#   dashboardSidebar(disable = TRUE),
#   body
# )

