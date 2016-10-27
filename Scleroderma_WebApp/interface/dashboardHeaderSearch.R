

nav_header <- function(){
  tags$nav(class="navbar navbar-default navbar-static-top", style="top: 100%;background-color: #3c8dbc;",
    tags$div(class="container-fluid", style= "background-color: #3c8dbc;",
             tags$a(class="navbar-brand", style="font-family: Times;font-weight: bold;
                    font-size: 24px;", href="#",tags$p("Scleroderma Lung Trajectory Tool",style="color:white;")),
  tags$form(class="navbar-form navbar-default",role="search", style="background-color: #3c8dbc;",
    tags$div(class="container",style="background-color: #3c8dbc;",
             tags$form(
             tags$div(id = "parent", class="form-group", style="padding-top:4px;width:35%;"
                        ,tags$input(id = "patientID",style="width:75%;float:left", type="text",class="form-control",placeholder="Search for patient using patient ID #")
                        #,tags$button(id = "subButton", style = "float:left", type="active",class="btn btn-default","Search")
                        
                      
                          
             ))
             #,tags$button(id = "subButton", type="active",class="btn btn-default","Submit")
    
             
             
    )
  )
    )
  )
  
}

cont <- function(){
  tags$div(id = "parent", class="form-group",style="",
           tags$input(id = "patientID",style="width:100px;float:left;", type="text",class="form-control",placeholder="Search")
  )
}



headerSearchForm <- function(title, textId = "patientID", buttonId="b", label = "Search for patient...",
                              icon = shiny::icon("refresh")) {
  tags$header(class = "main-header", style=""
              ,tags$div(style="background-color: #3c8dbc;")
              ,tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
              ,span(class = "logo", title, style="font-size: 1.5em;background-color: #3c8dbc;")
              ,tags$nav(class = "navbar navbar-static-top", role = "navigation", style="background-color: #3c8dbc;",
                       # Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       div(class = "navbar-custom-menu",style="padding-right:2%",
                           tags$ul(class = "nav navbar-nav", style="height:50px;",
                                   tags$li(class="active",tags$a(href="javascript:$('#welcomePage').modal('show');",tags$span("About and Startup", style="font-size: 1.1em; font-weight: bold;"))),
                                   tags$li(class="",tags$a(href="javascript:$('#newPatient').modal('show');",tags$span("Examine New Patient", style="font-size: 1.1em; font-weight: bold;"))),
                                   #tags$li(tags$a(href="#",tags$span("TBD"))),
                                   htmlOutput("patientDropdown",inline=TRUE,class="main-header",style="right:0px;display:inline-block;float:right;top:8px;width:7%;height:34px;"),
                                   tags$li(class="",tags$a(href="javascript:$('#nextPatient').click();",tags$span("View Next PTID", style="font-size: 1.1em; font-weight: bold; height:50px;")),style="float:right;height:50px;"),
                                
                                   div(style="vertical-align: middle;", 
                                    tags$button(id = "nextPatient", type = "action", class = "btn btn-primary action-button", style = "font-size:1.2em; font-weight:bold; float:right; padding-top:8px; width:7%;display:none;", "Next Pt"),
                                    tags$form(class = "main-header", style="float:right;width:8%;top:8px;display:inline-block;visibility: hidden;",
                                        span(class = "input-group", style="",
                                            #htmlOutput("patientDropdown",inline=TRUE,class="main-header",style="display:inline-block;"),
                                            span(tags$input(id = textId, type = "text", style="", class = "form-control",
                                                       placeholder = label
                                            ),style=""),
                                            span(class = "input-group-btn",
                                                 tags$button(id = "refresh", type = "action",
                                                                 class = "btn btn-flat action-button",
                                                                 icon
                                                     )
                                            )
                                        )
                                  )
                                )
                             # tags$form(class="navbar-form", role="search",
                                   #    tags$div(class="form-group",
                                   #      tags$input(type="text",class="form-control",placeholder="Search")
                                   #      ),
                                   #  tags$button(type="submit",class="btn btn-default","Submit")


                                   #  )
  
                           )
                       )
              )
  )
}

# <form class="navbar-form navbar-left" role="search">
#         <div class="form-group">
#           <input type="text" class="form-control" placeholder="Search">
#         </div>
#         <button type="submit" class="btn btn-default">Submit</button>
#  </form>



# tags$form(class = "main-header",
#                                         div(class = "input-group", style="",
#                                             tags$input(id = textId, type = "text", style="width:17%;float:right;", class = "form-control",
#                                                        placeholder = label
#                                             ),
#                                             span(class = "input-group-btn",
#                                                  tags$button(id = buttonId, type = "action",
#                                                                  class = "btn btn-flat action-button",
#                                                                  icon
#                                                      )
                                                 
                                                 

#                                             )
                                            
                                            

#                                         )
#                               )
# <ul class="nav navbar-nav">
#         <li class="active"><a href="#">Link <span class="sr-only">(current)</span></a></li>
#         <li><a href="#">Link</a></li>
#         <li class="dropdown">
#           <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
#           <ul class="dropdown-menu">
#             <li><a href="#">Action</a></li>
#             <li><a href="#">Another action</a></li>
#             <li><a href="#">Something else here</a></li>
#             <li role="separator" class="divider"></li>
#             <li><a href="#">Separated link</a></li>
#             <li role="separator" class="divider"></li>
#             <li><a href="#">One more separated link</a></li>
#           </ul>
#         </li>
#       </ul>


#                                                     tags$form(class = "main-header",
          #                                              div(class = "input-group", style="top:8px",
          #                                                  tags$input(id = textId, type = "text", class = "form-control",
          #                                                             placeholder = label
          #                                                  ),
          #                                                  span(class = "input-group-btn",
          #                                                       tags$button(id = buttonId, type = "button",
          #                                                                   class = "btn btn-flat action-button",
          #                                                                   icon
          #                                                       )
          #                                                  )
          #                                              )
          #                                    )