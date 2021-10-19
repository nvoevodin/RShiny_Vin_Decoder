




ui = fluidPage(tags$head(includeCSS("styles.css")),introjsUI(),shinyjs::useShinyjs(),use_waiter(),
               navbarPage("TLC VIN DECODER",
                          id = 'tabs',
                          
                          tabPanel("Vin Decoder", useShinydashboardPlus(), useShinyalert(), tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
                                   fluidRow(boxPlus(id = 'main_view1',solidHeader = T, collapsible = F, 
                                                    collapsed = F, closable = F, title = '', status = 'success',
                                                    textOutput('title0'),
                                                    
                                                    
                                                    
                                                    selectInput('radio', 'SELECT A SINGLE VIN OR A LIST OF VINS', choices = c('1 VIN', 'CSV List'), selected = '1 VIN'),br(),br(),
                                                    
                                                    conditionalPanel(condition = "input.radio == '1 VIN'", 
                                                                     textInput('vin', 'Type your VIN', 
                                                                               value = '3FA6P0HD7ER281417',
                                                                               placeholder = 'Type your VIN'),
                                                                     actionButton('submit', 'Submit', icon = icon('play')),br() ,br()              
                                                                     
                                                    ),
                                                    
                                                    conditionalPanel(condition = "input.radio == 'CSV List'", 
                                                                     fileInput('vins', 'Submit CSV with VINs (***MUST have a column: "vin")',accept = c(
                                                                       "text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv")),br(), 
                                                                     selectInput(inputId = "type", label = 'SHOW ME:', choices = c('Gasoline',
                                                                                                                                   'Hybrid', 
                                                                                                                                   'Electric',
                                                                                                                                   'Diesel',
                                                                                                                                   'CNG',
                                                                                                                                   'Ethanol',
                                                                                                                                   'LPG',
                                                                                                                                   'All'
                                                                     ), selected = 'All'), uiOutput('download1')
                                                                     
                                                    )
                                                    
                                                    
                                                    , width = NULL
                                   )),
                                   
                                   fluidRow(boxPlus(id = 'main_view12',solidHeader = T, collapsible = F, 
                                                     collapsed = F, closable = F, title = '', status = 'success',
                                                     # div(
                                                     #   id = "content",
                                                     #   h3("<-- ADD VIN or VINS")
                                                     # ),
                                                    conditionalPanel(condition = "input.radio == 'CSV List'",
                                                    dataTableOutput('table'),br(),
                                                    column(4,textOutput('title1'), br(),br(),textOutput('countOfVehs')),
                                                    column(4,textOutput('title2'), br(),echarts4rOutput('byType')),
                                                    column(4,textOutput('title3'), br(),textOutput('averageAge'))
                                                    ),
                                                    conditionalPanel(condition = "input.radio == '1 VIN'",
                                                    textOutput('vin1'), br(),
                                                    textOutput('make'), br(),
                                                    textOutput('model'), br(),
                                                    textOutput('year1'), br(),
                                                    textOutput('type1'))
                                                    
                                                     
                                                     , width = NULL
                                   ))
                                   
    
                          ),
                          
                          tabPanel("TLC Vehicles",
                                   
                                   actionButton('submit1', 'Show me', icon = icon('play')),br()
                                   , 
                                   actionButton('submit2', 'Download me', icon = icon('download')),br()
                                   ,
                           
                           column(9,
                                            
                                            fluidRow(
                                             
                                              column(6,textOutput("title4"),
                                                     textOutput("countOfVehs1")),
                                                     
                                              
                                              column(6,textOutput("title5"),
                                                     textOutput("averageAge1"))),br(),br(),br(),br(),br()
                                            
                                            
                                                     ,
                                            fluidRow(
                                                   echarts4rOutput('byType1'))),
                           column(3, textOutput("title6"),
                                            tableOutput('table2')
                                            
                                            
                                            
                                            
                                                             
                                            
                                            
                                            
                                            
                           ))
                           
                          )
                          
                          
                          
                          
                          
                          
                          
               )