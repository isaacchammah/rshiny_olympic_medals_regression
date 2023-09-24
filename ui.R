#App theme------------------------------------------------------------------------------------------------------


mytheme <- shinytheme ("paper")


#Dashboard------------------------------------------------------------------------------------------------------


shinyUI(dashboardPage(
  skin = 'blue' ,
  dashboardHeader(title = "Olympics Medals Prediction", dropdownMenuOutput("msgOutput")),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("Motivation", tabName = "Motivation", icon = icon("home")),
    menuItem("Data", tabName = "Data", icon = icon("th")),
    menuItem(
      "Regression",
      tabName = "Regression",
      icon = icon("dashboard")
    )
  )),
  
#Medals image------------------------------------------------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                background-color: #ffff;}'))),
    
    
    
    div(style = "display:flex; justify-content:center; background-color:white;",
        img(
          src = "medals2.jpg",
          width = "120px",
          height = "60px"
        )),
    tags$head(tags$style(HTML(
      ".main-sidebar { font-size: 25px; }"
    ))),
    use_theme(mytheme),
    
    
#Tab Motivation------------------------------------------------------------------------------------------------------
    
    
    tabItems( 
      tabItem(tabName = "Motivation",
              fluidPage(
                tabPanel("",
                         box(withSpinner(
                           htmlOutput("Motivation")
                         ), width = 12, border = TRUE)
                )
              )
      ),
      
      
 #Tab Historical Data------------------------------------------------------------------------------------------------------
      
      
      tabItem(tabName = "Data",
              fluidPage( style="background-color:white;",
                box(    sliderInput(
                  inputId = "year1",
                  label = "Year range",
                  min = min(o_m_5$year),
                  max = max(o_m_5$year),
                  value = c(min(o_m_5$year), max(o_m_5$year)),
                  step = 4,
                  sep = ""
                ),
                  withSpinner(plotOutput("slide")), 
                  width = 12, 
                  title = ""
                ),
                box(
                  withSpinner(plotlyOutput("mymap")),
                  width = 12,
                  title = ""
                ),
                box(
                  withSpinner(plotlyOutput("myranking")),
                  width = 12,
                  title = ""
                ),
                box(
                  withSpinner(plotlyOutput("Countries")),
                  width = 12,
                  title = ""
                )
              )),
      
      
#Tab Regression Model------------------------------------------------------------------------------------------------------
      
      tabItem(tabName = "Regression",
              
              fluidPage( style = "display:flex; justify-content:center;" ,
                box(
                  pickerInput(
                    inputId = "Region",
                    label = "Select Region",
                    choices = c('All', unique(df$Country_status)),
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE)
                  ),
                  solidHeader = TRUE,
                  options = list('actions-box' = TRUE),
                  style = "display:flex; justify-content:center;",
                  width = "3",
                  #height = "4",
                  status = "primary",
                  title = "Country status"
                ),
                
                box(
                  selectInput("SelectY", label = "Select variable to predict:",
                              choices = names(ynames)),
                  solidHeader = TRUE,
                  width = "3",
                  #height = "4",
                  status = "primary",
                  title = "Y - Dependent variable"
                ),
                box(
                  pickerInput(
                    inputId = "Corr",
                    label = "Select Data",
                    choices <-  names(xnames3),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  solidHeader = TRUE,
                  width = "3",
                  #height = "4",
                  status = "primary",
                  title = "X - Independent variables"
                ),
                
              ),
              
              fluidPage(
                tabBox(
                  id = "tabset1",
                  height = "500px",
                  width = 12,
                  
                  tabPanel("How it works",
                           box(withSpinner(htmlOutput(
                             "How"
                           ), ), width = 12, ), ),
                  
                  tabPanel("Variables",
                           box(withSpinner(
                             htmlOutput("Variables"),
                           ), width = 12, ), ),
                  
                  tabPanel("Data",
                           box(htmlOutput("Datatext"),
                             withSpinner(DTOutput(
                             "Data"
                           ), ), width = 12, )),
                  
                  tabPanel("Data Summary",
                           box(htmlOutput("Summtext"),
                             withSpinner(verbatimTextOutput("Summ_old"), ),
                             width = 12
                           )),
                  
                  tabPanel("Multicollinearity", 
                           box(htmlOutput("Multitext"), withSpinner(
                             plotOutput("Multi")
                           ), width = 12)),
                  
                  tabPanel(
                    "Plots",
                    box(htmlOutput("Corrtext"), withSpinner(plotOutput("Corr")), width = 12),
                    box(htmlOutput("Plotstext"), withSpinner(plotOutput("Plots")), width = 12)
                  ),
                  
                  
  #Code section for Linear Regression-----------------------------------------------------------------------------
                  
                  
                  tabPanel(tags$b(
                    "Model"),
                    htmlOutput("text1"),
                    box( htmlOutput("Modeltext"),
                      withSpinner(verbatimTextOutput("Model")),
                      width = 12,
                      title = "Model Summary",
                    ),
                    
                    box(htmlOutput("Plottext"),withSpinner(plotOutput("residualPlots")), width = 12, title = "Diagnostic Plots")
                    
                    
                    # box("Error",
                    #   box(htmlOutput("Errortext"),
                    #       withSpinner(DTOutput(
                    #         "Error"
                    #       ), ), width = 12, ))
                    
                    
                  ),
                  
                  tabPanel(
                    "Model: Develod x Underdeveloped Countries",
                    box( htmlOutput("Developedtext"),
                      withSpinner(verbatimTextOutput("Model2")),
                      width = 12,
                      title = "Model Summary"
                    ),
                    
                    box(htmlOutput("Developedplottext"),withSpinner(plotOutput("residualPlots2")), width = 12, title = "Diagnostic Plots - Developed "),
                    box(htmlOutput("Underdevelopedplottext"),withSpinner(plotOutput("residualPlots3")), width = 12, title = "Diagnostic Plots - Underdeveloped")
                    
                    
                  ),
                  
                  
                  tabPanel(
                    "Model Selection",
                    box( htmlOutput("AICtext"),
                      withSpinner(verbatimTextOutput("Model3")),
                      width = 12,
                      title = "Model Summary"
                    ),
                    
                    box( htmlOutput("AICplottext"),
                      withSpinner(plotOutput("residualPlots4")), width = 12, title = "Diagnostic Plots")
                    
                  ),
                  
                  
              
                  
                  
                  tabPanel("Conclusion",
                           box(withSpinner(
                             htmlOutput("Conclusion"),
                           ), width = 12, ), ),
                  
                )
              ))
      
    )
  )
)
)
