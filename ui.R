#App theme------------------------------------------------------------------------------------------------------


mytheme <- shinytheme ("paper")


#Dashboard------------------------------------------------------------------------------------------------------


shinyUI(dashboardPage(
  skin = 'purple' ,
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
          width = "150px",
          height = "75px"
        )),
    tags$head(tags$style(HTML(
      ".main-sidebar { font-size: 25px; }"
    ))),
    use_theme(mytheme),
    
    
#Tab Motivation------------------------------------------------------------------------------------------------------
    
    
    tabItems( 
      tabItem(tabName = "Motivation",
              fluidPage(
                tabBox(
                  id = "tabset1",
                  height = "1000px",
                  width = 12,
                  
                  
                  tabPanel("",
                           box(withSpinner(
                             htmlOutput("Motivation"),
                           ), width = 12,border=TRUE,),),
                  
                  
                ),
                
                
              )),
      
      
 #Tab Historical Data------------------------------------------------------------------------------------------------------
      
      
      tabItem(tabName = "Data",
              fluidPage( style="background-color:white;",
                sliderInput(
                  inputId = "year1",
                  label = "Year range",
                  min = min(o_m_5$year),
                  max = max(o_m_5$year),
                  value = c(min(o_m_5$year), max(o_m_5$year)),
                  step = 4,
                ),
                box(
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
                    choices = c('All', unique(df$Country_status))
                    ,
                    multiple = FALSE,
                    options = list(`actions-box` = TRUE)
                  ),
                  solidHeader = TRUE,
                  options = list('actions-box' = TRUE),
                  width = "3",
                  status = "primary",
                  title = "Country status"
                ),
                
                box(
                  selectInput("SelectY", label = "Select variable to predict:",
                              choices = names(ynames)),
                  solidHeader = TRUE,
                  width = "3",
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
                           box(withSpinner(DTOutput(
                             "Data"
                           ), ), width = 12, )),
                  
                  tabPanel("Data Summary",
                           box(
                             withSpinner(verbatimTextOutput("Summ_old"), ),
                             width = 12
                           )),
                  
                  tabPanel("Multicollinearity",
                           box(withSpinner(
                             plotOutput("Multi")
                           ), width = 12)),
                  
                  tabPanel(
                    "Plots",
                    box(withSpinner(plotOutput("Corr")), width = 12),
                    box(withSpinner(plotOutput("Plots")), width = 12)
                  ),
                  
                  tabPanel(
                    "Model",
                    htmlOutput("text1"),
                    box(
                      withSpinner(verbatimTextOutput("Model")),
                      width = 12,
                      title = "Model Summary",
                    ),
                    
                    box(withSpinner(plotOutput("residualPlots")), width = 12, title = "Diagnostic Plots")
                    
                  ),
                  
                  tabPanel(
                    "Model: Develod x Underdeveloped Countries",
                    box(
                      withSpinner(verbatimTextOutput("Model2")),
                      width = 12,
                      title = "Model Summary"
                    ),
                    
                    box(withSpinner(plotOutput("residualPlots2")), width = 12, title = "Diagnostic Plots - Developed "),
                    box(withSpinner(plotOutput("residualPlots3")), width = 12, title = "Diagnostic Plots - Underdeveloped")
                    
                    
                  ),
                  
                  
                  tabPanel(
                    "Model Selection",
                    box(
                      withSpinner(verbatimTextOutput("Model3")),
                      width = 12,
                      title = "Model Summary"
                    ),
                    
                    box(withSpinner(plotOutput("residualPlots4")), width = 12, title = "Diagnostic Plots")
                    
                  ),
                  
                  
                  # tabPanel(
                  #   "Best Model",
                  #   box(
                  #     withSpinner(verbatimTextOutput("Model4")),
                  #     width = 12,
                  #     title = "Model Summary"
                  #   ),
                    
                  #   box(withSpinner(plotOutput("residualPlots5")), width = 12, title = "Diagnostic Plots")
                  #   
                  # ),
                  
                  
                  
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
