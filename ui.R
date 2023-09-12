mytheme <- create_theme(
  adminlte_color(light_blue = "#434C5E"),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)



dashboardPage(
  skin = 'purple' ,
  dashboardHeader(title = "Olympics Medals", dropdownMenuOutput("msgOutput")),
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
  
  
  dashboardBody(div(style = "display:flex; justify-content:center;",
                    img(
                      src = "medals2.jpg",
                      width = "150px",
                      height = "75px"
                    )), 
                tags$head(tags$style(HTML(
                  ".main-sidebar { font-size: 25px; }"
                ))),
                use_theme(mytheme),
                
    
    
 
  
    
    tabItems(
      
      tabItem(tabName = "Motivation",
              fluidPage(
                tabBox(
                  id = "tabset1",
                  height = "1000px",
                  width = 12,
                  
                  
                  tabPanel("Motivation",
                           box(withSpinner(
                             htmlOutput("Motivation"),
                           ), width = 12, ), ),
                  
                  
                ),
                
                
              )),
      
      
      
      
      
      tabItem(tabName = "Regression",
              
              fluidPage(
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
                  height = "1000px",
                  width = 12,
                  
                  tabPanel("How it works",
                           box(withSpinner(
                             htmlOutput("How"),
                           ), width = 12, ), ),
                  
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
                  
                  
                  tabPanel("Multicoliniarity",
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
                      title = "Model Summary"
                    ),
                    
                    box(withSpinner(plotOutput("residualPlots")), width = 12, title = "Diagnostic Plots")
                    
                  ),
                  
                  tabPanel("Model: Develod x Underdeveloped Countries",
                           box(
                             withSpinner(verbatimTextOutput("Model2")),
                             width = 12,
                             title = "Model Summary"
                           )),
                  
                )
              )),
      
      
      
      tabItem(tabName = "Data",
              fluidPage(
                tabBox(
                  id = "tabset1",
                  height = "1000px",
                  width = 12,
                  
                  tabPanel(
                    "Ranking",
                    sliderInput(
                      inputId = "year1",
                      label = "Position",
                      min = min(o_m_5$year),
                      max = max(o_m_5$year),
                      value = c(min(o_m_5$year), max(o_m_5$year)),
                      step = 4
                    ),
                    box(withSpinner(plotOutput("slide")), width = 12, title = "Diagnostic Plots"),
                    
                    
                  ),
                  
                  
                  tabPanel("Map",
                           box(
                             withSpinner(plotlyOutput("mymap")),
                             width = 12,
                             title = "Map"
                           ), ),
                  
                  tabPanel("Ranking",
                           box(
                             withSpinner(plotlyOutput("myranking")),
                             width = 12,
                             title = "Ranking"
                           ), ),
                  
                  tabPanel("Countries",
                           box(
                             withSpinner(plotlyOutput("Countries")),
                             width = 12,
                             title = "Countries per game"
                           ), )
                  
                  
                  
                  
                ),
                
                
              ))
    )
  )
)
