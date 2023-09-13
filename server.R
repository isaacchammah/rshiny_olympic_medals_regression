shinyServer(function(input, output, session) {
  observeEvent(input$Region, {
    updatePickerInput(
      session = session,
      inputId = "Region",
      options = pickerOptions(selectedTextFormat = 'values')
    )
  }, ignoreInit = TRUE)
  
  
  
  output$Motivation<- renderUI({
    HTML(
      paste(
      
        '<center> <h1> Olympic medals are important </h1> </center>',
        '<p> Like it or not, every country, especially the United States, attaches great importance to winning medals, gold medals to be precise, at the Olympic Games, because it is a matter of national pride, symbol of national strength and a good way of commanding respect from others.<p/>',  
       '<p> China has done an excellent job of cultivating athletes. Those who criticize Chinas way of selecting and training athletes have the ulterior motive of stirring up controversy and creating a negative image of China. Yang Hui, on China Daily website </p>'
      )
    )
  })
  
 
  
  
  output$How<- renderUI({
    HTML(
      paste(
        '<h2> <center> Multiple linear regression (MLR)</center> </h2>',
        '<p> Multiple linear regression (MLR), also known simply as multiple regression, is a statistical technique that uses several explanatory variables to predict the outcome of a response variable. </p>',
          '<p> The goal of multiple linear regression is to model the linear relationship between the explanatory (independent) variables and response (dependent) variables. In essence, multiple regression is the extension of ordinary least-squares (OLS) regression because it involves more than one explanatory variable.<p/>',
        '<p>The objective of this app is to enable the user to build a multiple linear regression (MLR) model to predict which variables may influence a country to obtain more or fewer medals during the Olympic Games.</p>',
          '<p>The provided dataset refers to the 2020 Summer Olympic Games.</p>',
        '<b>Tabs </b>',
        
        "<p style='color:brown'> <b>Variables</b>: The user will see available independent and dependent variables in the dataset</p>",
        "<p style='color:brown'><b> Data</b>: Displays all available data, can be filtered for developed or developing countries</p>",
        "<p style='color:brown'><b>Data Summary</b>: Provides a summary of the variables</p>",
        "<p style='color:brown'><b>Multicollinearity</b>: According to the selected independent variables, it shows the correlation matrix between them. It\'s important to note that the correlation should not be high; otherwise, the model won\'t be correct.</p>",
        "<p style='color:brown'><b>Plots</b>: The first graph displays the correlation matrix between all independent variables and the dependent variable. The second graph illustrates this relationship and shows if the same is linear.</p>",
        "<p style='color:brown'><b>Model</b>: MLR Model according to the chosen region and graphs to check for homoscedastic errors, normally distributed errors, and non-autocorrelated errors.</p>",
        "<p style='color:brown'><b>Model: Developed x Underdeveloped Countries</b>: A model that compares a regression done only with developed countries and another done only with developing countries.</p>"
      )
    )
  })
  
  
  
  output$Variables <-
    renderUI({
      HTML(
        paste(
          "<b>  Dependent variables </b>",
          "<ul>",
          "<li style='color:blue'> Gold: Gold medals </li>",
          "<li style='color:blue'>Silver: Silver medals </li>",
          "<li style='color:blue'>Bronze: Bronze medals</li>",
          "<li style='color:blue'> Total: Total medals</li>",
          "<li style='color:blue'>Rank: % of medals won out of all medals</li>",
          "</ul>",
          "<b>  Independent variables </b>",
          "<ul>",
          "<li style='color:green'>Country: Name of the country.</li>",
          "<li style='color:green'>Density (P/Km2): Population density measured in persons per square kilometer.</li>",
          "<li style='color:green'>Abbreviation: Abbreviation or code representing the country.</li>",
          "<li style='color:green'>Agricultural Land (%): Percentage of land area used for agricultural purposes.</li>",
          "<li style='color:green'>Land Area (Km2): Total land area of the country in square kilometers divided by 10^6.</li>",
          "<li style='color:green'>Armed Forces Size: Size of the armed forces in the country.</li>",
          "<li style='color:green'>Birth Rate: Number of births per 1,000 population per year.</li>",
          "<li style='color:green'>Calling Code: International calling code for the country.</li>",
          "<li style='color:green'>Capital/Major City: Name of the capital or major city.</li>",
          "<li style='color:green'>CO2 Emissions: Carbon dioxide emissions in tons.</li>",
          "<li style='color:green'>CPI: Consumer Price Index, a measure of inflation and purchasing power.</li>",
          "<li style='color:green'>CPI Change (%): Percentage change in the Consumer Price Index compared to the previous year.</li>",
          "<li style='color:green'>Currency_Code: Currency code used in the country.</li>",
          "<li style='color:green'>Fertility Rate: Average number of children born to a woman during her lifetime.</li>",
          "<li style='color:green'>Forested Area (%): Percentage of land area covered by forests.</li>",
          "<li style='color:green'>Gasoline_Price: Price of gasoline per liter in local currency.</li>",
          "<li style='color:green'>GDP: Gross Domestic Product, the total value of goods and services produced in the country divided by 10^6.</li>",
          "<li style='color:green'>Gross Primary Education Enrollment (%): Gross enrollment ratio for primary education.</li>",
          "<li style='color:green'>Gross Tertiary Education Enrollment (%): Gross enrollment ratio for tertiary education.</li>",
          "<li style='color:green'>Infant Mortality: Number of deaths per 1,000 live births before reaching one year of age.</li>",
          "<li style='color:green'>Largest City: Name of the country's largest city.</li>",
          "<li style='color:green'>Life Expectancy: Average number of years a newborn is expected to live.</li>",
          "<li style='color:green'>Maternal Mortality Ratio: Number of maternal deaths per 100,000 live births.</li>",
          "<li style='color:green'>Minimum Wage: Minimum wage level in local currency.</li>",
          "<li style='color:green'>Official Language: Official language(s) spoken in the country.</li>",
          "<li style='color:green'>Out of Pocket Health Expenditure (%): Percentage of total health expenditure paid out-of-pocket by individuals.</li>",
          "<li style='color:green'>Physicians per Thousand: Number of physicians per thousand people.</li>",
          "<li style='color:green'>Population: Total population of the country divided by 10^6.</li>",
          "<li style='color:green'>Population: Labor Force Participation (%): Percentage of the population that is part of the labor force.</li>",
          "<li style='color:green'>Tax Revenue (%): Tax revenue as a percentage of GDP.</li>",
          "<li style='color:green'>Total Tax Rate: Overall tax burden as a percentage of commercial profits.</li>",
          "<li style='color:green'>Unemployment Rate: Percentage of the labor force that is unemployed.</li>",
          "<li style='color:green'>Urban Population: Percentage of the population living in urban areas.</li>",
          "<li style='color:green'>Latitude: Latitude coordinate of the country's location.</li>",
          "<li style='color:green'>Longitude: Longitude coordinate of the country's location.</li>",
          "<li style='color:green'>Country Status: If the GDP/capta was lower then $12,000 the country was considered Underdeveloped, else Developed </li>",
          "<li style='color:red'>All dependent variables can also be chosen in their log form</li>",
          "</ul>",
          collapse = "\n"
        )
      )
    })
  
  
  
  
  InputDataset <- reactive({
    region <- input$Region
    
    if (region == 'All') {
      return(df)
    }
    
    df %>%
      filter(Country_status == region)
    
  })
  
  output$slide = renderPlot({
    o_m_5 %>%
      filter(position <= 3, year >= input$year1[1], year <= input$year1[2]) %>%
      ggplot(aes(
        x = year,
        y = position,
        color = country,
        text = paste("country:", country)
      )) +
      geom_bump() +
      geom_point(size = 6) +
      coord_cartesian(xlim = input$year1)  +
      scale_y_reverse(breaks = seq(1, 3, by = 1)) + geom_smooth()
    
  })
  
  
  graph_properties <- list(
    scope = 'world',
    showland = TRUE,
    landcolor = toRGB("white"),
    color = toRGB("white")
  )
  
  font = list(family = "DM Sans",
              size = 15,
              color = "black")
  
  label = list(bgcolor = "#EEEEEE",
               bordercolor = "transparent",
               font = font)
  
  
  
  output$mymap = renderPlotly({
    plot_geo(o_m_6, locationmode = "world",
             frame = ~ Year) %>%
      add_trace(
        locations = ~ Code,
        z = ~ Position,
        zmin = 0,
        zmax = max(o_m_6$Position),
        color = ~ Position,
        colorscale = "Hot ",
        text = ~ Hover,
        hoverinfo = 'text'
      )  %>%
      layout(geo = graph_properties,
             title = "Countries/olympic gamerank \n1896 - 2020",
             font = list(family = "DM Sans")) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  output$myranking= renderPlotly({
    top_10 <- medals %>%
      group_by(Country) %>%
      summarise(Total_Count = sum(Count)) %>%
      top_n(10, Total_Count) %>%
      arrange(desc(Total_Count))
    
    ggplot(
      medals %>% filter(Country %in% top_10$Country, Medal != "Total"),
      aes(
        x = (Country),
        y = Count,
        fill = Medal
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("gold1", "gray70", "gold4", "blue")) +
      ggtitle("Historical medal counts from Competitions") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
  output$Countries= renderPlotly({
    hystoric %>%
      
      ggplot(aes(x = Year, y = Countries)) +
      
      geom_point(size = 3, shape = 21, fill = "white") +
      
      geom_line(color = "gold1", size = 1.5) + 
      
      scale_color_manual(values = c("chocolate", "gold1")) +
      
      labs(
        x = 'Year',
        y = "Number of Nations",
        title = "Number of Nations - Olympic Games from 1896 to 2020",
        subtitle = "Olympic Games from 1896 to 2020",
        caption = "Source: Kaggle"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),  
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  
  
  
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- df
    }
    else{
      dt <- df[, c(input$SelectX)]
    }
    
  })
  
  
  # observe({
  #   lstname <- names(InputDataset())
  #   updateSelectInput(session = session,
  #                     inputId = "SelectY",
  #                     choices = lstname)
  # })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  
  
  output$Summ_old <- renderPrint(skim(InputDataset()))
  
  
  
  
  
  output$structure <- renderDataTable({
    
    options = list(scrollX = TRUE,
                   columnDefs = list(
                     list(className = "dt-left", targets = "_all")
                   ))
    
    datatable(InputDataset())
  })
  
  
  
  set.seed(100)  
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(),]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(), ]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  
  
  
  output$Data <- renderDT({
    datatable(InputDataset(),
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(className = "dt-left", targets = "_all")
                             )))
  })
  
  
  output$Multi <- renderPlot({
    corrplot(
      cor(df %>% dplyr::select(input$Corr), use = "complete.obs"),
      method = 'number',
      order = 'AOE',
      type = 'lower'
    )
  })
  
  
  
  output$Corr <- renderPlot({
    corrplot(
      cor(df %>% dplyr::select(input$SelectY, input$Corr), use = "complete.obs"),
      method = 'ellipse',
      order = 'AOE',
      type = 'lower'
    )
  })
  
  
  output$Plots <- renderPlot ({
    ggpairs(
      df,
      columns = c(input$Corr,input$SelectY ),
      mapping = ggplot2::aes(color = Country_status),
      lower = list(continuous = 'points'),
      axisLabels = 'none',
      upper = list(continuous = 'blank'),
      cardinality_threshold = 211
    )
  })
  

  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~", paste(input$Corr, collapse = "+")))
  })
  
  
  Linear_Model <- reactive({
    region <- input$Region
    
    if (region == 'All') {
      return(lm(f(), data = df))
    }
    
    df_filtered <- df %>%
      filter(Country_status == region)
    
    lm(f(), data = df_filtered)
  })
  
  output$Model <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = 'text',
        digits = 2,
        title = 'All',
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )
  
  
  
  output$text1 <- renderUI({ 
    HTML(
      paste(
        "<center><h3> Model using search method for feature selection stepwise regression function 
      stepAIC <h3>", "<h6> Total ~ Land_area_km2 + Armed_forces_size + Birth_rate + Co2_emissions +
      Fertility_rate + Population + log_Agricultural_land_percent +
      log_Birth_rate + log_Fertility_rate + log_Gasoline_price +
      log_Gdp </h6></center>" )
    )
  })
  
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  
  Linear_Model2 <- reactive({
    region <- input$Region
    
    
    df_filtered2 <- df %>%
      filter(Country_status == 'Developed')
    
    lm(f(), data = df_filtered2)
  })
  
  
  Linear_Model3 <- reactive({
    region <- input$Region
    
    
    df_filtered3 <- df %>%
      filter(Country_status == 'Underdeveloped')
    
    lm(f(), data = df_filtered3)
  })
  
  output$Model2 <-
    renderPrint(
      stargazer(
        Linear_Model2(),
        Linear_Model3() ,
        type = 'text',
        digits = 2,
        title = paste('Developed', '          ', 'Underdeveloped'),
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )

})
