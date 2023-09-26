shinyServer(function(input, output, session) {
  #Tab Motivation------------------------------------------------------------------------------------------------------
  
  output$Motivation <- renderUI({
    HTML(
      paste(
        "<center><h2><span style='color:brown'>The Significance of Olympic Medals</span></h2></center>",
        "<h5>Whether we like it or not, winning medals, especially gold medals, at the Olympic Games holds immense importance for every country, including the United States. A medal is a source of national pride that reflects strength and commands respect from others.</h5>",
        "<p>This app consists of two sections:</p>",
        "<ol>",
        "<li>Data: Four graphs display information on the modern Olympic Summer Games.</li>",
        "<li>Regression: Users can create their own linear regression model to understand which variables influence a country's medal count in the 2020 Summer Olympics.</li>",
        "</ol>"
      )
    )
  })
  
  
  #Tab Data------------------------------------------------------------------------------------------------------
  
  output$slide = renderPlot({
    o_m_5 %>%
      filter(position <= 4,
             year >= input$year1[1],
             year <= input$year1[2],
             year %% 4 == 0) %>%
      ggplot(aes(
        x = year,
        y = position,
        color = country,
        text = paste("country:", country)
      )) +
      geom_bump() +
      geom_point(size = 6) +
      coord_cartesian(xlim = input$year1)  +
      scale_y_reverse(breaks = seq(1, 4, by = 1)) +
      scale_x_continuous(breaks = seq(input$year1[1], input$year1[2], by = 4)) +
      geom_smooth() +
      scale_color_viridis(discrete = TRUE) +  # Use a paleta de cores viridis
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )  +
      labs(x = 'Year' , y = 'Position', title = "Top 4 historical total medal position")
  })
  
  
  o_m_5 %>% filter(year == '2012')  %>% dplyr::select(country, position, total)
  
  
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
        colorscale = "Greys",
        colorbar = list(thickness = 30, len = 1),
        text = ~ Hover,
        hoverinfo = 'text'
      )  %>%
      layout(
        geo = graph_properties,
        title = list(text = "<b>Countries Olympic total medal position 1896 - 2020</b>", size = 300),
        font = list(family = "DM Sans", size = 13.5),
        size = 300
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  medals = o_m_6 %>%
    dplyr::select(Country, Gold, Silver, Bronze, Total) %>%
    group_by(Country) %>%
    summarise(
      Gold = sum(Gold),
      Silver = sum(Silver),
      Bronze = sum(Bronze),
      Total = sum(Total)
    )  %>%
    pivot_longer(
      cols = c(Gold, Silver, Bronze),
      names_to = "Medal",
      values_to = "Count"
    ) %>%
    filter(Medal != 'Total')
  
  output$myranking = renderPlotly({
    medals %>% arrange(desc(Total)) %>% head(30) %>%
      ggplot(aes(
        x = reorder(Country, Count),
        
        y = Count,
        
        fill = Medal
      )) +
      
      geom_col() +
      
      coord_flip() +
      
      scale_fill_manual(values = c("gold4", "gold1", "gray70")) +
      
      ggtitle("Historical medal counts from Competitions") +
      
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )    +
      labs (x = 'Country' , y = 'Medals', title = "Historical medal counts from Competitions")
  })
  
  
  
  hystoric = sports %>% mutate (Summer =  gsub("[[:digit:]]", "", edition),
                                Year = parse_number(edition)) %>%
    filter (Summer == ' Summer Olympics')  %>%  group_by(Year) %>% summarise(Countries =
                                                                               n_distinct(country_noc))
  
  
  
  output$Countries = renderPlotly({
    hystoric %>%
      
      ggplot(aes(x = Year, y = Countries)) +
      
      geom_point(size = 3,
                 shape = 21,
                 fill = "white") +
      
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
  
  
  #Tab Regression------------------------------------------------------------------------------------------------------
  
  
  observeEvent(input$Region, {
    updatePickerInput(
      session = session,
      inputId = "Region",
      options = pickerOptions(selectedTextFormat = 'values')
    )
  }, ignoreInit = TRUE)
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- df
    }
    else{
      dt <- df[, c(input$SelectX)]
    }
  })
  
  
  output$How <- renderUI({
    HTML(
      paste(
        '<h2><center>Multiple Linear Regression (MLR)</center></h2>',
        '<p>Multiple Linear Regression (MLR) is a statistical technique that uses multiple explanatory variables to predict the outcome of a response variable.</p>',
        '<p>The goal of MLR is to model the linear relationship between the independent variables and dependent variables. It extends ordinary least-squares (OLS) regression by involving more than one explanatory variable.</p>',
        '<p>This application allows users to go through the step-by-step process of MLR in a didactic and intuitive manner. Some background in statistics is recommended for optimal use. It is suitable for classroom exercises for students learning to build an OLS model.</p>',
        '<p>The application enables users to build an MLR model to predict variables that may influence the medal count of a country in the Olympic Games.</p>',
        '<p>The provided dataset is from the 2020 Summer Olympic Games, collected from 192 out of 206 participating teams.</p>',
        '<b>Tabs:</b>',
        "<p style='color:brown'><b>Variables:</b> Displays available describtion of independent and dependent variables in the dataset.</p>",
        "<p style='color:brown'><b>Data:</b> Shows all available data and can be filtered for developed or developing countries.</p>",
        "<p style='color:brown'><b>Data Summary:</b> Provides a summary of the variables.</p>",
        "<p style='color:brown'><b>Multicollinearity:</b> Shows the correlation matrix between selected independent variables. High correlation may affect model accuracy.</p>",
        "<p style='color:brown'><b>Plots:</b> The first graph displays the correlation matrix between independent variables and the dependent variable. The second graph shows the linearity of this relationship.</p>",
        "<p style='color:brown'><b>Model:</b> Builds an MLR model based on selected region and checks for homoscedastic errors, normally distributed errors, and non-autocorrelated errors.</p>",
        "<p style='color:brown'><b>Model: Developed x Underdeveloped Countries:</b> Compares regression models for developed and developing countries.</p>",
        "<p style='color:brown'><b>Model Selection:</b> Creates a model using the Akaike  Information Criterion (AIC) to estimate prediction error and select the best model with optimal predictive power and minimal predictor variables.</p>",
        "<p style='color:brown'><b>Conclusion:</b> Provides a brief discussion of the results found.</p>"
        
      )
    )
  })
  
  
  output$Variables <-
    renderUI({
      HTML(
        paste(
          "<b>  Country Status </b>",
          "<ul>",
          "<li style='color:red'> Developed: GDP/capita in the country is higher than $12,000 </li>",
          "<li style='color:red'> Underdeveloped: GDP/capita in the country is lower than $12,000  </li>",
          "</ul>",
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
          "<li style='color:green'>Density (P/Km2): Population density measured in persons per square kilometer.</li>",
          "<li style='color:green'>Agricultural Land (%): Percentage of land area used for agricultural purposes.</li>",
          "<li style='color:green'>Land Area (Km2): Total land area of the country in square kilometers divided by 10<sup>6</sup> .</li>",
          "<li style='color:green'>Armed Forces Size: Size of the armed forces in the country.</li>",
          "<li style='color:green'>Birth Rate: Number of births per 1,000 population per year.</li>",
          "<li style='color:green'>CO2 Emissions: Carbon dioxide emissions in tons.</li>",
          "<li style='color:green'>CPI: Consumer Price Index, a measure of inflation and purchasing power.</li>",
          "<li style='color:green'>CPI Change (%): Percentage change in the Consumer Price Index compared to the previous year.</li>",
          "<li style='color:green'>Currency_Code: Currency code used in the country.</li>",
          "<li style='color:green'>Fertility Rate: Average number of children born to a woman during her lifetime.</li>",
          "<li style='color:green'>Forested Area (%): Percentage of land area covered by forests.</li>",
          "<li style='color:green'>Gasoline_Price: Price of gasoline per liter in local currency.</li>",
          "<li style='color:green'>GDP: Gross Domestic Product, the total value of goods and services produced in the country in USD.</li>",
          "<li style='color:green'>Gross Primary Education Enrollment (%): Gross enrollment ratio for primary education.</li>",
          "<li style='color:green'>Gross Tertiary Education Enrollment (%): Gross enrollment ratio for tertiary education.</li>",
          "<li style='color:green'>Infant Mortality: Number of deaths per 1,000 live births before reaching one year of age.</li>",
          "<li style='color:green'>Largest City: Name of the country's largest city.</li>",
          "<li style='color:green'>Life Expectancy: Average number of years a newborn is expected to live.</li>",
          "<li style='color:green'>Maternal Mortality Ratio: Number of maternal deaths per 100,000 live births.</li>",
          "<li style='color:green'>Minimum Wage: Minimum wage level in local currency.</li>",
          "<li style='color:green'>Out of Pocket Health Expenditure (%): Percentage of total health expenditure paid out-of-pocket by individuals.</li>",
          "<li style='color:green'>Physicians per Thousand: Number of physicians per thousand people.</li>",
          "<li style='color:green'>Population: Total population of the country divided by 10<sup>6</sup> .</li>",
          "<li style='color:green'>Population: Labor Force Participation (%): Percentage of the population that is part of the labor force.</li>",
          "<li style='color:green'>Tax Revenue (%): Tax revenue as a percentage of GDP.</li>",
          "<li style='color:green'>Total Tax Rate: Overall tax burden as a percentage of commercial profits.</li>",
          "<li style='color:green'>Unemployment Rate: Percentage of the labor force that is unemployed.</li>",
          "<li style='color:green'>Urban Population: Percentage of the population living in urban areas.</li>",
          " <li style='color:green'>Individual Athletes: Number of athletes competing individually for a country</li>",
          "  <li style='color:green'>Team Athletes: Number of teams competing for a country</li>",
          "  <li style='color:green'>Percentage male individual (%): Percentage of individual athletes who are male</li>",
          " <li style='color:green'>Percentage male team (%): Percentage of teams that are composed of male athletes</li>",
          "<li style='color:green'>GDP/capita:  GDP divided by population size </li>",
          
          "<li style='color:green'>Country Status: If the GDP/capita was lower than $12,000 the country was considered Underdeveloped, else Developed </li>",
          
          "<li style='color:gray'>All independent variables can also be chosen in their log form</li>",
          "</ul>",
          collapse = "\n"
        )
      )
    })
  names(df)
  #choose type of data to be displayed
  InputDataset <- reactive({
    region <- input$Region
    
    if (region == 'All') {
      return(df)
    }
    
    df %>%
      filter(Country_status == region)
    
  })
  
  
  #slider for data visualization
  #data visualization
  
  
  output$Datatext <- renderUI({
    HTML(paste("<p> Data table for the selected region </p>"))
  })
  
  
  
  output$Data <- renderDT({
    datatable(InputDataset(),
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(className = "dt-left", targets = "_all")
                             )))
  })
  
  
  
  
  output$Summtext <- renderUI({
    HTML(paste("<p> Data summary for the selected region</p>"))
  })
  
  
  #summarizing the data
  output$Summ_old <- renderPrint(skim(InputDataset()))
  
  skim(df)
  
  
  output$Multitext <- renderUI({
    HTML(paste("<p> Correlation matrix between the chosen X variables </p>"))
  })
  
  
  #Multicolliniarity - Correlation matrix between x variables
  output$Multi <- renderPlot({
    corrplot(
      cor(InputDataset() %>% dplyr::select(input$Corr), use = "complete.obs"),
      method = 'number',
      order = 'AOE',
      type = 'lower'
    )
  })
  
  
  output$Corrtext <- renderUI({
    HTML(paste(
      "<p> Correlation matrix between chosen x variables and y variable </p>"
      
    ))
  })
  
  
  #Correlation matrix between x variables and y variable
  output$Corr <- renderPlot({
    corrplot(
      cor(
        InputDataset() %>% dplyr::select(input$SelectY, input$Corr),
        use = "complete.obs"
      ),
      method = 'ellipse',
      order = 'AOE',
      type = 'lower'
    )
  })
  
  
  output$Plotstext <- renderUI({
    HTML(
      paste(
        "<p> Scatterplot graphs showing the relashionship of the chosen variables  </p>",
        "<p> If all regions are selected red represents Developed countries and blue represents Undeveloped countries</p>"
        
      )
    )
  })
  
  
  #scatterplot between variables
  output$Plots <- renderPlot ({
    ggpairs(
      InputDataset(),
      columns = c(input$Corr, input$SelectY),
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
  
  
  output$Modeltext <- renderUI({
    HTML(paste(
      "<p> Multiple linear regression between the chosen region and variables</p>"
      
    ))
  })
  
  
  #General Model
  output$Model <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = 'text',
        digits = 4,
        title = 'All',
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )
  
  
  output$Plottext <- renderUI({
    HTML(paste("<p> Residual plots from the model created</p>"))
  })
  
  
  #General Model residual plots
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model())
    par(mfrow = c(1, 1))
    
  })
  
  # output$Error <- renderDT({
  #   datatable(tabela_comparativa(),
  #             options = list(scrollX = TRUE,
  #                            columnDefs = list(
  #                              list(className = "dt-left", targets = "_all")
  #                            )))
  # })
  
  
  output$Developedtext <- renderUI({
    HTML(
      paste(
        "<p> Residual plots from the model created comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option All regions is selected </p>"
        
      )
    )
  })
  
  
  
  
  #Developed X Underdeveloped Model
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
  
  
  output$Developedtext <- renderUI({
    HTML(
      paste(
        "<p> Model comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option 'All regions' is selected </p>"
        
      )
    )
  })
  
  
  output$Model2 <-
    renderPrint(
      stargazer(
        Linear_Model2(),
        Linear_Model3() ,
        type = 'text',
        digits = 4,
        title = paste('Developed    Underdeveloped'),
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )
  
  
  
  output$Developedplottext <- renderUI({
    HTML(paste(
      "<p> Residual plots from the model created for Developed countries</p>",
      
      
    ))
  })
  
  
  #General Model residual plots for developed
  
  output$residualPlots2 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model2())
    par(mfrow = c(1, 1))
    
  })
  
  output$Underdevelopedplottext <- renderUI({
    HTML(
      paste(
        "<p> Residual plots from the model created for Underdeveloped countries</p>",
        
        
      )
    )
  })
  
  
  
  
  #General Model residual plotsfor underdeveloped
  
  output$residualPlots3 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model3())
    par(mfrow = c(1, 1))
    
  })
  
  
  
  #AIC model
  
  Linear_Model4 <- reactive({
    region <- input$Region
    
    if (region == 'All') {
      return(lm(f(), data = df))
    }
    
    df_filtered <- df %>%
      filter(Country_status == region)
    
    lm(f(), data = df_filtered)
  })
  
  
  
  output$AICtext <- renderUI({
    HTML(paste("<p> Model created using AIC</p>"))
  })
  
  
  
  output$Model3 <-
    renderPrint(
      stargazer(
        stepAIC(Linear_Model4()),
        type = 'text',
        digits = 4,
        title = 'All',
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )
  
  
  output$AICplottext <- renderUI({
    HTML(paste("<p> Residual plots from the model created using AIC</p>"))
  })
  
  
  
  #AIC residual plot
  output$residualPlots4 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model4())
    par(mfrow = c(1, 1))
    
  })
  
  
  # best_model = (
  #   lm(
  #     Total ~ Density_p_km2 + Land_area_km2 + Armed_forces_size + Co2_emissions +
  #       Gasoline_price + Infant_mortality + Life_expectancy + Maternal_mortality_ratio +
  #       Population + Population_labor_force_participation_percent +
  #       Unemployment_rate + Urban_population + Gdp_capta + log_Density_p_km2 +
  #       log_Agricultural_land_percent + log_Birth_rate + log_Fertility_rate +
  #       log_Gasoline_price + log_Gdp + log_Infant_mortality + log_Maternal_mortality_ratio +
  #       log_Out_of_pocket_health_expenditure_percent + log_Physicians_per_thousand +
  #       log_Population + log_Urban_population + log_Gdp_capta,
  #     data = df
  #   )
  # )
  
  
  output$Model4 <-
    renderPrint(
      summary(
        stepAIC(best_model),
        type = 'text',
        digits = 4,
        title = 'All',
        style = 'qje',
        out = 'Lm_1.doc'
      )
    )
  #AIC residual plot
  output$residualPlots5 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(best_model)
    par(mfrow = c(1, 1))
    
  })
  
  #Tab Conclusion------------------------------------------------------------------------------------------------------
  
  output$Conclusion <- renderUI({
    HTML(
      paste0(
        '<p>Despite the large number of independent variables, only 4 variables were enough to
        have a model that meets all the assumptions of a multiple linear regression (multicollinearity, linearity, constant variance,
        normality, independent errors) and that presents a high R<sup>2</sup> . The equation shown below has the total number
        of medals won in all countries as the dependent variable, but if the dependent variable were another of the
        possible available selections, the result would be quite similar.</p>',
        
        
        
        "<p>",
        "<strong>Best Model:</strong> ",
        "Total = 3.3663 ",
        "+ <span style='color:green'>0.8814 * Land_area_km2</span> ",
        "+ <span style='color:green'>0.000002 * Gdp</span> ",
        "+ <span style='color:green'>0.1733 * Individual_athletes</span> ",
        "- <span style='color:red'>0.6002 * log_Co2_emissions</span> ",
        "</p>",
        "<p>",
        "Interpretation:",
        "</p>",
        '<p> 92.27% of the variability of the total number of medals won by all countries in the 2020 summer olympics is explained by the 4 variables listed bellow: </p>',
        "<ol>",
        "<li>Land_area_km2: For each million square-kilometer <span style='color:green'>increase</span> in land area, there is an average <span style='color:green'>increase</span> of 0.8814 medals won, keeping everything else constant</li>",
        "<li>Gdp: For every dollar <span style='color:green'>increase</span> in the Gdp, there is an average <span style='color:green'>increase</span> of 0.000002 medals won, keeping everything else constant</li>",
        "<li>Individual_athletes: With every additional athlete participating in the games for an individual sport there is an <span style='color:green'>increase</span> of 0.1733 medals won, keeping everything else constant</li>",
        "<li>log_Co2_emissions: For an 1% <span style='color:green'>increase</span>  in carbon dioxide emissions in tons, there is an average <span style='color:red'>decrease</span> of 0.6002 medals won, keeping everything else constant</li>",
        "</ol>",
        '</br>',
        "<p>However, if the same model were used only for underdeveloped countries, it would not satisfy the assumptions of multiple linear regression, though it would work for developed countries.For underdeveloped countries, a model with only the number of individual athletes already yields an R² of approximately 70%.</p>",
        
        
        
      )
    )
  })
  
  
  
  
  
  
})
