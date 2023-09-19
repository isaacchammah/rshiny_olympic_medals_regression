shinyServer(function(input, output, session) {
  #Tab Motivation------------------------------------------------------------------------------------------------------
  
  output$Motivation <- renderUI({
    HTML(
      paste(
        "<center> <h1> <span style='color:brown'>Olympic medals are important</span>    </h1> </center>",
        "<h5> Like it or not, every country, especially the United States, attaches great importance to winning medals, gold medals to be precise, at the Olympic Games, because it is a matter of national pride, symbol of national strength and a good way of commanding respect from others.<h5/>"
      )
    )
  })
  
  
  #Tab Data------------------------------------------------------------------------------------------------------
  

  output$slide = renderPlot({
    o_m_5 %>%
      filter(position <= 4, year >= input$year1[1], year <= input$year1[2], year %% 4 == 0) %>%
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
      ggtitle("Historical total medal position") + 
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )  
  })
  
  
o_m_5%>% filter(year=='2012')  %>% dplyr::select(country, position, total)
  
  
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
             title = list(text = "Countries/olympic gamerank 1896 - 2020", size = 300),
             font = list(family = "DM Sans")) %>%
      config(displayModeBar = FALSE)
  })
  
  medals
  
  output$myranking = renderPlotly({
    medals %>% arrange(desc(Total)) %>% head(30) %>%
      ggplot(aes(x = Country,
                 
                 y = Total,
                 
                 fill = Medal)) +
      
      geom_col() +
      
      coord_flip() +
      
      scale_fill_manual(values = c("gold4", "gold1", "gray70")) +
      
      ggtitle("Historical medal counts from Competitions") +
      
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()
      )    
  })
  
  
  
  
  
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
        '<h2> <center> Multiple linear regression (MLR)</center> </h2>',
        '<p> Multiple linear regression (MLR), also known simply as multiple regression, is a statistical technique that uses several explanatory variables to predict the outcome of a response variable. </p>',
        '<p> The goal of multiple linear regression is to model the linear relationship between the explanatory (independent) variables and response (dependent) variables. In essence, multiple regression is the extension of ordinary least-squares (OLS) regression because it involves more than one explanatory variable.<p/>',
        
        '<p> The objective of this application is to allow users to go through the step-by-step process of multiple linear regression in a didactic and intuitive manner. However, it is necessary to have a background in statistics to handle it in the best possible way. It is a suitable application to be used in the classroom with the purpose of learning how to build an OLS model. </p>',
        
        '<p>It will enable the user to build a multiple linear regression (MLR) model to predict which variables may influence a country to obtain more or fewer medals during the Olympic Games.</p>',
        '<p>The provided dataset refers to the 2020 Summer Olympic Games.</p>',
        '<p>Data was colected from 192 out of the 206 teams that participated in the games </p>',
        
        '<b>Tabs </b>',
        
        "<p style='color:brown'> <b>Variables</b>: The user will see available independent and dependent variables in the dataset</p>",
        "<p style='color:brown'><b> Data</b>: Displays all available data, can be filtered for developed or developing countries</p>",
        "<p style='color:brown'><b>Data Summary</b>: Provides a summary of the variables</p>",
        "<p style='color:brown'><b>Multicollinearity</b>: According to the selected independent variables, it shows the correlation matrix between them. It\'s important to note that the correlation should not be high; otherwise, the model won\'t be correct.</p>",
        "<p style='color:brown'><b>Plots</b>: The first graph displays the correlation matrix between all independent variables and the dependent variable. The second graph illustrates this relationship and shows if the same is linear.</p>",
        "<p style='color:brown'><b>Model</b>: MLR Model according to the chosen region and graphs to check for homoscedastic errors, normally distributed errors, and non-autocorrelated errors.</p>",
        "<p style='color:brown'><b>Model: Developed x Underdeveloped Countries</b>: A model that compares a regression done only with developed countries and another done only with developing countries.</p>",
        "<p style='color:brown'><b>Model Selection</b>: Model created using the  Aikake Information Criterion (AIC) is an estimator of prediction error. The AIC awards the lowest score to a model possessing the least loss of information (or that with most predictive power) while minimizing the number of predictor variables.</p>",
        "<p style='color:brown'><b>Conclusion</b>: Brief discussion of the results found </p>"
        
      )
    )
  })
  
  
  output$Variables <-
    renderUI({
      HTML(
        paste(
          "<b>  Country Status </b>",
          "<ul>",
          "<li style='color:red'> Developed: GDP/capta in the country is higher then $12,000 </li>",
          "<li style='color:red'> Underdeveloped: GDP/capta in the country is lower then $12,000  </li>",
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
          "<li style='color:green'>Land Area (Km2): Total land area of the country in square kilometers divided by 10^6.</li>",
          "<li style='color:green'>Armed Forces Size: Size of the armed forces in the country.</li>",
          "<li style='color:green'>Birth Rate: Number of births per 1,000 population per year.</li>",
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
          "<li style='color:green'>Out of Pocket Health Expenditure (%): Percentage of total health expenditure paid out-of-pocket by individuals.</li>",
          "<li style='color:green'>Physicians per Thousand: Number of physicians per thousand people.</li>",
          "<li style='color:green'>Population: Total population of the country divided by 10^6.</li>",
          "<li style='color:green'>Population: Labor Force Participation (%): Percentage of the population that is part of the labor force.</li>",
          "<li style='color:green'>Tax Revenue (%): Tax revenue as a percentage of GDP.</li>",
          "<li style='color:green'>Total Tax Rate: Overall tax burden as a percentage of commercial profits.</li>",
          "<li style='color:green'>Unemployment Rate: Percentage of the labor force that is unemployed.</li>",
          "<li style='color:green'>Urban Population: Percentage of the population living in urban areas.</li>",
          "<li style='color:green'>Country Status: If the GDP/capta was lower then $12,000 the country was considered Underdeveloped, else Developed </li>",
          "<li style='color:gray'>All independent variables can also be chosen in their log form</li>",
          "</ul>",
          collapse = "\n"
        )
      )
    })
  
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
    HTML(
      paste(
        "<p> Data table for the selected region </p>"
        
      ))   
  }) 
  
  
  
  output$Data <- renderDT({
    datatable(InputDataset(),
              options = list(scrollX = TRUE,
                             columnDefs = list(
                               list(className = "dt-left", targets = "_all")
                             )))
  })
  
  
  
  
  output$Summtext <- renderUI({
    HTML(
      paste(
        "<p> Data summary for the selected region</p>"
        
      ))   
  }) 
  
  
  #summarizing the data
  output$Summ_old <- renderPrint(skim(InputDataset()))
  
  skim(df)
  
  
  output$Multitext <- renderUI({
    HTML(
      paste(
        "<p> Correlation matrix between the chosen X variables </p>"

      ))   
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
    HTML(
      paste(
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
        "<p> If all regions are selected Red represents Developed countries and Blues represents Undeveloped countries</p>"
        
      ))   
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
    HTML(
      paste(
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
    HTML(
      paste(
        "<p> Residual plots from the model created</p>"
      ))   
  }) 
  
  
  #General Model residual plots
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model())
    par(mfrow = c(1, 1))
    
  })
  
  output$Developedtext <- renderUI({
    HTML(
      paste(
        "<p> Residual plots from the model created comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option All regions is selected </p>"
        
      ))   
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
        "<p> Residual plots from the model created comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option All regions is selected </p>"
        
      ))   
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
    HTML(
      paste(
        "<p> Residual plots from the model created comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option All regions is selected </p>"
        
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
        "<p> Residual plots from the model created comparing Developed and Undeveloped countries</p>",
        "<p> This only functions if the option All regions is selected </p>"
        
      ))   
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
    HTML(
      paste(
        "<p> Model using AIC</p>"
        
      ))   
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
    HTML(
      paste(
        "<p> Model using AIC</p>"
        
      ))   
  }) 
  
  
  
  #AIC residual plot
  output$residualPlots4 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model4())
    par(mfrow = c(1, 1))
    
  })
  
  
  best_model = (
    lm(
      Total ~ Density_p_km2 + Land_area_km2 + Armed_forces_size + Co2_emissions +
        Gasoline_price + Infant_mortality + Life_expectancy + Maternal_mortality_ratio +
        Population + Population_labor_force_participation_percent +
        Unemployment_rate + Urban_population + Gdp_capta + log_Density_p_km2 +
        log_Agricultural_land_percent + log_Birth_rate + log_Fertility_rate +
        log_Gasoline_price + log_Gdp + log_Infant_mortality + log_Maternal_mortality_ratio +
        log_Out_of_pocket_health_expenditure_percent + log_Physicians_per_thousand +
        log_Population + log_Urban_population + log_Gdp_capta,
      data = df
    )
  )
  
  
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
        "<p> A very simple model with a high R^2 would be to use the variables: Land_area_km2, Gdp, Population, and log_Infant_Mortality.
        However, this model does not satisfy the condition that the mean residual value for every fitted value region is
        close to 0, as the red line on the residual vs. fitted plot does not remain at zero.</p>",
        "</br>",
        
        "<p>Therefore, the best model selected to predict the total amount of medals won by
        all countries was the one created with the AIC estimator. The assumptions for multiple linear
        regression were considerably satisfied, except for multicollinearity, which was slightly present in
        the model.</p>",
        
        "<p>",
        "<strong>Best Model:</strong> ",
        "Total = 11.3310 ",
        "+ <span style='color:green'>1.8687 * Land_area_km2</span> ",
        "+ <span style='color:green'>0.00001 * Armed_forces_size</span> ",
        "+ <span style='color:green'>0.00001 * Co2_emissions</span> ",
        "- <span style='color:red'>0.0580 * Population</span> ",
        "+ <span style='color:green'>1.4158 * log_Agricultural_land_percent</span> ",
        "- <span style='color:red'>14.6936 * log_Birth_rate</span> ",
        "+ <span style='color:green'>14.0683 * log_Fertility_rate</span> ",
        "+ <span style='color:green'>5.2804 * log_Gasoline_price</span> ",
        "+ <span style='color:green'>2.0094 * log_Gdp</span> ",
        "- <span style='color:red'>1.5553 * log_Out_of_pocket_health_expenditure_percent</span>",
        "</p>",
        "<p>",
        "Interpretation:",
        "</p>",
        '<p> 78,65% of the variability of the total number of medals won by all countries in the 2020 summer olympics is explained by the 10 variables listed bellow </p>',
        "<ol>",
        "<li>Land_area_km2: For each million square-kilometer <span style='color:green'>increase</span> in land area, there is an average <span style='color:green'>increase</span> of 1.8687 medals won, keeping everything else constant</li>",
        "<li>Armed_forces_size: For every one unit <span style='color:green'>increase</span> in the size of the armed forces, there is an average <span style='color:green'>increase</span> of 0.00001 units in medals won, keeping everything else constant</li>",
        "<li>Co2_emissions: With every one ton <span style='color:green'>increase</span> in carbon dioxide emissions, there is an average <span style='color:green'>increase</span> of 0.00001 medals won, keeping everything else constant</li>",
        "<li>Population: For each <span style='color:green'>increase</span>  in one million of the population, there is an average <span style='color:red'>decrease</span> of 0.0580 medals won, keeping everything else constant</li>",
        
        "<li>log_Agricultural_land_percent: An <span style='color:green'>increase</span> of one percent of the percentage of land used for agricultural purposes results in an average <span style='color:green'>increase</span> of 0.014158 medals won, keeping everything else constant</li>",
        
        "<li>log_Birth_rate: A one percent <span style='color:green'>increase</span> of the birth rate is associated with an average <span style='color:red'>decrease</span> of 0.146936 medals won, keeping everything else constant",
        
        "<li>log_Fertility_rate: A one percent <span style='color:green'>increase</span> of the fertility rate leads to an average <span style='color:green'>increase</span> of 0.140683 medals won, keeping everything else constant </li>",
        
        "<li>log_Gasoline_price: With each one percent <span style='color:green'>increase</span> in the natural logarithm of the gasoline price, there is an average <span style='color:green'>increase</span> of 0.052804 medals won, keeping everything else constant</li>",
        
        "<li>log_Gdp: A one percent <span style='color:green'>increase</span> of the GDP results results in an average <span style='color:green'>increase</span> of 0.020094 medals won, keeping everything else constant </li>",
        
        "<li>log_Out_of_pocket_health_expenditure_percent: An <span style='color:green'>increase</span> of one percent in the of the percentage of out-of-pocket health expenditure is associated with an average <span style='color:red'>decrease</span> of 0.015553 medals won, keeping everything else constant</li>",
        "</ol>"
      )
    )
  })
  
  
  
  
  
  
})
