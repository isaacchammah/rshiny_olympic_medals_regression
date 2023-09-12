library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(DT)
library(shiny)
library(leaflet)
library (RColorBrewer)
library(tidygeocoder)
library(readxl)
library(maps)
library(ggbump)
library(corrplot)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(rio)
library(DT)
library(varImp)
library(sjPlot)
library(lme4)
library(sjlabelled)
library(broom)
library(sf)
library(maps)
library(plotly)
library(flextable)
library(stargazer)
library(shinyWidgets)
library(MASS)
library(fresh)
library(car)
library(skimr)

o_m <-
  read_csv(
    "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Games_Medal_Tally.csv"
  )

# View(o_m)
# head(o_m)

o_m = o_m %>% mutate (Summer =  gsub("[[:digit:]]", "", edition))
# unique(o_m$Summer)
o_m = o_m %>% filter(Summer == " Summer Olympics")

# print(o_m, n=20)
total = o_m %>% group_by(year) %>% summarise(total_games_medal = sum(gold) + sum(silver) + sum(bronze))
# total

o_m_2 = inner_join(o_m, total, by = 'year')
# print(o_m_2,n=20)
o_m_3 = o_m_2 %>% mutate(rank = total / total_games_medal)
o_m_3
# unique(o_m_3$year)

o_m_2020 = o_m_3 %>% filter(year == '2020')
# view(o_m_2020)

# names(o_m_2020)

countries <-
  read_csv(
    "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/world-data-2023.csv"
  )

o_m_2020 = o_m_2020 %>%  rename(Country = country)



df = inner_join(o_m_2020, countries, by = 'Country')


view(df)



paises = as.data.frame(unique(o_m_3$country))

# write.csv(paises,
# "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/latlon.csv",
# row.names = FALSE)

average_latitude_longitude_countries <-
  read_excel(
    "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/average-latitude-longitude-countries.xlsx"
  )
average_latitude_longitude_countries$lat = as.numeric(average_latitude_longitude_countries$lat)
class(average_latitude_longitude_countries$lat)
# view(average_latitude_longitude_countries)
average_latitude_longitude_countries$lng = as.numeric(average_latitude_longitude_countries$lng)

o_m_4 = inner_join(o_m_3, average_latitude_longitude_countries, by = 'country')

names(o_m_4)

leaflet(o_m_4 %>% filter(country == o_m_4$country)) %>% addTiles() %>% addMarkers(lat =
                                                                                    ~ lat, lng = ~ lng)

unique(o_m_4$year)

o_m_4 %>% group_by(year) %>% summarise(sum(total))

o_m_5 = (o_m_4 %>% group_by(year) %>% mutate(position = rank(-rank)))

o_m_5 %>% filter(position <= 3) %>%
  ggplot(aes(x = year, y = position, color = country)) +
  geom_bump() +
  geom_point(size = 6)


sports <-
  read_csv(
    "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Athlete_Event_Results.csv"
  )


sports_2020 = sports %>% filter(edition == "2020 Summer Olympics" ,
                                medal == c('Gold', 'Silver', 'Bronze'))



o_m_5 %>%
  group_by(country) %>%
  summarise(TOTAL = sum(total)) %>%
  arrange(desc(TOTAL))



codes <-
  read_excel(
    "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/codes.xlsx"
  )

o_m_6 = inner_join(o_m_5, codes, by = 'country')

o_m_6 = o_m_6  %>% mutate(hover = paste0(country, "\n", position))



df = df %>% mutate_at(
  c(
    'Land_Area_km2',
    'Armed_Forces_size',
    'CPI_Change_percent',
    'Forested_Area_percent',
    'Tax_revenue_percent',
    'Latitude',
    'Longitude'
  ),
  as.numeric
)

names(df)
sum(is.na(df))
# df = na.omit(df)
dim(df)

rows_with_na <- df [!complete.cases(df),]
print (rows_with_na)

names(df)
names(df) = str_to_title(names(df))

df =  df %>% dplyr::select (
  -Edition_id,
  -Country_noc,
  -Total_games_medal,
  -Summer,
  -Currency_code,
  -Abbreviation
)


df = df %>% mutate(Land_area_km2=Land_area_km2/1000000, Population=Population/1000000, Gdp = Gdp/1000000 ) 


df = df %>% mutate (Gdp_capta = (Gdp / Population))


df <-
  df %>% mutate(Country_status = ifelse(Gdp_capta < 12000, "Underdeveloped", "Developed"))

df[df$Country == "Venezuela", "Country_status"] <- "Underdeveloped"
df[df$Country == "Palestine", "Country_status"] <- "Underdeveloped"


names(o_m_6) = str_to_title(names(o_m_6))




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
    cols = c(Gold, Silver, Bronze, Total),
    names_to = "Medal",
    values_to = "Count"
  )



xnames =  df %>%  dplyr::select(
  -Rank,
  -Gold,
  -Silver,
  -Bronze,
  -Total,
  -Edition,
  -Year,
  -Country,
  -Calling_code,
  -Capital_major_city,
  -Largest_city,
  -`Official Language`
)


ynames =  df %>%  dplyr::select(Rank, Gold, Silver, Bronze, Total)

unique(df$Country_status)

df = df %>% mutate_()

names(xnames)

xnames2 = xnames %>% dplyr::select (-Country_status)

df <- df %>% 
  mutate(across(all_of(names(xnames2)), ~log(.x), .names = 'log_{col}'))



xnames3 =  df %>%  dplyr::select(
  -Rank,
  -Gold,
  -Silver,
  -Bronze,
  -Total,
  -Edition,
  -Year,
  -Country,
  -Calling_code,
  -Capital_major_city,
  -Largest_city,
  -`Official Language`,
  -log_Forested_area_percent
)




# Total ~ Density_p_km2 + Land_area_km2 + Armed_forces_size + Birth_rate + 
#   Co2_emissions + Fertility_rate + Gasoline_price + Infant_mortality + 
#   Life_expectancy + Maternal_mortality_ratio + Population + 
#   Population_labor_force_participation_percent + Unemployment_rate + 
#   Urban_population + Latitude + Longitude + Gdp_capta + log_Density_p_km2 + 
#   log_Agricultural_land_percent + log_Birth_rate + log_Fertility_rate + 
#   log_Gasoline_price + log_Gdp + log_Infant_mortality + log_Life_expectancy + 
#   log_Maternal_mortality_ratio + log_Physicians_per_thousand + 
#   log_Population + log_Urban_population + log_Gdp_capta
# 
# Total ~ Land_area_km2 + Armed_forces_size + Birth_rate + Co2_emissions + 
#   Fertility_rate + Population + log_Agricultural_land_percent + 
#   log_Birth_rate + log_Fertility_rate + log_Gasoline_price + 
#   log_Gdp


summary(lm(
  Total ~ Land_area_km2 + Armed_forces_size + Birth_rate + Co2_emissions +
    Fertility_rate + Population + log_Agricultural_land_percent +
    log_Birth_rate + log_Fertility_rate + log_Gasoline_price +
    log_Gdp, data=df
))


df =
  df %>% mutate(Has_medal= ifelse(Total >0, 1, 0))

logit.overall =
  glm(
    Has_medal ~ Land_area_km2 +
      Armed_forces_size +
      Birth_rate +
      Co2_emissions +
      Fertility_rate +
      Population +
      log_Agricultural_land_percent +
      log_Birth_rate +
      log_Fertility_rate +
      log_Gasoline_price +
      log_Gdp,
    data = df,
    family = "binomial"
  )


newdata =
  with(df, data.frame (
    Land_area_km2 = mean(Land_area_km2) ,
    Armed_forces_size = mean(Armed_forces_size) ,
    Birth_rate = mean(Birth_rate) ,
    Co2_emissions = mean(Co2_emissions) ,
    Fertility_rate= mean(Fertility_rate) ,
    Population = mean(Population) ,
    g_Agricultural_land_percent=  mean(log_Agricultural_land_percent) ,
    log_Birth_rate= mean(log_Birth_rate) ,
    log_Fertility_rate= mean(log_Fertility_rate) ,
    log_Gasoline_price= mean(log_Gasoline_price) ,
    log_Gdp= mean(log_Gdp)
  ))



# o_m_6 %>% group_by(year) %>
  
hystoric = sports %>% mutate (Summer =  gsub("[[:digit:]]", "", edition), Year = parse_number(edition)) %>% 
 filter (Summer==' Summer Olympics')  %>%  group_by(Year) %>% summarise(Countries =n_distinct(country_noc ))

hystoric %>%
  
  ggplot(aes(x = Year, y = Countries)) +
  
  geom_point(size = 3, shape = 21, fill = "white") +
  
  geom_line(size = 1.5) +
  
  scale_color_manual(values = c("chocolate","deepskyblue4")) + 
  
  labs(
    x = NULL,
    y = "Number of Nations",
    title = "Nations, Athletes and Events",
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

skim(df)


# Gdp                                        0.0000***               
#   (0.0000)               
# 
# Population                                  -0.01**                
#   (0.01)                
# 
# Country_statusUnderdeveloped                -3.75***               
#   (1.29) 