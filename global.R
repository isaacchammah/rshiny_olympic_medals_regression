#Libraries------------------------------------------------------------------------------------------------------


library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(DT)
library(shiny)
library (RColorBrewer)
library(tidygeocoder)
library(readxl)
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
library(plotly)
library(flextable)
library(stargazer)
library(shinyWidgets)
library(MASS)
library(fresh)
library(car)
library(skimr)
library(RSQLite)
library(data.table)
library(forcats)
library(rsconnect)
library(formattable)
library(viridis)


#DF used------------------------------------------------------------------------------------------------------

o_m <- read_csv("Olympic_Games_Medal_Tally.csv")

countries <- read_csv("world-data-2023.csv")

average_latitude_longitude_countries <-
  read_excel("average-latitude-longitude-countries.xlsx")

athlete <- read_csv("Olympic_Athlete_Event_Results.csv")

sports <- read_csv("Olympic_Athlete_Event_Results.csv")

codes <- read_excel("codes.xlsx")

#DF manipulation------------------------------------------------------------------------------------------------------

o_m = o_m %>% mutate (Summer =  gsub("[[:digit:]]", "", edition))
o_m = o_m %>% filter(Summer == " Summer Olympics")

total = o_m %>% group_by(year) %>% summarise(total_games_medal = sum(gold) + sum(silver) + sum(bronze))

o_m_2 = inner_join(o_m, total, by = 'year')
o_m_3 = o_m_2 %>% mutate(rank = total / total_games_medal)

o_m_2020 = o_m_3 %>% filter(year == '2020')

o_m_2020 = o_m_2020 %>%  rename(Country = country)



df = inner_join(o_m_2020, countries, by = 'Country')


average_latitude_longitude_countries$lat = as.numeric(average_latitude_longitude_countries$lat)
average_latitude_longitude_countries$lng = as.numeric(average_latitude_longitude_countries$lng)

o_m_4 = inner_join(o_m_3, average_latitude_longitude_countries, by = 'country')

leaflet(o_m_4 %>% filter(country == o_m_4$country)) %>% addTiles() %>% addMarkers(lat =
                                                                                    ~ lat, lng = ~ lng)
o_m_4 %>% group_by(year) %>% summarise(sum(total))

o_m_5 = (o_m_4 %>% group_by(year) %>% mutate(position = trunc(rank(-rank))))

o_m_5$position = trunc(o_m_5$position)



individual = athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport ==
                                  'FALSE') %>%
  dplyr::group_by (Country) %>% dplyr::summarise(Individual_Athletes = n_distinct(athlete_id))

individual_male =
  athlete %>% filter(edition == "2020 Summer Olympics",
                     isTeamSport == 'FALSE',
                     `Sex` == 'Male') %>%
  dplyr::group_by (Country) %>% dplyr::summarise(Individual_Athletes_Male = n_distinct(athlete_id))

team = athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport ==
                            'TRUE') %>%
  dplyr::group_by (Country) %>% dplyr::summarise(Team_Athletes = n_distinct(event))

team_male = athlete %>% filter(edition == "2020 Summer Olympics",
                               isTeamSport == 'TRUE',
                               `Sex` == 'Male') %>%
  dplyr::group_by (Country) %>% dplyr::summarise(Team_Athletes = n_distinct(event))

athletes = full_join(individual, team, by = "Country")
athletes = full_join(athletes, individual_male , by = "Country")
athletes = full_join(athletes, team_male, by = "Country")

athletes = athletes %>% rename (Team_Athletes = Team_Athletes.x,
                                Team_Athletes_Male = Team_Athletes.y) %>%
  mutate(
    Perc_Male_Individual = (Individual_Athletes_Male / Individual_Athletes) * 100,
    Perc_Male_Team = (Team_Athletes_Male / Team_Athletes) * 100
  )

athletes <- athletes %>% replace(is.na(.), 0)

o_m_5 %>%
  group_by(country) %>%
  summarise(TOTAL = sum(total)) %>%
  arrange(desc(TOTAL))

o_m_6 = inner_join(o_m_5, codes, by = 'country')

o_m_6 = o_m_6  %>% mutate(hover = paste0(country, "\n", position))

df = inner_join(df, athletes, by = 'Country')

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

rows_with_na <- df [!complete.cases(df), ]
print (rows_with_na)

names(df)
names(df) = str_to_title(names(df))

df =  df %>% dplyr::select (
  -Edition_id,
  -Country_noc,
  -Total_games_medal,
  -Summer,
  -Currency_code,
  -Abbreviation,
  -Latitude,
  -Longitude
)

df = df %>% mutate(
  Land_area_km2 = Land_area_km2 / 1000000,
  Population = Population / 1000000,
  Gdp = Gdp / 1
)

df = df %>% mutate (Gdp_capita = ((Gdp / 1000000) / Population))


df <-
  df %>% mutate(Country_status = ifelse(Gdp_capita < 12000, "Underdeveloped", "Developed"))

df[df$Country == "Venezuela", "Country_status"] <- "Underdeveloped"
df[df$Country == "Palestine", "Country_status"] <- "Underdeveloped"

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
  -`Official Language`,
  -Individual_athletes_male,
  -Team_athletes_male
)


ynames =  df %>%  dplyr::select(Rank, Gold, Silver, Bronze, Total)

unique(df$Country_status)

df = df %>% mutate_()

xnames2 = xnames %>% dplyr::select (-Country_status)

df <- df %>%
  mutate(across(all_of(names(xnames2)), ~ log(.x), .names = 'log_{col}'))

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
  -log_Forested_area_percent,
  -Individual_athletes_male,
  -Team_athletes_male
)










#=Prediction table---------------------------------------------------------------------------------------
#
# lm = lm(Total ~ Land_area_km2 + Gdp + Individual_athletes + log_Co2_emissions, data=df)
#
# df2 <- df[complete.cases(df[, c("Land_area_km2", "Gdp", "Individual_athletes", "log_Co2_emissions")]), ]
#
#
# # Dataframe com suas variáveis independentes
# seus_dados = data.frame(x0= df2$Country, x1 = df2$Land_area_km2, x2 = df2$Gdp, x3 = df2$Individual_athletes, x4=df2$log_Co2_emissions )
# seus_dados
#
# # Predições com base no modelo de regressão
# valores_estimados <- predict(lm, data = seus_dados)
#
# # Criar um dataframe com os valores reais e estimados
# tabela_comparativa <- data.frame(Country = df2$Country ,Real = df2$Total, Estimate = valores_estimados)
#
#
# tabela_comparativa = tabela_comparativa %>% mutate('Error' = Real-Estimate)
# tabela_comparativa
#
#
#
# tabela_comparativa= tabela_comparativa %>%
#   mutate_if(is.numeric, ~round(., 2))  %>% mutate(Position = trunc(rank(-Real)))




# tabela_comparativa <- tabela_comparativa[order(tabela_comparativa$Real,decreasing = TRUE), ]
#
# tabela_comparativa <- tabela_comparativa %>%dplyr::select(Position, Country, Real, Estimate, Error)
#
# formattable(tabela_comparativa)
#

# Deploy xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 
# rsconnect::setAccountInfo(name='isaacchammah', token='D41C0B9A69F35706E1F5DABA9671719B', secret='it/rDoNiSa8MPJTYVWeEklBXzzY49P8+3cG/GJk4')
# 
# rsconnect::deployApp('C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics')
