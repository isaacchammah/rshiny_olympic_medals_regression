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
library(RSQLite)
library(data.table)
library(forcats)
library(rsconnect)





# o_m <-
#   read_csv(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Games_Medal_Tally.csv"
#   )


o_m <-
  read_csv(
    "Olympic_Games_Medal_Tally.csv"
  )



o_m = o_m %>% mutate (Summer =  gsub("[[:digit:]]", "", edition))
o_m = o_m %>% filter(Summer == " Summer Olympics")

total = o_m %>% group_by(year) %>% summarise(total_games_medal = sum(gold) + sum(silver) + sum(bronze))

o_m_2 = inner_join(o_m, total, by = 'year')
o_m_3 = o_m_2 %>% mutate(rank = total / total_games_medal)

o_m_2020 = o_m_3 %>% filter(year == '2020')


# countries <-
#   read_csv(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/world-data-2023.csv"
#   )

countries <-
  read_csv(
    "world-data-2023.csv"
  )





o_m_2020 = o_m_2020 %>%  rename(Country = country)



df = inner_join(o_m_2020, countries, by = 'Country')

paises = as.data.frame(unique(o_m_3$country))

# average_latitude_longitude_countries <-
#   read_excel(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/average-latitude-longitude-countries.xlsx"
#   )

average_latitude_longitude_countries <-
read_excel(
  "average-latitude-longitude-countries.xlsx"
)



average_latitude_longitude_countries$lat = as.numeric(average_latitude_longitude_countries$lat)
class(average_latitude_longitude_countries$lat)
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


# athlete <-
#   read_csv(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Athlete_Event_Results.csv"
#   )


athlete <-
  read_csv(
    "Olympic_Athlete_Event_Results.csv"
  )





individual = athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport=='FALSE') %>% 
  dplyr::group_by (Country) %>% dplyr::summarise(Individual_Athletes = n_distinct(athlete_id))

individual

individual_male =
 athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport=='FALSE', `Sex` =='Male') %>% 
  dplyr::group_by (Country) %>% dplyr::summarise(Individual_Athletes_Male = n_distinct(athlete_id))


team =athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport=='TRUE') %>% 
  dplyr::group_by (Country) %>% dplyr::summarise( Team_Athletes = n_distinct(event))

team_male =athlete %>% filter(edition == "2020 Summer Olympics", isTeamSport=='TRUE',`Sex` =='Male' ) %>% 
  dplyr::group_by (Country) %>% dplyr::summarise( Team_Athletes = n_distinct(event))



athletes = inner_join(individual, team, by="Country")
athletes = inner_join(athletes,individual_male ,by="Country")
athletes = inner_join(athletes,team_male,by="Country")

athletes = athletes %>% rename (Team_Athletes = Team_Athletes.x,
                                Team_Athletes_Male = Team_Athletes.y) %>%
  mutate(
    Perc_Male_Individual = (Individual_Athletes_Male / Individual_Athletes) * 100,
    Perc_Male_Team = (Team_Athletes_Male / Team_Athletes) * 100
  ) 



o_m_5 %>%
  group_by(country) %>%
  summarise(TOTAL = sum(total)) %>%
  arrange(desc(TOTAL))



# codes <-
#   read_excel(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/codes.xlsx"
#   )


codes <-
  read_excel(
    "codes.xlsx"
  )





o_m_6 = inner_join(o_m_5, codes, by = 'country')

o_m_6 = o_m_6  %>% mutate(hover = paste0(country, "\n", position))


df = inner_join(df, athletes,by ='Country')

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
dim(df)

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
  Gdp = Gdp / 1000000
)

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
    cols = c(Gold, Silver, Bronze),
    names_to = "Medal",
    values_to = "Count"
  ) %>% 
  filter(Medal!='Total') 


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

names(df)

ynames =  df %>%  dplyr::select(Rank, Gold, Silver, Bronze, Total)

unique(df$Country_status)

df = df %>% mutate_()

names(xnames)

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


# sports <-
#   read_csv(
#     "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Athlete_Event_Results.csv"
#   )

sports <-
  read_csv(
    "Olympic_Athlete_Event_Results.csv"
  )


hystoric = sports %>% mutate (Summer =  gsub("[[:digit:]]", "", edition),
                              Year = parse_number(edition)) %>%
  filter (Summer == ' Summer Olympics')  %>%  group_by(Year) %>% summarise(Countries =
                                                                             n_distinct(country_noc))







#ano de 2012??
# tirar a virgula do slider
#medalhas - filtro
#trocar