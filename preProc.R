# library(RSQLite)
# library(data.table)
# 
# cvspath1 ="C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Games_Medal_Tally.csv"
# cvspath2 ="C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/world-data-2023.csv"
# cvspath3= "C:/Users/danie/OneDrive/Desktop/DATA SCIENCE/R/Shiny/Olympics/Olympics/Olympic_Athlete_Event_Results.csv"
# 
# 
# data1 <- fread(cvspath1)
# data2 <- fread(cvspath2)
# data3 <- fread(cvspath3)
# tablename1= 'medals'
# tablename2= 'world'
# tablename3= 'athlete'
# 
# dbname= "olympics.sqlite"
# 
# conn <- dbConnect(drv = SQLite(), dbname=dbname)
#         
# dbWriteTable (conn=conn, name =tablename1,value=data1)
# dbWriteTable (conn=conn, name =tablename2,value=data2)
# dbWriteTable (conn=conn, name =tablename3,value=data3)
# 
# dbDisconnect(conn)          

