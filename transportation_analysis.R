library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(ggrepel)
library(plyr)


# Custom Functions

source("utils_library.R")

us_censusdata_transportation = read_csv2("us_censusdata_transportation.csv")
View(us_censusdata_transportation)

# Split data into CA and NY

us_censusdata_transportation$state = trimws(us_censusdata_transportation$state)
us_censusdata_transportation_ca = subset(us_censusdata_transportation,us_censusdata_transportation$state == "California")

us_censusdata_transportation_ny = subset(us_censusdata_transportation,us_censusdata_transportation$state != "California")

View(us_censusdata_transportation_ca)
View(us_censusdata_transportation_ny)


# Creating a table to calcualte aggregate

us_censusdata_transportation_sum_ca = us_censusdata_transportation_ca[-(1:2)]
us_censusdata_transportation_sum_ny = us_censusdata_transportation_ny[-(1:2)]

View(us_censusdata_transportation_sum_ca)

View(us_censusdata_transportation_sum_ny)

# Do aggregation

us_censusdata_transportation_sum_ca = colSums(us_censusdata_transportation_sum_ca)

us_censusdata_transportation_sum_ny = colSums(us_censusdata_transportation_sum_ny)

View(us_censusdata_transportation_sum_ca)

View(us_censusdata_transportation_sum_ny)

View(us_censusdata_transportation_ca_aggregate)
View(us_censusdata_transportation_ny_aggregate)

us_censusdata_race_ca_aggregate = ldply(us_censusdata_race_ca_sum,data.frame)

us_censusdata_race_ca_aggregate$region = "LA Area"
us_censusdata_race_ca_aggregate = rename(us_censusdata_race_ca_aggregate,c(".id" = "race", "X..i.." = "total"))
View(us_censusdata_race_ca_aggregate)

us_censusdata_race_ny_aggregate = ldply(us_censusdata_race_ny_sum,data.frame)

us_censusdata_race_ny_aggregate$region = "NY Area"
us_censusdata_race_ny_aggregate = rename(us_censusdata_race_ny_aggregate,c(".id" = "race", "X..i.." = "total"))
View(us_censusdata_race_ny_aggregate)

us_censusdata_race_ny_la_csa_aggregate = rbind(us_censusdata_race_ny_aggregate,us_censusdata_race_ca_aggregate)
View(us_censusdata_race_ny_la_csa_aggregate)


us_censusdata_transportation = read_csv("transportation.csv")

View(us_censusdata_transportation)

subtitletexttransportation =  paste("Transportation for work")


captionuscensus = paste("US Census Data based on 2018 \n American community Survey")


ggplot(us_censusdata_transportation, aes(fill=Region, y=as.character(Total), x=Category)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Total), vjust=2.0, color="white",
            position = position_dodge(0.9), size=3.5)+
  labs(fill = "Transportation to Work",subtitle = subtitletexttransportation, caption = captionuscensus, title= "LA CSA vs NY CSA", y="Population", x = "Region")+
  coord_flip()
