library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(ggrepel)
library(plyr)


# Custom Functions

source("utils_library.R")

#rm(list=ls())
# Load data from US Census

us_censusdata = read_csv("~/Downloads/ACSDP1Y2018.DP05_data_with_overlays_2020-04-29T175719.csv", 
                         skip = 1)
View(us_censusdata)
us_censusdata = na.omit(us_censusdata)

us_censusdata_name = select(us_censusdata,2)
View(us_censusdata_name)

us_censusdata_name = us_censusdata_name %>% separate('Geographic Area Name',c("couty","state"),",")

# Remove unwanted columns

us_censusdata = us_censusdata %>% select(-contains("Margin"))
View(us_censusdata)
us_censusdata = us_censusdata %>% select(-contains("Percent"))
View(us_censusdata)

us_censusdata = us_censusdata %>% select(-contains("Estimate!!SEX AND AGE!!Total population!!Sex ratio (males per 100 females)"))
View(us_censusdata)



us_censusdata = us_censusdata %>% select(-contains("Sex ratio"))
View(us_censusdata)

# split into population and Race

us_censusdata_race = us_censusdata %>% select(contains("RACE"))
View(us_censusdata_race)

us_censusdata_race = us_censusdata_race %>% select(-contains("_"))
View(us_censusdata_race)

us_censusdata_race = us_censusdata_race %>% select(contains("One race"))
View(us_censusdata_race)


# Get all current column names

current_column_names_race = colnames(us_censusdata_race)

# Get Desired column names

desired_column_name_race = get_desiered_column_names(current_column_names_race,"(!!)")

View(desired_column_name_race)

# Rename column names

us_censusdata_race = rename_columns(us_censusdata_race,current_column_names_race,desired_column_name_race)
View(us_censusdata_race)

# Dropping the first column

us_censusdata_race = us_censusdata_race %>% select(-contains("One race"))
View(us_censusdata_race)

# Adding Geographic Names

us_censusdata_race$state = us_censusdata_name$state
us_censusdata_race$county = us_censusdata_name$couty
us_censusdata_race = us_censusdata_race %>% select(state,county,everything())
View(us_censusdata_race)

# Write files to disk

write_csv2(us_censusdata_race, "us_census_population_race.csv")

# create california and ny group

us_censusdata_race$state = trimws(us_censusdata_race$state)
us_censusdata_race_ca = subset(us_censusdata_race,us_censusdata_race$state == "California")

us_censusdata_race_ny = subset(us_censusdata_race,us_censusdata_race$state != "California")


us_censusdata_race_ca = us_censusdata_race_ca[-c(1,2)]
View(us_censusdata_race_ca)


us_censusdata_race_ny = us_censusdata_race_ny[-c(1,2)]
View(us_censusdata_race_ny)

# Some Columns are of character type. So need to make all columns number to perform aggregation

us_censusdata_race_ca_numeric = data.matrix(us_censusdata_race_ca,rownames.force = NA)
us_censusdata_race_ny_numeric = data.matrix(us_censusdata_race_ny,rownames.force = NA)

# Sum all columns independently

us_censusdata_race_ca_sum = colSums(us_censusdata_race_ca_numeric)
us_censusdata_race_ny_sum = colSums(us_censusdata_race_ny_numeric)

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

# Remove empty rows

us_censusdata_race_ny_la_csa_aggregate = na.omit(us_censusdata_race_ny_la_csa_aggregate,cols="total")
View(us_censusdata_race_ny_la_csa_aggregate)

# Eliminating Other race and ethinicity from LA

us_censusdata_race_ny_la_csa_aggregate = rbind(head(us_censusdata_race_ny_la_csa_aggregate,10),rbind(head(tail(us_censusdata_race_ny_la_csa_aggregate,6),1),tail(us_censusdata_race_ny_la_csa_aggregate,1)))
View(us_censusdata_race_ny_la_csa_aggregate)


# Plot Graph for aggregate

subtitletext =  paste("Race and Ethinicity")
captionuscensus = paste("US Census Data based on 2018 \n American community Survey")

ggplot(us_censusdata_race_ny_la_csa_aggregate, aes(fill=race, y=total, x=region)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(fill = "Total Population (By Race)",subtitle = subtitletext, caption = captionuscensus, title= "LA CSA vs NY CSA", y="Total Population", x = "Region")


