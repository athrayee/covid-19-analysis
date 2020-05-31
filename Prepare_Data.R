library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(ggrepel)


# Custom Functions

source("utils_library.R")

#rm(list=ls())
# Load data from US Census

# Setting working directory

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

us_censusdata_population_demograpics = read_csv("/ACSDP1Y2018.DP05_data_with_overlays_2020-04-29T175719.csv", 
                         skip = 1)


us_censusdata_transportation = read_csv("ACSST1Y2018.S0801_data_with_overlays_2020-05-20T120857.csv", 
                                                skip = 1)
View(us_censusdata_population_demograpics)
us_censusdata_population_demograpics = na.omit(us_censusdata_population_demograpics)

us_censusdata_transportation = na.omit(us_censusdata_transportation)

View(us_censusdata_transportation)


us_censusdata_name = select(us_censusdata_population_demograpics,2)
View(us_censusdata_name)

us_censusdata_name = us_censusdata_name %>% separate('Geographic Area Name',c("county","state"),",")

# Remove unwanted columns

us_censusdata_population_demograpics = remove_margin_and_percent_columns_us_censusdata(us_censusdata_population_demograpics)
View(us_censusdata_population_demograpics)

us_censusdata_transportation = remove_margin_and_percent_columns_us_censusdata(us_censusdata_transportation)
View(us_censusdata_transportation)


us_censusdata_population_demograpics = us_censusdata_population_demograpics %>% select(-contains("Estimate!!SEX AND AGE!!Total population!!Sex ratio (males per 100 females)"))
View(us_censusdata_population_demograpics)



us_censusdata_population_demograpics = us_censusdata_population_demograpics %>% select(-contains("Sex ratio"))
View(us_censusdata_population_demograpics)

# split into population and Race

us_censusdata_race = us_censusdata_population_demograpics %>% select(contains("RACE"))
View(us_censusdata_race)

us_censusdata_total_population = us_censusdata_population_demograpics %>% select(-(contains("RACE") | contains("VOTING AGE")))
View(us_censusdata_total_Population)

us_censusdata_total_population = us_censusdata_total_population %>% select(-(contains("Id") | contains("Geographic")))
View(us_censusdata_total_population)

us_censusdata_total_population_Gender_Age = us_censusdata_total_population %>% select(-(contains("Under") | contains("Over")))
View(us_censusdata_total_population_Gender_Age)

us_censusdata_race = us_censusdata_race %>% select(-contains("_"))
View(us_censusdata_race)

us_censusdata_race = us_censusdata_race %>% select(contains("One race"))
View(us_censusdata_race)


# Get all current column names

current_column_names_total_population = colnames(us_censusdata_total_population_Gender_Age)

current_column_names_race = colnames(us_censusdata_race)

current_column_names_transportation = colnames(us_censusdata_transportation)

# Get Desired column names

desired_column_name_total_population = get_desiered_column_names(current_column_names_total_population,"(!!)")

desired_column_name_race = get_desiered_column_names(current_column_names_race,"(!!)")

desired_column_name_transportation = get_desiered_column_names(current_column_names_transportation,"(!!)")

View(desired_column_name_total_population)
View(desired_column_name_race)
View(desired_column_name_transportation)


# Transportation only needs 3 columns

us_censusdata_transportation = us_censusdata_transportation[3:5]


us_censusdata_transportation = us_censusdata_transportation %>% rename("Total" = 'Estimate!!Total!!Workers 16 years and over', "Male Workers" = 'Estimate!!Male!!Workers 16 years and over',"Female Workers" = 'Estimate!!Female!!Workers 16 years and over')
View(us_censusdata_transportation)


# Rename column names

us_censusdata_total_population_Gender_Age = rename_columns(us_censusdata_total_population_Gender_Age,current_column_names_total_population,desired_column_name_total_population)
View(us_censusdata_total_population_Gender_Age)

us_censusdata_race = rename_columns(us_censusdata_race,current_column_names_race,desired_column_name_race)
View(us_censusdata_race)


# Dropping the first column

us_censusdata_race = us_censusdata_race %>% select(-contains("One race"))
View(us_censusdata_race)

# Adding Geographic Names

us_censusdata_race = add_county_state_and_reorder(us_censusdata_race,us_censusdata_name$state,us_censusdata_name$county)
View(us_censusdata_race)

us_censusdata_total_population_Gender_Age = add_county_state_and_reorder(us_censusdata_total_population_Gender_Age,us_censusdata_name$state,us_censusdata_name$county)
View(us_censusdata_total_population_Gender_Age_new)

us_censusdata_transportation = add_county_state_and_reorder(us_censusdata_transportation,us_censusdata_name$state,us_censusdata_name$county)
View(us_censusdata_transportation)


# Write files to disk

write_csv2(us_censusdata_total_population_Gender_Age,"us_total_population_gender_age.csv")

write_csv2(us_censusdata_race, "us_census_population_race.csv")

write_csv2(us_censusdata_transportation,"us_censusdata_transportation.csv")


