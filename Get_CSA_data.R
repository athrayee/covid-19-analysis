library(dplyr)
library(tidyr)
library(ggplot2)

# Read CSV files from NYTimes
us_counties = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", 
                        skip = 0)
View(us_counties)

date_2_filter = Sys.Date() - 2

View(date_2_filter)

# Only need data for NY Combined statistical area and LA combined statistical area

#list_of_counties_in_ca = c("Los Angeles", "Ventura","Orange","Riverside","San Bernardino")
#list_of_counties_in_ct = c("Fairfield","Litchfield","New Haven")
#list_of_counties_in_nj = c("Fairfield","Litchfield","New Haven","Los Angeles", "Ventura","Orange","Riverside","San Bernardino","Bergen","Hunterdon","Essex","Morris","Monmouth","Mercer","Passiac","Union","Sussex","Warren")
#list_of_states_needed = c("New York","New Jersey","Pennsylvania","Connecticut","California")

list_of_counties_needed = c("09001","09005","09009","34003","34013","34017","34019","34021","34023","34025","34027","34029","34031","34035","34037","34039","34041","42025","42077","42089","42095","42103","36027","36059","36071","36079","36087","36103","36111","36119","06037","06059","06065","06071","06111")

# Filter data for a date (today - 2)

ny_la_area_data = us_counties %>% filter(date == date_2_filter)
View(ny_la_area_data)
View(list_of_counties_needed)
# All 5 boroughs of NYC are grouped as New York City. So we need to filter that out first.
nycdata = ny_la_area_data %>% filter(county== "New York City")
View(nycdata)

# Filter out other counties that are not part of statistical area counties 
ny_la_area_data = ny_la_area_data %>% filter(fips %in% list_of_counties_needed)
View(ny_la_area_data)

# Combine NYC and other related counties
ny_la_area_data = rbind(nycdata,ny_la_area_data)
View(ny_la_area_data)

# Make 2 subsets so the data is shaped for plotting

death_subset = infection_subset = subset(ny_la_area_data)
View(death_subset)
View(infection_subset)

# Drop death column from infection subset and cases column from death subset. 
# Rename the column of cases and death as total

death_subset = select(death_subset,-matches("cases")) %>% rename("total" = "deaths")
View(death_subset)

infection_subset = select(infection_subset,-matches("deaths")) %>% rename("total" = "cases")
View(infection_subset)

# Add new column to identify infections and deaths so we can plot
death_subset$type = "Deaths"
View(death_subset)
infection_subset$type = "Infections"
View(infection_subset)

# Merge subsets back to the main table. Create a backup for main table
ny_la_area_data_backup = ny_la_area_data
ny_la_area_data = rbind(infection_subset,death_subset)
View(ny_la_area_data)

# create smaller table to re structure the data to calculate sum

la_csa_data = subset(ny_la_area_data, state == "California")
View(la_csa_data)
ny_csa_data = subset(ny_la_area_data, state != "California")
View(ny_csa_data)

# Create Aggregation of totals based on type and creating a unified data source

ny_csa_data_aggregate = ny_cas_data %>% group_by(type) %>% summarise(totalcases = sum(total))
View(ny_csa_data_aggregate)
ny_csa_data_aggregate$region = "NY Area"
la_csa_data_aggregate = la_csa_data %>% group_by(type) %>% summarise(totalcases = sum(total))
View(la_csa_data_aggregate)
la_csa_data_aggregate$region = "LA Area"
ny_la_csa_aggregate_data = rbind(ny_csa_data_aggregate,la_csa_data_aggregate)
View(ny_la_csa_aggregate_data)

# changing column to be discrete values by changing to character
ny_la_csa_aggregate_data$totalcases = as.character(ny_la_csa_aggregate_data$totalcases)

# Plot Graph for aggregate

subtitletext =  paste("Data updated as of ", format(date_2_filter, format="%B %d %Y"))

ggplot(ny_la_csa_aggregate_data, aes(fill=type, y=totalcases, x=region)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(fill = "Case Type",subtitle = subtitletext, caption = "NY Times Data Source", title= "LA CSA vs NY CSA", y="Total Cases (in thousands)", x = "Region")

