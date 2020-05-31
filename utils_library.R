library(dplyr)
library(tidyr)
library(hash)
library(data.table)

remove_margin_and_percent_columns_us_censusdata = function(table_2_remove)
{
  return(table_2_remove %>% select(-(contains("Margin") | contains("Percent"))))
}

add_county_state_and_reorder = function(table_2_add,state_list,county_list)
{
  table_2_add$state = state_list
  table_2_add$county = county_list
  return(table_2_add %>% select(state,county,everything()))
}

get_desiered_column_names = function(column_name_collection,string_split_pattern)
{
  desired_col_name = c()
  
  for (col_nam in column_name_collection) {
    
    col_nam_list = str_split(col_nam,string_split_pattern)
    col_nam_extract = unlist(col_nam_list)
    col_nam_extract_len = length(col_nam_extract)
    desired_col_name = c(desired_col_name,col_nam_extract[col_nam_extract_len])
  }
  
  return(desired_col_name)
}

get_rename_column_list = function(current_column_name,desired_column_name)
{
  new_column_list = hash()
  .set(new_column_list,keys=desired_column_name,values=current_column_name)

  return(new_column_list)
  
}

rename_columns = function(table_to_rename,current_column_names,desired_column_name)
{
  
  return(setnames(table_to_rename,current_column_names,desired_column_name))
 
}

convert_2_character = function(column_to_convert)
{
    return(as.character(column_to_convert))
}


  