pacman::p_load(
  httr,     #pulling api data
  magrittr, #piping functions
  dplyr,
  tidyr, 
  purrr)   #connecting to remote data


#Code----
# Define the function to extract a pattern
process_decennial_data <- function(year){
  extract_pattern <- function(x, n) {
  # x = pattern
  first_part <- substr(x, 1, 7)
  second_part <- as.integer(substr(x, 8, 10))
  next_pattern <- paste0(first_part, sprintf("%03d", second_part + n))
  return(next_pattern)
  }
  
  # Define the function to generate patterns
  generate_patterns <- function(initial_pattern, total_patterns, num_parts) {
    # Calculate the number of patterns per part
    patterns_per_part <- total_patterns %/% num_parts
    
    # Initialize a list to store the patterns
    patterns <- vector("list", num_parts)
    
    # Initialize a counter
    counter <- 0
    
    # Generate the patterns and split them into parts
    for (i in 1:num_parts) {
      patterns[[i]] <- character(patterns_per_part)
      for (j in 1:patterns_per_part) {
        pattern <- extract_pattern(initial_pattern, counter)
        patterns[[i]][j] <- pattern
        counter <- counter + 1
      }
    }
    
    # If the total number of patterns is not divisible by the number of parts,
    # add the remaining patterns to the last part
    if (total_patterns %% num_parts != 0) {
      remaining <- sapply(counter:(total_patterns - 1), function(i) extract_pattern(initial_pattern, i))
      patterns[[num_parts]] <- c(patterns[[num_parts]], remaining)
    }
    
    # Convert each part into a string with commas between the patterns
    patterns <- sapply(patterns, function(x) paste(x, collapse = ","))
    
    return(patterns)
  }
  
  # Define the initial patterns for males
  white_male  = generate_patterns('PCT012I001', 105, 3)
  boaa_male   = generate_patterns('PCT012J001', 105, 3)
  aian_male   = generate_patterns('PCT012K001', 105, 3)
  asian_male  = generate_patterns('PCT012L001', 105, 3)
  nhpi_male   = generate_patterns('PCT012M001', 105, 3)
  others_male = generate_patterns('PCT012N001', 105, 3)
  two_more_male = generate_patterns('PCT012O001', 105, 3)
  # 
  # # Define the initial patterns for females
  white_female  = generate_patterns('PCT012I106', 104, 3)
  boaa_female   = generate_patterns('PCT012J106', 104, 3)
  aian_female   = generate_patterns('PCT012K106', 104, 3)
  asian_female  = generate_patterns('PCT012L106', 104, 3)
  nhpi_female   = generate_patterns('PCT012M106', 104, 3)
  others_female = generate_patterns('PCT012N106', 104, 3)
  two_more_female = generate_patterns('PCT012O106', 104, 3)
  
  
  #Building API for males
  
  convert_process_df <- function(endpoint, query, list_name, filter_sub, year = NULL){
    df_list <- list()
    for (pattern in list_name){
      response <- GET(paste0(endpoint, query, pattern, filter_sub)) |> 
        content('text') |> 
        jsonlite::fromJSON() %>% as.data.frame()
      
      #or using httr2
      # response <- httr2::request(paste0(endpoint, query, pattern, filter_sub)) |>
      #   httr2::req_perform() |>
      #   httr2::resp_body_string() |>
      #   jsonlite::fromJSON() |> as.data.frame()
      
      df_list <- c(df_list, list(response))
    }
    
    return(df_list)
  }
  
  endpoint = paste0('https://api.census.gov/data/', sprintf('%04d', year = year), '/dec/sf1')
  query = '?get='
  filter_sub = '&for=county%20subdivision:*&in=state:34&in=county:*'
  
  
  
  lists_male   = list(white_male, boaa_male, aian_male, asian_male, nhpi_male, others_male, two_more_male)
  lists_female = list(white_female, boaa_female, aian_female, asian_female, nhpi_female, others_female, two_more_female)
  
  
  years = c(year)
  
  # df_male <- data.frame()
  # df_female <- data.frame()
  
  for (year in years) {
    dfs_male <- lapply(lists_male, function(list_name) 
      convert_process_df(endpoint, query, list_name, filter_sub, year = year))
    
    dfs_female <- lapply(lists_female, function(list_name) 
      convert_process_df(endpoint, query, list_name, filter_sub, year = year))
  }
  
  # Define your categories
  categories <- c('white', 'boaa', 'aian', 'asian', 'nhpi', 'others', 'two_more')
  
  # Extract data frames from dfs_male and dfs_female
  dfs_male_categories <- list()
  dfs_female_categories <- list()
  
  for (i in seq_along(categories)) {
    dfs_male_categories[[categories[i]]] <- dfs_male[[i]]
    dfs_female_categories[[categories[i]]] <- dfs_female[[i]]
  }
  
  
  # Function to process a list of dataframes
  rename_col <- function(df_list) {
    for (category in names(df_list)) {
      for (i in seq_along(df_list[[category]])) {
        df <- df_list[[category]][[i]]
        
        # Extract the first row as column names
        col_names <- df[1, ]
        
        # Set the extracted row values as column names
        colnames(df) <- col_names
        
        # Remove the first row
        df <- df[-1, ]
        
        # Update the list with modified data frame
        df_list[[category]][[i]] <- df
      }
    }
    return(df_list)
  }
  
  #Applying rename cols to categories
  dfs_male_categories <- rename_col(dfs_male_categories)
  dfs_female_categories <- rename_col(dfs_female_categories)
  
  
  merge_keys = c('state', 'county', 'county subdivision')
  
  # Function to merge DataFrames within a category
  merge_category_dfs <- function(category_dfs) {
    merged_df <- category_dfs[[1]]
    
    if (length(category_dfs) > 1) {
      for (df in category_dfs[2:length(category_dfs)]) {
        merged_df <- merge(merged_df, df, by = merge_keys, all = TRUE)
      }
    }
    
    # Reorder columns
    cols <- c(setdiff(names(merged_df), merge_keys), merge_keys)
    merged_df <- merged_df[, cols]
    
    return(merged_df)
  }
  
  # Merge all DataFrames within each male category
  dfs_male_categories <- lapply(dfs_male_categories, merge_category_dfs)
  
  # Merge all DataFrames within each female category
  dfs_female_categories <- lapply(dfs_female_categories, merge_category_dfs)
  
  
  # Create a list of new column names
  new_names_male <- c('Total 2010', 'Total Male','Under 1 year', paste0(as.character(seq(1, 99)), ' years'), '100-104 years', '105-109 years','110 and over years', 'State', 'County', 'County Subdivision')
  
  new_names_female <- c('Total Female','Under 1 year', paste0(as.character(seq(1, 99)), ' years'), '100-104 years', '105-109 years','110 and over years', 'State', 'County', 'County Subdivision')
  
  
  # Function to rename columns
  rename_columns <- function(df, new_names) {
    if (ncol(df) == length(new_names)) {
      colnames(df) <- new_names
    } else {
      warning("Number of columns in dataframe does not match number of new names")
    }
    return(df)
  }
  
  # Rename columns in male categories
  dfs_male_categories <- lapply(dfs_male_categories, rename_columns, new_names = new_names_male)
  
  # Rename columns in female categories
  dfs_female_categories <- lapply(dfs_female_categories, rename_columns, new_names = new_names_female)
  
  
  #now lets transpose the columns in each category
  pivot_df <- function(df, category, sex) {
    df %>%
      pivot_longer(
        cols = -c(State, County, `County Subdivision`),
        names_to = "Variables",
        values_to = "Values"
      ) %>%
      mutate(
        Source = category,
        Sex = sex
      )
  }
  
  # Process male categories
  male_pivoted <- map(
    names(dfs_male_categories),
    ~pivot_df(dfs_male_categories[[.x]], .x, "Male")
  ) |>  bind_rows()
  
  # Process female categories
  female_pivoted <- map(
    names(dfs_female_categories),
    ~pivot_df(dfs_female_categories[[.x]], .x, "Female")
  ) |> bind_rows()
  
  
  # Combine male and female data
  all_pivoted <- bind_rows(male_pivoted, female_pivoted)
  
  #remove 00000 county subdision
  all_pivoted_new  <-  all_pivoted |> subset(`County Subdivision` != '00000')
  }
