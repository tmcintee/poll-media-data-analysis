add_ratings_weights <- function(poll_table,grade_offset = 0, default_rating = 2.5)
{
  poll_table <- poll_table %>%
    mutate(Rating = default_rating)
  for(i in 1:nrow(poll_table))
  {
    if(is.na(poll_table$Rating[[i]]))
    {
      poll_table$Rating[[i]] <- default_rating
    }
    else if(poll_table$Rating[[i]] == "A+")
    {
      poll_table$Rating[[i]] <- 4.3
    }
    else if(poll_table$Rating[[i]] == "A")
    {
      poll_table$Rating[[i]] <- 4
    }
    else if(poll_table$Rating[[i]] == "A-")
    {
      poll_table$Rating[[i]] <- 3.7
    }
    else if(poll_table$Rating[[i]] == "B+")
    {
      poll_table$Rating[[i]] <- 3.3
    }
    else if(poll_table$Rating[[i]] == "B")
    {
      poll_table$Rating[[i]] <- 3
    }
    else if(poll_table$Rating[[i]] == "B-")
    {
      poll_table$Rating[[i]] <- 2.7
    }
    else if(poll_table$Rating[[i]] == "C+")
    {
      poll_table$Rating[[i]] <- 2.3
    }
    else if(poll_table$Rating[[i]] == "C")
    {
      poll_table$Rating[[i]] <- 2
    }
    else if(poll_table$Rating[[i]] == "C-")
    {
      poll_table$Rating[[i]] <- 1.7
    }
    else if(poll_table$Rating[[i]] == "D+")
    {
      poll_table$Rating[[i]] <- 1.3
    }
    else if(poll_table$Rating[[i]] == "D")
    {
      poll_table$Rating[[i]] <- 1
    }
    else if(poll_table$Rating[[i]] == "D-")
    {
      poll_table$Rating[[i]] <- 0.7
    }
    else if(poll_table$Rating[[i]] == "F")
    {
      poll_table$Rating[[i]] <- 0
    }
  }
  poll_table <- poll_table %>% mutate(Weight = sqrt(sample_size)*max(0,Rating-grade_offset))
  return(poll_table)
}
clean_candidate_name <- function(vector)
{
  vector <- as.character(vector)
  list_names <- str_split(vector," ")
  for(i in 1:length(list_names))
  {
    first_name <- list_names[[i]][[1]]
    last_name <- list_names[[i]][[length(list_names[[i]])]]
    if(first_name == "JuliÃ¡n")
    {
      first_name <- "Julian"
    }
    else if(first_name == "Joseph")
    {
      first_name <- "Joe"
    }
    if(last_name == "Jr." | last_name == "III")
    {
      last_name <- list_names[[i]][[length(list_names[[i]])-1]]
    }
    name_candidate_cleaned <- paste0(first_name," ",last_name)
    vector[[i]] <- paste0(first_name," ",last_name)
  }

  return(vector)
}
month_column_add <- function(date_vector)
{
  month_vector <- vector(mode = "character",length = length(date_vector))
  month_vector[str_starts(date_vector,"1/")] <- "January"
  month_vector[str_starts(date_vector,"2/")] <- "February"
  month_vector[str_starts(date_vector,"3/")] <- "March"
  month_vector[str_starts(date_vector,"4/")] <- "April"
  month_vector[str_starts(date_vector,"5/")] <- "May"
  month_vector[str_starts(date_vector,"6/")] <- "June"
  month_vector[str_starts(date_vector,"7/")] <- "July"
  month_vector[str_starts(date_vector,"8/")] <- "August"
  month_vector[str_starts(date_vector,"9/")] <- "September"
  month_vector[str_starts(date_vector,"10/")] <- "October"
  month_vector[str_starts(date_vector,"11/")] <- "November"
  month_vector[str_starts(date_vector,"12/")] <- "December"
  month_vector[str_starts(date_vector,"2019-01")] <- "January"
  month_vector[str_starts(date_vector,"2019-02")] <- "February"
  month_vector[str_starts(date_vector,"2019-03")] <- "March"
  month_vector[str_starts(date_vector,"2019-04")] <- "April"
  month_vector[str_starts(date_vector,"2019-05")] <- "May"
  month_vector[str_starts(date_vector,"2019-06")] <- "June"
  month_vector[str_starts(date_vector,"2019-07")] <- "July"
  month_vector[str_starts(date_vector,"2019-08")] <- "August"
  month_vector[str_starts(date_vector,"2019-09")] <- "September"
  month_vector[str_starts(date_vector,"2019-10")] <- "October"
  month_vector[str_starts(date_vector,"2019-11")] <- "November"
  month_vector[str_starts(date_vector,"2018-12")] <- "December"
  return(month_vector)
}
