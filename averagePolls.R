averagePolls <- function(specific_date,specific_candidate,data_frame,window = 0)
{
  require(tidyverse)
  require(Hmisc)
  window_start <- specific_date - ceiling(window/2)
  window_end <- specific_date + floor(window/2)
  working_frame <- data_frame %>% filter(end_date >= window_start,
                                         start_date <= window_end,
                                         candidate_name == specific_candidate)
  if(nrow(working_frame) == 0)
  {
    return(NA)
  }
  #Remove redundant rows
  working_frame$keep <- TRUE
  for(i in 1:nrow(working_frame))
  {
    for(j in 1:nrow(working_frame))
    {
      if(working_frame$pollster[[i]] == working_frame$pollster[[j]] & i != j)
      {
        working_frame$end_date[[i]] <= working_frame$end_date[[j]]
        working_frame$start_date[[i]] >= working_frame$start_date[[j]]
        working_frame$sample_size[[j]] < working_frame$sample_size[[j]]
      }
      if(working_frame$pollster[[j]] == working_frame$pollster[[i]] &
         working_frame$end_date[[i]] <= working_frame$end_date[[j]] &
         working_frame$start_date[[i]] >= working_frame$start_date[[j]] &
         working_frame$sample_size[[i]] < working_frame$sample_size[[j]])
      {
        working_frame$keep[[i]] <- FALSE
      }
    }
  }
  working_frame <- working_frame[working_frame$keep,]
  #print(working_frame[c("pollster","start_date","end_date","sample_size","pct")])
  working_frame <- working_frame %>% mutate(days = end_date - start_date + 1,
                                            days_in_window = max(window_start,start_date) - min(window_end,end_date)+1,
                                            sample_adj = as.numeric(days_in_window)/as.numeric(days)*sample_size)
  #Pool together samples from same pollster
  working_frame <- working_frame %>% group_by(pollster) %>% summarise(sample_size = sum(sample_adj),
                                                                      Rating = mean(Rating),
                                                                      pct = wtd.mean(pct,sample_adj))
  working_frame <- working_frame %>% mutate(Weight = Rating * sqrt(sample_size))
  average_value <- wtd.mean(working_frame$pct,working_frame$Weight)
  #print(working_frame[c("pollster","pct","Weight")])

  return(average_value)
}
