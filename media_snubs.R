media_complete <- media_total_joined[complete.cases(media_total_joined),]
media_snub <- data.frame(Candidate_1 = character(),
                         Candidate_2 = character(),
                         Polling_before_1 = numeric(),
                         Polling_before_2 = numeric(),
                         Polling_at_1 = numeric(),
                         Polling_at_2 = numeric(),
                         Polling_after_1 = numeric(),
                         Polling_after_2 = numeric(),
                         Coverage_1 = numeric(),
                         Coverage_2 = numeric(),
                         Date_of_comparison = numeric())
pairs <- 0
for(each_date in unique(media_complete$date))
{
  media_date <- media_complete %>% filter(date == each_date)
  for(i in 1:nrow(media_date))
  {
    for(j in 1:nrow(media_date))
    {
      if(i != j)
      {
        pairs <- pairs +1
      }
      if((media_date$combined[[i]] <= media_date$combined[[j]] &
         media_date$Polling_before[[i]] > media_date$Polling_before[[j]] &
         media_date$Polling_at[[i]] > media_date$Polling_at[[j]])|
         (media_date$combined[[i]] < media_date$combined[[j]] &
          media_date$Polling_before[[i]] >= media_date$Polling_before[[j]] &
          media_date$Polling_at[[i]] >= media_date$Polling_at[[j]]))
      {
        media_snub = add_case(media_snub,
                              Candidate_1 = media_date$name[[i]],
                              Candidate_2 = media_date$name[[j]],
                              Polling_before_1 = media_date$Polling_before[[i]],
                              Polling_before_2 = media_date$Polling_before[[j]],
                              Polling_at_1 = media_date$Polling_at[[i]],
                              Polling_at_2 = media_date$Polling_at[[j]],
                              Polling_after_1 = media_date$Polling_after[[i]],
                              Polling_after_2 = media_date$Polling_after[[j]],
                              Coverage_1 = media_date$combined[[i]],
                              Coverage_2 = media_date$combined[[j]],
                              Date_of_comparison = each_date)
      }

    }
  }
}
media_snub$Date_of_comparison <- as.Date.numeric(origin = "1970-01-01",media_snub$Date_of_comparison)
media_snub <- media_snub %>% mutate(Pair = paste0(Candidate_1, " ", Candidate_2),
                                    Poll_ratio = Polling_at_1/Polling_at_2,
                                    Coverage_ratio = Coverage_1/Coverage_2)

favorites_date <- table(media_snub[c("Candidate_2","Date_of_comparison")])
snubs_date <- table(media_snub[c("Candidate_1","Date_of_comparison")])
comparison_table <- table(media_snub[c("Candidate_1","Candidate_2")])

