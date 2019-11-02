require(EloRating)
active_candidates <- c("Joe Biden",
                       "Bernie Sanders",
                       "Elizabeth Warren",
                       "Kamala Harris",
                       "Pete Buttigieg",
                       "Beto O'Rourke",
                       "Andrew Yang",
                       "Amy Klobuchar",
                       "Cory Booker",
                       "Michael Bennet",
                       "Steve Bullock",
                       "Tulsi Gabbard",
                       "John Delaney",
                       "Joe Sestak",
                       "Marianne Williamson",
                       "Tom Steyer",
                       "Julian Castro",
                       "Wayne Messam")

media_complete <- media_total_joined[complete.cases(media_total_joined),]
media_snub <- data.frame(Candidate_1 = character(),
                         Candidate_2 = character(),
                         Polling_before_1 = numeric(),
                         Polling_before_2 = numeric(),
                         Polling_at_1 = numeric(),
                         Polling_at_2 = numeric(),
                         Polling_after_1 = numeric(),
                         Polling_after_2 = numeric(),
                         Polling_diff = numeric(),
                         Coverage_1 = numeric(),
                         Coverage_2 = numeric(),
                         Coverage_diff = numeric(),
                         Date_of_comparison = numeric()),

pairs <- 0
media_sig_snub <- media_snub
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
      if((media_date$combined[[i]] < media_date$combined[[j]] &
         media_date$Polling_before[[i]] > media_date$Polling_before[[j]] &
         media_date$Polling_at[[i]] > media_date$Polling_at[[j]])|
         (media_date$combined[[i]] < media_date$combined[[j]] &
          media_date$Polling_before[[i]] > media_date$Polling_before[[j]] &
          media_date$Polling_at[[i]] > media_date$Polling_at[[j]]))
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
                              Polling_diff = media_date$Polling_at[[i]] - media_date$Polling_at[[j]],
                              Coverage_1 = media_date$combined[[i]],
                              Coverage_2 = media_date$combined[[j]],
                              Coverage_diff = media_date$combined[[j]] - media_date$combined[[i]],
                              Date_of_comparison = each_date)
        if((media_date$combined[[i]] + sqrt(media_date$combined[[i]]))/media_date$Polling_at[[i]] <
           media_date$combined[[j]]/media_date$Polling_at[[i]] &
           (media_date$combined[[i]] + sqrt(media_date$combined[[i]]))/media_date$Polling_before[[i]] <
           media_date$combined[[j]]/media_date$Polling_before[[i]])
        {
          media_sig_snub = add_case(media_sig_snub,
                                    Candidate_1 = media_date$name[[i]],
                                    Candidate_2 = media_date$name[[j]],
                                    Polling_before_1 = media_date$Polling_before[[i]],
                                    Polling_before_2 = media_date$Polling_before[[j]],
                                    Polling_at_1 = media_date$Polling_at[[i]],
                                    Polling_at_2 = media_date$Polling_at[[j]],
                                    Polling_after_1 = media_date$Polling_after[[i]],
                                    Polling_after_2 = media_date$Polling_after[[j]],
                                    Polling_diff = media_date$Polling_at[[i]] - media_date$Polling_at[[j]],
                                    Coverage_1 = media_date$combined[[i]],
                                    Coverage_2 = media_date$combined[[j]],
                                    Coverage_diff = media_date$combined[[j]] - media_date$combined[[i]],
                                    Date_of_comparison = each_date)
        }

      }


    }
  }
}
media_snub$Date_of_comparison <- as.Date.numeric(origin = "1970-01-01",media_snub$Date_of_comparison)
media_sig_snub$Date_of_comparison <- as.Date.numeric(origin = "1970-01-01",media_sig_snub$Date_of_comparison)

media_snub <- media_snub %>% mutate(Pair = paste0(Candidate_1, " ", Candidate_2),
                                    Poll_ratio = Polling_at_1/Polling_at_2,
                                    Coverage_ratio = Coverage_1/Coverage_2)

favorites_date <- table(media_snub[c("Candidate_2","Date_of_comparison")])
snubs_date <- table(media_snub[c("Candidate_1","Date_of_comparison")])
comparison_table <- table(media_snub[c("Candidate_1","Candidate_2")])

snubs_over_one <- media_snub %>% filter(Polling_at_1 >= 1 & Polling_before_1 >=1)
sig_snubs_over_one <- media_sig_snub %>% filter(Polling_at_1 >= 1 & Polling_before_1 >=1)
snubs_active <- media_snub %>% filter(Candidate_1 %in% active_candidates & Candidate_2 %in% active_candidates)
snubs_active_major <- media_sig_snub %>% filter(Candidate_1 %in% active_candidates &
                                                  Candidate_2 %in% active_candidates &
                                                  Polling_at_1 >= 1 &
                                                  Polling_before_1 >= 1)


elo_raw <- elo.seq(media_snub$Candidate_2,media_snub$Candidate_1,media_snub$Date_of_comparison,k = 32,startvalue = 1200)
elo_over <- elo.seq(snubs_over_one$Candidate_2, snubs_over_one$Candidate_1,snubs_over_one$Date_of_comparison, k=32,startvalue = 1200)
elo_sig <- elo.seq(media_sig_snub$Candidate_2,media_sig_snub$Candidate_1,media_sig_snub$Date_of_comparison, k=32, startvalue = 1200)
elo_sig_one <- elo.seq(sig_snubs_over_one$Candidate_2, sig_snubs_over_one$Candidate_1,sig_snubs_over_one$Date_of_comparison, k=32, startvalue = 1200)
elo_active <- elo.seq(snubs_active$Candidate_2,snubs_active$Candidate_1,snubs_active$Date_of_comparison,k=32, startvalue = 1200)
elo_active_major <- elo.seq(snubs_active_major$Candidate_2,snubs_active_major$Candidate_1,snubs_active_major$Date_of_comparison,k=32, startvalue = 1200)

raw_elo_ratings <- extract_elo(elo_raw)
one_elo_ratings <- extract_elo(elo_over)
sig_elo_ratings <- extract_elo(elo_sig)
one_sig_elo_ratings <- extract_elo(elo_sig)
active_elo <- extract_elo(elo_active)
major_elo <- extract_elo(elo_active_major)
