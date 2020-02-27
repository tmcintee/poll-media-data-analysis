current_candidates <- c(           "Joe Biden",
                                   "Bernie Sanders",
                                   "Elizabeth Warren",
                                   "Pete Buttigieg",
                                   "Amy Klobuchar",
                                   "Tom Steyer",
                                   "Tulsi Gabbard",
                                   "Michael Bloomberg"
)
SC_primary_polls <- president_primary_polls %>% filter(state == "South Carolina",candidate_name %in% current_candidates)
window_size <- 13
SC_dates <- unique(c(SC_primary_polls$start_date,SC_primary_polls$end_date))
polling_averages_SC <- data.frame(matrix(nrow = length(SC_dates),ncol = length(current_candidates)+1))
colnames(polling_averages_SC) <- c("Date",current_candidates)
polling_averages_SC$Date <- as.Date(polling_averages_SC$Date)
for(j in 1:length(SC_dates))
{
  polling_averages_SC$Date[[j]] <- SC_dates[[j]]
}

for(each_candidate in current_candidates)
{
  temp <- SC_primary_polls %>% filter(candidate_name == each_candidate)
  for(j in 1:length(SC_dates))
  {
    polling_averages_SC[j,each_candidate] <- averagePolls(SC_dates[[j]],each_candidate,temp,window = window_size)
  }
}
SC_primary_polls$Candidate = SC_primary_polls$candidate_name
SC_primary_polls$Polling = SC_primary_polls$pct
SC_primary_polls$Date = SC_primary_polls$start_date

gathered_averages_SC <- gather(polling_averages_SC,key = "Candidate", value = "Polling",-c("Date")) %>%
  filter(!is.na(Polling) & !is.nan(Polling))

ggplot(gathered_averages_SC,aes(x = Date, y = Polling, color = Candidate))+
  scale_x_date()+
  scale_y_log10(limits = c(1,41), breaks = c(1,2,3,4,5,10,15,20,30,40))+
  geom_smooth(se = FALSE)+
  #geom_point(data = SC_primary_polls,aes(alpha = Weight))+
  #geom_point(data = ppp_filtered, aes(x = as.Date(mean(c(end_date,start_date))), y = pct), alpha = 0.1)+
  scale_color_brewer(palette = "Set1")+
  labs(title = "South Carolina polling averages (weighted, 2 week window, log scale)")

