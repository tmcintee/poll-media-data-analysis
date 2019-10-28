#average polling
window_size <- 13
all_candidates <- unique(candidate_coverage$candidate_name)
all_dates <- unique(c(president_primary_polls$start_date,president_primary_polls$end_date))
polling_averages <- data.frame(matrix(nrow = length(all_dates),ncol = length(all_candidates)+1))
colnames(polling_averages) <- c("Date",all_candidates)
polling_averages$Date <- as.Date(polling_averages$Date)
for(j in 1:length(all_dates))
{
  polling_averages$Date[[j]] <- all_dates[[j]]
}
for(each_candidate in all_candidates)
{
  temp <- president_primary_polls %>% filter(state == "",
                                             candidate_name == each_candidate)
  for(j in 1:length(all_dates))
  {
    polling_averages[j,each_candidate] <- averagePolls(all_dates[[j]],each_candidate,temp,window = window_size)
  }
}
gathered_averages <- gather(polling_averages,key = "Candidate", value = "Polling",-c("Date")) %>% filter(!is.na(Polling))
temp <- gathered_averages %>% group_by(Candidate) %>% summarise(mean_polling = mean(Polling))
top_ten_candidates <- temp$Candidate[order(temp$mean_polling,decreasing = TRUE)][1:11]
top_ten_averages <- gathered_averages %>% filter(Candidate %in% top_ten_candidates)
top_ten_averages$Candidate <- factor(top_ten_averages$Candidate, levels = top_ten_candidates)
media_total_filtered <- media_total %>% filter(name %in% top_ten_candidates)
media_total_filtered$Candidate <- factor(media_total_filtered$name,levels = top_ten_candidates)
media_total_filtered$Date <- media_total_filtered$date

ggplot(top_ten_averages,aes(x=Date,y=Polling, color = Candidate))+
  geom_smooth()+
  geom_smooth(data=media_total_filtered,aes(y=Coverage_share))+
  scale_y_log10(limits = c(0.5,100),breaks = c(0.5,1,2,5,10,20,30))+
  facet_wrap(~Candidate)
#media_total
#

media_total$Polling_after <- numeric(length = nrow(media_total))
media_total$Polling_before <- numeric(length = nrow(media_total))
media_total$Polling_at <- numeric(length = nrow(media_total))
for(i in 1:nrow(media_total))
{
  temp <- president_primary_polls %>% filter(state == "",
                                             candidate_name == media_total$name[[i]])
  #2 week window, -7 / +6 days
  media_total$Polling_before[[i]] <- averagePolls(specific_date = media_total$date[[i]] - 4,media_total$name[[i]],data_frame = temp,window = 6)
  media_total$Polling_after[[i]] <- averagePolls(specific_date = media_total$date[[i]] + 4,media_total$name[[i]],data_frame = temp,window = 6)
  media_total$Polling_at[[i]] <- averagePolls(specific_date = media_total$date[[i]],media_total$name[[i]],data_frame = temp,window = 6)
}
