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

ggplot(top_ten_averages,aes(x=Date,y=Polling, color = Candidate))+
  geom_smooth()+
  geom_smooth(data=media_total_joined_filtered,aes(y=Coverage_share))+
  scale_y_log10(limits = c(0.5,100),breaks = c(0.5,1,2,5,10,20,30))+
  facet_wrap(~Candidate)
#media_total_joined
#

media_total_joined$Polling_after <- numeric(length = nrow(media_total_joined))
media_total_joined$Polling_before <- numeric(length = nrow(media_total_joined))
media_total_joined$Polling_at <- numeric(length = nrow(media_total_joined))
for(i in 1:nrow(media_total_joined))
{
  temp <- president_primary_polls %>% filter(state == "",
                                             candidate_name == media_total_joined$name[[i]])
  #2 week window, -7 / +6 days
  media_total_joined$Polling_before[[i]] <- averagePolls(specific_date = media_total_joined$date[[i]] - 4,media_total_joined$name[[i]],data_frame = temp,window = 6)
  media_total_joined$Polling_after[[i]] <- averagePolls(specific_date = media_total_joined$date[[i]] + 4,media_total_joined$name[[i]],data_frame = temp,window = 6)
  media_total_joined$Polling_at[[i]] <- averagePolls(specific_date = media_total_joined$date[[i]],media_total_joined$name[[i]],data_frame = temp,window = 6)
}
media_total_joined_filtered <- media_total_joined %>% filter(name %in% top_ten_candidates)
media_total_joined_filtered$Candidate <- factor(media_total_joined_filtered$name,levels = top_ten_candidates)
media_total_joined_filtered$Date <- media_total_joined_filtered$date
media_total_joined_filtered <- media_total_joined_filtered[complete.cases(media_total_joined_filtered),]

media_averaged <- media_total_joined_filtered %>% group_by(name,Month) %>% summarise(Coverage_share = mean(Coverage_share),
                                                                                    Polling_at = mean(Polling_at),
                                                                                    Polling_before = mean(Polling_before),
                                                                                    Polling_after = mean(Polling_after),)
media_total_joined_filtered <- ungroup(media_total_joined_filtered)
ggplot(media_total_joined_filtered, aes(y = Coverage_share,x = Polling_before, color = name,label = name, group = NA)) +
  #geom_point(alpha = 0.2)+
  #geom_smooth(method = "lm", se = FALSE)+
  geom_text(data = media_averaged)+
  geom_point(data = media_averaged)+
  facet_wrap(~Month)

big_picture <- media_total_joined %>% group_by(name) %>% summarise(combined = sum(combined),Polling = mean(Polling_at,na.rm = TRUE))
ggplot(big_picture,aes(x = Polling, y = combined, color = name, label = name))+
  geom_point()+
  geom_text_repel()+
  geom_smooth(method = "lm", se = FALSE)
