#average polling
source("averagePolls.R")
require(RColorBrewer)
require(tidyverse)
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
top_six_candidates <- temp$Candidate[order(temp$mean_polling,decreasing = TRUE)][c(1:5,8)]
top_six_averages <- gathered_averages %>% filter(Candidate %in% top_six_candidates)
top_six_averages$Candidate <- factor(top_six_averages$Candidate, levels = top_six_candidates)

color_set <- colorRampPalette(brewer.pal(9,"Set1"))
colors_instance <- color_set(12)
colors_instance[[8]] <- "seagreen"

next_six_candidates <- c(temp$Candidate[order(temp$mean_polling,decreasing = TRUE)][7:11],"Tom Steyer")
next_six_averages <- gathered_averages %>% filter(Candidate %in% next_six_candidates)
next_six_averages$Candidate <- factor(next_six_averages$Candidate, levels = next_six_candidates)

#media_total_joined

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
media_total_joined$Coverage_PPP <- media_total_joined$combined / media_total_joined$Polling_at


media_total_joined_filtered <- media_total_joined %>% filter(name %in% top_ten_candidates)
media_total_joined_filtered$Candidate <- factor(media_total_joined_filtered$name,levels = top_ten_candidates)
media_total_joined_filtered$Date <- media_total_joined_filtered$date
media_total_joined_filtered <- media_total_joined_filtered[complete.cases(media_total_joined_filtered),]


media_averaged <- media_total_joined_filtered %>% group_by(name,Month) %>% summarise(Coverage_share = mean(Coverage_share),
                                                                                    Polling_at = mean(Polling_at),
                                                                                    Polling_before = mean(Polling_before),
                                                                                    Polling_after = mean(Polling_after),)
media_total_joined_filtered <- ungroup(media_total_joined_filtered)

big_picture <- media_total_joined %>% group_by(name) %>% summarise(combined = sum(combined),Polling = mean(Polling_at,na.rm = TRUE))
recent_dates <- c(max(top_six_averages$Date),max(top_six_averages$Date)-30)
fifth_campaign_qualifier_cutoffs <- parse_date(c("2019-09-20","2019-11-07"))
