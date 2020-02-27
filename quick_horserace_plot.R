require(tidyverse)
current_candidates <- c(           "Joe Biden",
                        "Bernie Sanders",
                        "Elizabeth Warren",
                        "Pete Buttigieg",
                        "Amy Klobuchar",
                        "Tom Steyer",
                        "Tulsi Gabbard",
                        "Michael Bloomberg"
                        )
current_polling_averages <- polling_averages[c("Date",intersect(names(polling_averages),current_candidates))] %>%
  gather("Candidate","Polling",-Date,na.rm = TRUE)
temp <- current_polling_averages %>%
  group_by(Candidate) %>%
  summarise(mean_polling = mean(Polling)) %>%
  arrange(-mean_polling)

ppp_filtered <- president_primary_polls %>% filter(candidate_name %in% current_candidates)
ppp_filtered$Candidate <- factor(ppp_filtered$candidate_name,levels = temp$Candidate)

current_polling_averages$Candidate <- factor(current_polling_averages$Candidate, levels = temp$Candidate)
ggplot(current_polling_averages %>% filter(Polling > 1),aes(x = Date, y = Polling, color = Candidate))+
  scale_x_date()+
  scale_y_log10(limits = c(1,41), breaks = c(1,2,3,4,5,10,15,20,30,40))+
  geom_line(size = 1)+
  #geom_point(data = ppp_filtered, aes(x = as.Date(mean(c(end_date,start_date))), y = pct), alpha = 0.1)+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Horse-race polling averages (weighted, 2 week window, log scale)")

