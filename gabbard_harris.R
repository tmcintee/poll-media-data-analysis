GH_media_total_joined <- media_total_joined %>%
  filter(name %in% c("Kamala Harris","Tulsi Gabbard"))
GH_polling_averages <- polling_averages[c("Date","Kamala Harris","Tulsi Gabbard")] %>%
  gather("Candidate","Polling",-Date,na.rm = TRUE)


ggplot(GH_polling_averages,aes(x = Date,y = Polling, color = Candidate))+
  geom_line(size = 1)+
  #geom_smooth()+
  scale_x_date()+
  expand_limits(y = 0)+
  #geom_vline(xintercept = parse_date("2019-08-24"))+
  #geom_rect(aes(xmin = parse_date("2019-08-24"),xmax = parse_date("2019-09-07"),ymin = 0, ymax = Inf, alpha = 0.5))+
  geom_text(x = parse_date("2019-07-31"), y = 8, angle = 90, label = "Second debate: Gabbard attacks Harris", color = "Black", alpha = 0.5)+
  geom_text(x = parse_date("2019-11-20"), y = 8, angle = 90, label = "Fifth debate: Harris attacks Gabbard", color = "Black", alpha = 0.5)+
  labs(color = "Candidate",
       x = "Date",
       y = "Horse race polling (two week rolling weighted average)",
       title = "Harris v. Gabbard (2020)",
       text = NA)
KH_polling <- president_primary_polls %>% filter(candidate_name == "Kamala Harris") %>% group_by(display_name) %>% summarise(mean_pct = mean(pct), count = n())

ggplot(GH_polling_averages %>% filter(Date > parse_date("2019-07-31")),aes(x = Date,y = Polling, color = Candidate))+
  geom_line(size = 1)+
  #geom_smooth()+
  scale_x_date()+
  #geom_vline(xintercept = parse_date("2019-08-24"))+
  #geom_rect(aes(xmin = parse_date("2019-08-24"),xmax = parse_date("2019-09-07"),ymin = 0, ymax = Inf, alpha = 0.5))+
  #geom_text(x = parse_date("2019-07-31"), y = 5, angle = 90, label = "Second debate: Gabbard attacks Harris", color = "Black", alpha = 0.5)+
  #geom_text(x = parse_date("2019-11-20"), y = 5, angle = 90, label = "Fifth debate: Harris attacks Gabbard", color = "Black", alpha = 0.5)+
  geom_text(x = parse_date("2019-10-18"), y = 2, angle = 90, label = "Clinton attacks Gabbard", color = "Black", alpha = 0.5)+
  labs(color = "Candidate",
       x = "Date",
       y = "Horse race polling (two week rolling weighted average)",
       title = "Harris v. Gabbard (2020)",
       text = NA)+
  expand_limits(y = 0)

ggplot(GH_polling_averages %>% filter(Date > parse_date("2019-11-19")),aes(x = Date,y = Polling, color = Candidate))+
  geom_line(size = 1)+
  #geom_smooth()+
  scale_x_date()+
  #geom_vline(xintercept = parse_date("2019-08-24"))+
  #geom_rect(aes(xmin = parse_date("2019-08-24"),xmax = parse_date("2019-09-07"),ymin = 0, ymax = Inf, alpha = 0.5))+
  #geom_text(x = parse_date("2019-07-31"), y = 5, angle = 90, label = "Second debate: Gabbard attacks Harris", color = "Black", alpha = 0.5)+
  geom_text(x = parse_date("2019-11-20"), y = 5, angle = 90, label = "Fifth debate: Harris attacks Gabbard", color = "Black", alpha = 0.5)+
  #geom_text(x = parse_date("2019-10-18"), y = 2, angle = 90, label = "Clinton attacks Gabbard", color = "Black", alpha = 0.5)+
  labs(color = "Candidate",
       x = "Date",
       y = "Horse race polling (two week rolling weighted average)",
       title = "Harris v. Gabbard (2020)",
       text = NA)+
  expand_limits(y = 0)
