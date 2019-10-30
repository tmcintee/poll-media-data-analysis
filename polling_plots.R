plot_top_six <- ggplot(top_six_averages,aes(x=Date,y=Polling, color = Candidate))+
  scale_color_manual(values = colors_instance[1:6])
plot_next_six <- ggplot(next_six_averages,aes(x=Date,y=Polling, color = Candidate))+
  scale_color_manual(values = colors_instance[7:12])
plot_all <- ggplot(rbind(top_six_averages,next_six_averages), aes(x = Date, y = Polling, color = Candidate))+
  scale_color_manual(values = colors_instance)
plot_low <- ggplot(rbind(top_six_averages %>% filter(Candidate == "Beto O'Rourke"),next_six_averages) %>%
                               filter(), aes(x = Date, y = Polling, color = Candidate))+
  scale_color_manual(values = colors_instance[6:12])

plot_top_six +
  geom_line(size=1)+
  labs(title = "Aggregate polling by date, 2 week rolling window")
plot_top_six +
  geom_line(size=1)+
  #geom_smooth(model = "lm", formula = y ~ x)+
  scale_x_date(limits = c(max(top_six_averages$Date)-30,max(top_six_averages$Date)))+
  labs(title = "Recent polling")
plot_next_six +
  geom_line(size=1)+
  labs(title = "Aggregate polling by date, 2 week rolling window")

plot_next_six +
  geom_line(size=1)+
  scale_x_date(limits = c(max(top_six_averages$Date)-30,max(top_six_averages$Date)))+
  labs(title = "Recent polling")

plot_all +
  geom_line(size = 1)+
  scale_x_date(limits = c(max(top_six_averages$Date)-30,max(top_six_averages$Date))) +
  scale_y_log10(limits = c(0.5,30))+
  labs(title = "Recent polling (log scaled)")

plot_low +
  geom_line(size = 1)+
  scale_x_date(limits = fifth_campaign_qualifier_cutoffs)+
  scale_y_continuous(limits = c(0,4))
  labs(title = "Aggregated polls, two-week rolling average (September 13th polls and later)")

ggplot(media_total_joined_filtered, aes(y = Coverage_share,x = Polling_before, color = name,label = name, group = NA)) +
  #geom_point(alpha = 0.2)+
  #geom_smooth(method = "lm", se = FALSE)+
  geom_text(data = media_averaged)+
  geom_point(data = media_averaged)+
  facet_wrap(~Month)
ggplot(big_picture,aes(x = Polling, y = combined, color = name, label = name))+
  geom_point()+
  geom_text_repel()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(candidate_coverage_monthly,aes(x=Polling,y=combined,label = candidate_name))+
  geom_text_repel()+
  geom_point()+
  facet_wrap(~Month, scale = "free")+
  scale_x_continuous(limits = c(1,40))+
  #scale_x_log10(limits = c(1,40), breaks = c(1,2,5,10,20,30))+
  #scale_y_log10()+
  geom_smooth(method = 'lm',se = FALSE)

ggplot(candidate_coverage_monthly,aes(x=Polling,y=combined,label = candidate_name))+
  geom_text_repel()+
  geom_point()+
  facet_wrap(~Month, scale = "free")+
  #scale_x_continuous(limits = c(0.5,10))+
  scale_x_log10(limits = c(1,40), breaks = c(1,2,5,10,20,30))+
  scale_y_log10()+
  stat_function(fun = function(x) sum(combined)/100*x)

ggplot(candidate_coverage,aes(x=Polling,y=combined,label = candidate_name))+
  geom_text_repel()+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  scale_x_continuous(limits = c(1,35), breaks = c(1,2,5,10,15,20,30))+
  labs(x = "Weighted average of polls",
       y = "Total number of stories and clips",
       title = "Candidate media coverage versus horse-race polling numbers")

ggplot(president_primary_polls %>% filter(candidate_name %in% candidate_coverage$candidate_name),aes(x = end_date,y = pct, color = candidate_name, group = candidate_name, alpha = Weight))+
  geom_jitter()+
  scale_y_log10(limits = c(1,40),breaks = c(1,2,5,10,20,40))
