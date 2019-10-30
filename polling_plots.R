
ggplot(top_six_averages,aes(x=Date,y=Polling, color = Candidate))+
  #geom_smooth(alpha = 0.1)+
  geom_line(size=1)+
  scale_color_manual(values = colors_instance[1:6])+
  #geom_smooth(data=media_total_joined_filtered,aes(y=Coverage_share),alpha = 0.5)+
  scale_y_log10(breaks = c(0.5,1,2,5,10,20,40), limits = c(0.5,40))+
  facet_wrap(~Candidate,nrow = 2)
ggplot(next_six_averages,aes(x=Date,y=Polling, color = Candidate))+
  #geom_smooth(alpha = 0.1)+
  geom_line(size=1)+
  scale_color_manual(values = colors_instance[7:12])+
  #geom_smooth(data=media_total_joined_filtered,aes(y=Coverage_share),alpha = 0.5)+
  scale_y_log10(breaks = c(0.5,1,2,5,10,20,40), limits = c(0.5,40))+
  facet_wrap(~Candidate,nrow = 2)

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
