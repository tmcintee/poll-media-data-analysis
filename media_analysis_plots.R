candidate_clips <- media_cable %>% group_by(name) %>% summarise(clips = sum(matched_clips))
candidate_stories <- media_online %>% group_by(name) %>% summarise(stories = sum(matched_stories))
candidate_coverage <- inner_join(candidate_stories,candidate_clips)
candidate_coverage$combined <- candidate_coverage$clips + candidate_coverage$stories
candidate_horserace_polling <- president_primary_polls %>%
  filter(state == "") %>%
  group_by(candidate_name) %>%
  summarise(Polling = wtd.mean(pct,Weight))
names(candidate_coverage)[[1]] <- "candidate_name"
candidate_coverage <- inner_join(candidate_coverage,candidate_horserace_polling)

candidate_clips_monthly <- media_cable %>%
  group_by(name,Month) %>%
  summarise(clips = sum(matched_clips))
candidate_stories_monthly <- media_online %>%
  group_by(name,Month) %>%
  summarise(stories = sum(matched_stories))
candidate_coverage_monthly <- inner_join(candidate_clips_monthly,candidate_stories_monthly)
candidate_coverage_monthly$combined <- candidate_coverage_monthly$clips + candidate_coverage_monthly$stories
names(candidate_coverage_monthly)[[1]] <- "candidate_name"
candidate_horserace_polling_monthly <- president_primary_polls %>%
  filter(state == "") %>%
  group_by(candidate_name,Month) %>%
  summarise(Polling = wtd.mean(pct,Weight)) %>%
  filter(Polling != 0)
candidate_coverage_monthly <- inner_join(candidate_coverage_monthly,candidate_horserace_polling_monthly)
candidate_coverage_monthly$Month <- factor(candidate_coverage_monthly$Month,levels =
                                             c("December",
                                               "January",
                                               "February",
                                               "March",
                                               "April",
                                               "May",
                                               "June",
                                               "July",
                                               "August",
                                               "September",
                                               "October",
                                               "November"))


ggplot(candidate_coverage_monthly,aes(x=Polling,y=combined,label = candidate_name))+
  geom_text_repel()+
  geom_point()+
  facet_wrap(~Month, scale = "free")+
  scale_x_continuous(limits = c(1,40))+
  #scale_x_log10(limits = c(1,40), breaks = c(1,2,5,10,20,30))+
  #scale_y_log10()+
  geom_smooth(method = 'lm',se = FALSE)
fn <- function(x)
{
  x*sum(candidate_clips_monthly)/100
}
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

candidate_horserace_polling <- candidate_horserace_polling %>% ungroup()
ggplot(president_primary_polls %>% filter(candidate_name %in% candidate_coverage$candidate_name),aes(x = end_date,y = pct, color = candidate_name, group = candidate_name, alpha = Weight))+
  geom_jitter()+
  scale_y_log10(limits = c(1,40),breaks = c(1,2,5,10,20,40))
