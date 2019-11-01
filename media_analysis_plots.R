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

fn <- function(x)
{
  x*sum(candidate_clips_monthly)/100
}
candidate_horserace_polling <- candidate_horserace_polling %>% ungroup()
candidate_coverage$Facet <- candidate_coverage$Polling >=2
