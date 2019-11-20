president_polls$state_abb <- state.abb[match(president_polls$state,state.name)]
president_polls$state_abb[is.na(president_polls$state_abb)] <- "US"
president_polls$median_rep <- NA
president_polls$median_dem <- NA
president_polls$diff_dem <- NA
president_polls$diff_rep <- NA
for(each_id in president_polls$poll_id)
{
  small_president <- president_polls %>% filter(poll_id == each_id)
  median_rep_poll <- median((small_president %>% filter(candidate_party == "REP"))$pct)
  median_dem_poll <- median((small_president %>% filter(candidate_party == "DEM"))$pct)
  president_polls[president_polls$poll_id == each_id,"median_rep"] <- median_rep_poll
  president_polls[president_polls$poll_id == each_id,"median_dem"] <- median_dem_poll
}
for(i in 1:nrow(president_polls))
{
  party <- president_polls$candidate_party[[i]]
  question <- president_polls$question_id[[i]]
  if(sum(president_polls$question_id == question)==2)
  {
    if(party == "REP")
    {
      president_polls$diff_rep[[i]] <-  president_polls$pct[[i]] - president_polls$median_rep[[i]]
      max_dem_opp <- max((president_polls %>% filter(question_id == question, candidate_party == "DEM"))$pct)
      president_polls$diff_dem[[i]] <- max_dem_opp - president_polls$median_dem[[i]]
    }
    else if(party == "DEM")
    {
      president_polls$diff_dem[[i]] <-  president_polls$pct[[i]] - president_polls$median_dem[[i]]
      max_rep_opp <- max((president_polls %>% filter(question_id == question, candidate_party == "REP"))$pct)
      president_polls$diff_rep[[i]] <- max_rep_opp - president_polls$median_rep[[i]]
    }
  }
}
president_polls <- president_polls %>% mutate(net_swing = diff_dem-diff_rep)
avgs_h2h <- president_polls %>%
  group_by(candidate_name) %>%
  summarise(avg_swing = wtd.mean(net_swing,Weight),
            count = n()) %>%
  arrange(-avg_swing)
president_polls$net_swing <- president_polls$diff_dem - president_polls$diff_rep
president_polls$candidate_name <- factor(president_polls$candidate_name,
                                         levels = (president_polls %>%
                                                     group_by(candidate_name) %>%
                                                     summarise(avg_swing = wtd.mean(net_swing,Weight)) %>%
                                                     arrange(-avg_swing))$candidate_name)
pres_polls_top_candidates <- president_polls %>% filter(candidate_party == "DEM") %>% group_by(candidate_name) %>% summarise(Polls = n()) %>% arrange(-Polls)
top_six <- pres_polls_top_candidates$candidate_name[1:6]
ggplot(president_polls %>% filter(candidate_name %in% top_six),aes(x = diff_rep, y= diff_dem, label = state_abb, color = candidate_name,alpha = Weight))+
  geom_text()+
  geom_abline()+
  facet_wrap(~candidate_name)+
  guides(label = FALSE, color = FALSE, alpha = FALSE)+
  labs(x = "Change in vote for Republican from median in same poll",
     y = "Change in vote for Democrat from median in same poll",
     title = "2020 head to head polling summarized")


