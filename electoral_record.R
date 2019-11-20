electoral_record <- read.csv("electoral_history_presidential_candidates.csv")
names(electoral_record)[[1]] <- "Candidate"
for(selectedState in unique(electoral_record$State))
{
  g <- ggplot(electoral_record %>% filter(State == selectedState),
              aes(x=Against, y= For, color = Highlight, label = Tag)) +
    geom_text_repel() +
    facet_wrap(~Year) +
    theme(legend.position = "none")+
    coord_equal() +
    geom_abline(slope = 1, intercept = 0) +
    geom_point()+
    labs(title = paste0("Elections in ",selectedState), y = "Democratic votes", x = "Republican votes")
  print(g)
}
electoral_record <- electoral_record
electoral_record <- electoral_record %>% mutate(For_pct = (For / Total), Against_pct = (Against / Total))
electoral_record$Tag[electoral_record$Highlight == 0] <- " "
#electoral_record$Year[electoral_record$Year < 2008] <- "Before 2008"
#electoral_record$Year <- as.character(electoral_record$Year)
ggplot(electoral_record,
       aes(x=Against, y= For, color = Tag, label = Tag, alpha = Year)) +
  geom_text_repel() +
  facet_wrap(~State, scale = "free") +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5) +
  geom_point() +
  geom_point(inherit.aes = FALSE,aes(x=For, y= Against), alpha = 0)+
  guides(color = FALSE, alpha = FALSE) + labs(x = "Votes for Republican candidate",
                                              y = "Votes for non-Republican candidate", title = "Democratic presidential candidates and their electoral performance in past statewide elections")
electoral_record <- electoral_record %>% group_by(State,Year) %>% mutate(for_vs_med = For - median(For),
                                                                         against_vs_med = Against - median(Against),
                                                                         net_vs_med = For - median(For) -Against + median(Against),
                                                                         for_pct_vs_med = For_pct - median(For_pct),
                                                                         against_pct_vs_med = Against_pct - median(Against_pct),
                                                                         net_pct_vs_med = For_pct - Against_pct + median(Against_pct) - median(For_pct))
summary_electoral_record <- electoral_record %>%
  group_by(Candidate) %>%
  filter(Highlight == 1) %>%
  summarise(Mean_for_vs_med = mean(for_vs_med),
            Mean_against_vs_med = mean(against_vs_med),
            Mean_net_vs_med = mean(net_vs_med),
            Mean_for_pct_elec <- mean(for_pct_vs_med),
            Mean_against_pct_elec <- mean(against_pct_vs_med),
            Mean_net_pct_elec <- mean(net_pct_vs_med))
