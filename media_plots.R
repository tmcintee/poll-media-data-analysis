ggplot(candidate_coverage, aes(x = factor(candidate_name,
                                          levels = candidate_coverage$candidate_name[order(candidate_coverage$combined,
                                                                                           decreasing = TRUE)]),
                               y = combined)) +
  geom_bar(stat= "identity") +
  labs(y = "Total media coverage",
       x= "Candidate",
       title = "Media coverage of Democratic presidential candidates, December 2018-October 2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggplot(candidate_coverage, aes(x = Polling, y = combined, label = candidate_name, color = )) +
  geom_point() +
  expand_limits(x = 0, y = 0)+
  geom_text_repel() +
  #geom_smooth(method = 'lm', se = FALSE )+
  labs(x = "Aggregate polling",
       y = "Media coverage",
       title = "Media coverage versus polling",
       facet = "")+
  facet_wrap(~Facet, scale = "free")+
  theme(strip.text.x = element_blank())
