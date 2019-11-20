require(tidyverse)
require(ggrepel)
candidate_coverage_display <- candidate_coverage
candidate_coverage_monthly_display <- candidate_coverage_monthly
candidate_coverage_display$Facet <- candidate_coverage$Polling >=1
candidate_coverage_monthly_display$Facet <- candidate_coverage_monthly$Polling < 2
candidate_coverage_display$candidate_name[candidate_coverage_display$candidate_name == "Bill Blasio"] <- "Bill de Blasio"
candidate_coverage_monthly_display$candidate_name[candidate_coverage_monthly_display$candidate_name == "Bill Blasio"] <- "Bill de Blasio"

ggplot(candidate_coverage_display, aes(x = factor(candidate_name,
                                          levels = candidate_coverage_display$candidate_name[order(candidate_coverage_display$combined,
                                                                                           decreasing = TRUE)]),
                               y = combined)) +
  geom_bar(stat= "identity") +
  labs(y = "Total media coverage",
       x= "Candidate",
       title = "Media coverage of Democratic presidential candidates, December 2018-October 2019") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggplot(candidate_coverage_display, aes(x = Polling, y = combined, label = candidate_name, color = candidate_name)) +
  geom_point() +
  expand_limits(x = 0, y = 0)+
  #scale_y_continuous(breaks = c(2000*1:4,10000*1:9))+
  geom_text_repel() +
  #geom_smooth(method = 'lm', se = FALSE )+
  labs(x = "Aggregate polling",
       y = "Media coverage",
       title = "Media coverage versus polling",
       facet = "")+
  facet_wrap(~Facet, scale = "free")+
  theme(strip.text.x = element_blank(),
        legend.position = "none")

ggplot(candidate_coverage_monthly_display %>% filter(Month != "December"), aes(x = Polling, y = combined, label = candidate_name, color = candidate_name)) +
  geom_point() +
  scale_x_log10()+
  scale_y_log10()+
  #geom_text_repel() +
  #geom_smooth(method = 'lm', se = FALSE )+
  labs(x = "Aggregate polling",
       y = "Media coverage",
       title = "Media coverage versus polling",
       color = "Candidate")+
  facet_wrap(~Month, scales = "free", nrow = 2)
ggplot(candidate_coverage_monthly_display %>% filter(Facet == FALSE, Month != "December"), aes(x = Polling, y = combined, label = candidate_name, color = candidate_name)) +
  geom_point(alpha = 0.5) +
  expand_limits(x = 0, y = 0)+
  geom_text_repel() +
  #geom_smooth(method = 'lm', se = FALSE )+
  labs(x = "Aggregate polling",
       y = "Media coverage",
       title = "Media coverage versus polling")+
  facet_wrap(~Month, scales = "free", nrow = 2)+
  theme(legend.position = "none",
        strip.text.y = element_blank())
ggplot(candidate_coverage_monthly_display, aes(x = Polling, y = combined, label = candidate_name, color = candidate_name)) +
  geom_point(alpha = 0.5) +
  expand_limits(x = 0, y = 0)+
  #geom_text_repel() +
  #geom_smooth(method = 'lm', se = FALSE )+
  labs(x = "Aggregate polling",
       y = "Media coverage",
       title = "Media coverage versus polling")+
  facet_wrap(~Month, scale = "free")



ggplot(media_total_joined_clean, aes(x = name, y = Coverage_PPP,))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  labs(title = "Coverage relative to polling numbers in weeks where aggregate polling is better than 0.5%", x = "Candidate", y="Coverage per point of polling")

ggplot(elo_DF, aes(x = name, y= Elo, fill = Polling))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(trans = "log", breaks = c(0.5,1,2,5,10,25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(breaks = c(0,200,400,600,800,1000,1200,1400,1600))+
  labs(title = "Degree of media preference for candidate",
       x = "Candidate",
       y = "Media preference")

ggplot(snubbed_coverage, aes(x = Candidate, y= total_snub_coverage))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  labs(title = "Snubs coverage",
       x = "Candidate",
       y = "Total difference in coverage")

ggplot(media_total_joined_clean, aes(x = Polling_at, y = combined))+
  geom_smooth(method = "lm") +
  geom_point()+
  facet_wrap(~Month)
