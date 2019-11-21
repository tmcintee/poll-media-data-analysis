source("electoral_record.R")
source("H2H_polls.R")

fifth_debate_electability_summary <- full_join(avgs_h2h,summary_electoral_record) %>%
  filter(Candidate %in% c("Joe Biden",
                                                                                                              "Bernie Sanders",
                                                                                                              "Elizabeth Warren",
                                                                                                              "Pete Buttigieg",
                                                                                                              "Kamala Harris",
                                                                                                              "Andrew Yang",
                                                                                                              "Cory Booker",
                                                                                                              "Tulsi Gabbard",
                                                                                                              "Tom Steyer",
                                                                                                              "Amy Klobuchar")) %>%
  select(Candidate,poll.count,avg_swing,elec.count,Net) %>%
  mutate(elec.wt = sqrt(10*elec.count),poll.wt = sqrt(poll.count)) %>%
  mutate(elec = elec.wt*Net,poll = poll.wt*avg_swing) %>%
  replace_na(replace = list(elec = 0, poll = 0, elec.wt = 0, poll.wt = 0))

fifth_debate_electability_summary$Office <- -100/3
fifth_debate_electability_summary$Office[fifth_debate_electability_summary$Candidate %in% c("Andrew Yang",
                                                                                            "Tom Steyer",
                                                                                            "Marianne Williamson")] <- 100/3-100/3
fifth_debate_electability_summary$Office[fifth_debate_electability_summary$Candidate %in% c("Elizabeth Warren",
                                                                                            "Kamala Harris",
                                                                                            "Cory Booker",
                                                                                            "Bernie Sanders",
                                                                                            "Amy Klobuchar",
                                                                                            "Michael Bennet",)] <- 25-100/3
fifth_debate_electability_summary$Office[fifth_debate_electability_summary$Candidate %in% c("Tulsi Gabbard")] <- 40-100/3
fifth_debate_electability_summary$Office[fifth_debate_electability_summary$Candidate %in% c("Joe Biden")] <- 100*5/9-100/3

fifth_debate_electability_summary$Ideology <- 0
fifth_debate_electability_summary$Ideology[fifth_debate_electability_summary$Candidate %in% c("Bernie Sanders",
                                                                                            "Elizabeth Warren")] <- -10
fifth_debate_electability_summary$Ideology[fifth_debate_electability_summary$Candidate %in% c("Joe Biden",
                                                                                              "Amy Klobuchar")] <- 10
fifth_debate_electability_summary$Outsider <- 0
fifth_debate_electability_summary$Outsider[fifth_debate_electability_summary$Candidate %in% c("Andrew Yang",
                                                                                              "Bernie Sanders",
                                                                                              "Tulsi Gabbard",
                                                                                              "Tom Steyer")] <- 5



fifth_debate_electability_summary <- fifth_debate_electability_summary %>%
  mutate(score = (elec + poll + Office + Ideology + Outsider)/(elec.wt + poll.wt + 3))

