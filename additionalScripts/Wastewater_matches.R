ww <- read_csv("../miscData/esrGithubww_site.csv")
sites <- read_csv("../miscData/esrGithubsites.csv")
wwcounts <- read_csv("../miscData/esrGithubcases_site.csv")
step1 <- ww %>% 
  inner_join(wwcounts, by = c("week_end_date","SampleLocation")) %>%
  filter(week_end_date > ymd("2022-02-07")) %>%
  arrange(SampleLocation, week_end_date) %>%
  group_by(SampleLocation) %>%
  mutate(after_week = lead(week_end_date),
         before_week = week_end_date,
         after_copies = lead(copies_per_day_per_person),
         before_copies = copies_per_day_per_person,
         after_cases = lead(case_7d_avg),
         before_cases = case_7d_avg) %>%
  ungroup() %>%
  rename(latest_week = week_end_date, 
         latest_copies = copies_per_day_per_person,
         latest_cases = case_7d_avg)
latest <- step1 %>%
  group_by(SampleLocation) %>%
  slice(n()) %>%
  ungroup() %>%
  select(SampleLocation, latest_week, latest_copies, latest_cases) %>%
  filter(latest_week == max(latest_week))
ww_week_end <- format(latest$latest_week[1], "%a %d %b %Y")
wwtimematched <- step1 %>% 
  group_by(SampleLocation) %>%
  slice(1:(n()-1)) %>%
  ungroup() %>%
  select(SampleLocation, after_week, before_week,
         after_copies, before_copies, after_cases, before_cases) %>%
  inner_join(latest, by="SampleLocation") %>%
  filter(latest_copies >= before_copies & 
           latest_copies <= after_copies |
           latest_copies <= before_copies &
           latest_copies >= after_copies) %>%
  arrange(SampleLocation, before_week) %>%
  group_by(SampleLocation) %>%
  slice(1) %>%
  ungroup() %>%
  inner_join(sites, by="SampleLocation") %>%
  mutate(past_change = (latest_copies-before_copies)/
           (after_copies-before_copies),
         past_cases = before_cases + past_change * 
           (after_cases-before_cases))

ranges <- wwtimematched %>% 
  select(shp_label, before_week,after_week, latest_cases,past_cases) %>%
  distinct() %>%
  filter(!is.na(shp_label)) %>%
  group_by(shp_label, before_week,after_week, latest_cases) %>%
  summarise(past_cases = round(mean(past_cases),0), .groups="drop") %>%
  arrange(shp_label) %>% select(shp_label, before_week,after_week)
  


