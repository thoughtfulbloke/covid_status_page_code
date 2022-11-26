sixcols= colorblind_pal()(6)

case_counts <- read_csv("../miscData/mohGithubCovidCaseCounts.csv") %>%
  mutate(Age=case_when(`Age group` == "90+" ~ "70+",
                       `Age group` == "80 to 89" ~ "70+",
                       `Age group` == "70 to 79" ~ "70+",
                       TRUE ~ `Age group`))
  
zeros <- expand.grid(Date = unique(case_counts$`Report Date`),
                     Age = unique(case_counts$Age),
                     Cases = 0)

ages_June_2022 = "Age,Population
0 to 9,625490
10 to 19,655720
20 to 29,679450
30 to 39,733760
40 to 49,631220
50 to 59,654040
60 to 69,561800
70+,582600"
statsNZpop <- read.csv(text=ages_June_2022)


firsts <- case_counts %>%
  filter(`Case Status` == "Confirmed",
         !(District %in% c("At the border", "Unknown")),
         `Infection status` == "First") %>%
  group_by(Date = `Report Date`,Age) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop") %>%
  bind_rows(zeros) %>%
  arrange(Age, Date, desc(Cases)) %>%
  group_by(Age, Date) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(Age) %>%
  mutate(cumulative_firsts = cumsum(Cases),
         cum28 = lag(cumulative_firsts,28),
         cum90 = lag(cumulative_firsts,90),
         pool28to90 = cum28-cum90) %>%
  ungroup() %>%
  inner_join(statsNZpop, by="Age")

reinf_under90 <- case_counts %>%
  filter(`Case Status` == "Confirmed",
         !(District %in% c("At the border", "Unknown")),
         `Infection status` == "Reinfection (< 90 days)") %>%
  group_by(Date = `Report Date`,Age) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop") %>%
  bind_rows(zeros) %>%
  arrange(Age, Date, desc(Cases)) %>%
  group_by(Age, Date) %>%
  slice(1) %>%
  ungroup() %>%
  rename(cases_u90 = Cases) %>%
  inner_join(firsts, by = c("Date", "Age"))
  
reinf_over90 <- case_counts %>%
  filter(`Case Status` == "Confirmed",
         !(District %in% c("At the border", "Unknown")),
         `Infection status` == "Reinfection") %>%
  group_by(Date = `Report Date`,Age) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop") %>%
  bind_rows(zeros) %>%
  arrange(Age, Date, desc(Cases)) %>%
  group_by(Age, Date) %>%
  slice(1) %>%
  ungroup() %>%
  rename(cases_o90 = Cases) %>%
  inner_join(firsts, by = c("Date", "Age"))

rate_first <- firsts %>%
  mutate(denominator = Population - cumulative_firsts,
         rate = 10000*Cases/denominator,
         seven_day = roll_meanr(rate,7),
         Series = "First Infection") %>%
  select(Age, Date, seven_day, Series) %>%
  filter(!is.na(seven_day))
  
rate_under90 <- reinf_under90 %>%
  mutate(rate = 10000*cases_u90/pool28to90,
         seven_day = roll_meanr(rate,7),
         Series = "Reinfection within 90 days") %>%
  select(Age, Date, seven_day, Series) %>%
  filter(!is.na(seven_day))

rate_over90 <- reinf_over90 %>%
  mutate(rate = 10000*cases_o90/cum90,
         seven_day = roll_meanr(rate,7),
         Series = "Reinfection over 90 days") %>%
  select(Age, Date, seven_day, Series) %>%
  filter(!is.na(seven_day))
#######################################
un90 <- rate_under90 %>%
  inner_join(rate_first %>% 
               select(Age,Date, prime = seven_day), 
             by = c("Age","Date")) %>%
  mutate(as_percent = 100*seven_day/prime)
ov90 <- rate_over90 %>%
  inner_join(rate_first %>% 
               select(Age,Date, prime = seven_day), 
             by = c("Age","Date")) %>%
  mutate(as_percent = 100*seven_day/prime)

######################
graf <- bind_rows(un90,ov90) %>% 
  filter(Date >= ymd("2022-05-01")) %>%
  ggplot(aes(x=Date, y=as_percent, colour=Series)) +
  geom_smooth(method="gam") + 
  facet_wrap(~Age, ncol=4, scales="free_y") +
  scale_colour_manual(values=sixcols) +
  theme_minimal() +
  theme(legend.position="top") +
  labs(title="GAM smoothed NZ Reinfections as percentage of current 7 day rate of first infection",
       subtitle="Rates based on uninfected population for first infections,
infected population within time window for reinfections\n",
y="Percentage", x="",
caption= "By @thoughtfulnz on twitter & mastodon.nz\nSources:MoH Github case counts, StatsNZ population estimates")

######################
ggsave(filename="~/Desktop/NZcovidGraphs/AgeReinfections.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")
  
  

