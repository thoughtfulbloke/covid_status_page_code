six_cols <- colorblind_pal()(6)
smoothing <- 0.55
cases <- read_csv("../miscData/mohGithubCovidCaseCounts.csv") %>%
  group_by(Date = `Report Date`) %>%
  summarise(n=sum(`Number of cases reported`))
hospitalisations <- read_excel("../miscData/daily_hospitalisations.xlsx") %>%
  mutate(in_hospital = as.numeric(`COVID cases in hospital`),
         Date = as.Date(Date))
admissionsh <- read_csv("../miscData/mohGithubHospitalAdmissions.csv") %>% filter(`Variable Label` == "Age band") %>%
  group_by(Date = `Admissions for COVID-19 in the week ending`) %>% summarise(weektot=sum(Hospitalisations, na.rm=TRUE), .groups="drop")
admissionsi <- read_csv("../miscData/mohGithubHospitalAdmissions.csv") %>% filter(`Variable Label` == "Age band") %>%
  group_by(Date = `Admissions for COVID-19 in the week ending`) %>% summarise(weektot=sum(ICU, na.rm=TRUE), .groups="drop")
deaths28 <- read_csv("../miscData/mohGithubDeaths.csv") %>% 
  select(Date = `Week ending`, weektot = `Deaths within 28 days of being reported as case`)
deaths_contrib <- read_csv("../miscData/mohGithubDeaths.csv") %>%
  select(Date = `Week ending`, weektot = `Deaths attributable to COVID-19`)

deaths <- read_csv("../miscData/mohGithubDeaths.csv")

zero_padding <- data.frame(Date=seq.Date(from=min(cases$Date),
                                         to=max(cases$Date),
                                         by="day"),
                           n=0)

c7 <- cases %>%
  bind_rows(zero_padding) %>%
  arrange(Date, desc(n)) %>%
  group_by(Date) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(n = roll_meanr(n,7,na.rm = TRUE)/400,
         dataset = "Cases/400")
h7 <- hospitalisations %>%
  group_by(Date) %>%
  summarise(n = sum(in_hospital), .groups="drop") %>%
  mutate(n = roll_meanr(n,7,na.rm = TRUE)/45,
         dataset = "Hospitalisations/45")

nil_padding <- zero_padding %>% select(-n)
ah7 <- admissionsh %>%
  bind_rows(nil_padding) %>%
  arrange(Date) %>%
  mutate(n=weektot/7) %>%
  fill(n,.direction = "up") %>%
  mutate(n=lag(n)/5, dataset = "Hospital Admissions") %>%
  filter(is.na(weektot), !is.na(n)) %>%
  select(-weektot)
ai7 <- admissionsi %>%
  bind_rows(nil_padding) %>%
  arrange(Date) %>%
  mutate(n=weektot/7) %>%
  fill(n,.direction = "up") %>%
  mutate(n=lag(n), dataset = "ICU Admissions") %>%
  filter(is.na(weektot), !is.na(n)) %>%
  select(-weektot)
d28 <- deaths28 %>%
  bind_rows(nil_padding) %>%
  arrange(Date) %>%
  mutate(n=weektot/7) %>%
  fill(n,.direction = "up") %>%
  mutate(n=lag(n), dataset = "Deaths in 28 days") %>%
  filter(is.na(weektot), !is.na(n)) %>%
  select(-weektot)
dwas <- deaths_contrib %>%
  bind_rows(nil_padding) %>%
  arrange(Date) %>%
  mutate(n=weektot/7) %>%
  fill(n,.direction = "up") %>%
  mutate(n=lag(n), dataset = "Deaths covid contributed to") %>%
  filter(is.na(weektot), !is.na(n)) %>%
  select(-weektot)

setsData <- bind_rows(c7,h7, ah7,ai7,d28,dwas)  %>% 
  filter(Date > ymd("22-01-01")) 
graf <- ggplot(setsData, aes(x=Date,y=n, colour=dataset, linetype=dataset, shape=dataset)) + 
  geom_line() + geom_point(data=setsData %>% 
                             filter(mday(Date)==1)) +
  scale_colour_viridis_d(end=0.9) +
  scale_linetype_manual(values=c(1:3,1:3)) +
  theme_minimal(base_family="OpenSans",
                base_size = 8) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        axis.ticks.x = element_line(size=0.2),
        axis.line.y.left = element_line(size=0.1),
        axis.ticks.y.left = element_line(size=0.2),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FCFCFC"),
        plot.caption = element_text(margin=margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                                    size=7, hjust=1),
        plot.caption.position = "plot",
        legend.position = "top",
        panel.grid.major = element_blank(),
        legend.key.size = unit(0.5, 'cm')) +
  labs(title = "For NZ, cases (divided by 400), Deaths covid contributed to, Deaths within 28 days,
       Hospitalisations (divided by 45), Hospital Admissions, ICU admissions",
       y="Count\n",
       x= "",
       caption="By @thoughtfulnz on twitter & mastodon.nz\nSource data: NZ MoH github") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave(filename="~/Desktop/NZcovidGraphs/Everything.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")
  


