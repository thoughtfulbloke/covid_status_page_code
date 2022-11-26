
### colourblind friendly 
six_cols <- (colorblind_pal()(6))


weekly_deaths <- read_csv("../miscData/statsnzAPIweeklyDeaths.csv",
                          col_types=cols(
                            ...1 = col_double(),
                            id = col_character(),
                            ResourceID = col_character(),
                            GeoUnit = col_logical(),
                            Geo = col_logical(),
                            Period = col_date(format = ""),
                            Duration = col_character(),
                            Label1 = col_character(),
                            Label2 = col_logical(),
                            Label3 = col_logical(),
                            Label4 = col_logical(),
                            Label5 = col_logical(),
                            Label6 = col_logical(),
                            Value = col_double(),
                            Unit = col_character(),
                            Measure = col_character(),
                            Multiplier = col_double(),
                            NullReason = col_logical(),
                            Status = col_logical()
                          ), name_repair = "unique")
latest_week = max(weekly_deaths$Period)
# for the infoshare estimates 
# for most recent time
# Population Estimates - DPE
# Table: Estimated Resident Population by Age and Sex (1991+)
# (Qrtly-Mar/Jun/Sep/Dec)
# columns for 5 year intervals to 90 then 90+
age_populations <- read_csv("../LongTermData/DPE403901_20221126_085634_38.csv", skip=3,
                            col_types = cols(
                              ...1 = col_character(),
                              `0-4 Years` = col_double(),
                              `5-9 Years` = col_double(),
                              `10-14 Years` = col_double(),
                              `15-19 Years` = col_double(),
                              `20-24 Years` = col_double(),
                              `25-29 Years` = col_double(),
                              `30-34 Years` = col_double(),
                              `35-39 Years` = col_double(),
                              `40-44 Years` = col_double(),
                              `45-49 Years` = col_double(),
                              `50-54 Years` = col_double(),
                              `55-59 Years` = col_double(),
                              `60-64 Years` = col_double(),
                              `65-69 Years` = col_double(),
                              `70-74 Years` = col_double(),
                              `75-79 Years` = col_double(),
                              `80-84 Years` = col_double(),
                              `85-89 Years` = col_double(),
                              `90 Years and Over` = col_double()), 
                            name_repair = "unique")

age_populations$Total = rowSums(age_populations[,2:20])
age_populations$`80 and over` = rowSums(age_populations[,18:20])
age_populations$`60 to 79` =  rowSums(age_populations[,14:17])
age_populations$`30 to 59` =  rowSums(age_populations[,8:13])
age_populations$`Under 30` =  rowSums(age_populations[,2:7])

quartly_pop <- age_populations %>%
  filter(!is.na(Total)) %>%
  separate(`...1`, into=c("Yr","Qt"), sep="Q", convert = TRUE) %>%
  mutate(Quarterly_date = floor_date(ISOdate(Yr, Qt*3,30) + days(3),"quarter")) %>%
  select(Quarterly_date, Total:`Under 30`) %>%
  gather(key="Label1", value="Population", Total:`Under 30`)
# since population estimates lag death info dates,
# I am just duplicating the last quarter to make a "next" entry
nextq <- quartly_pop %>%
  filter(Quarterly_date > max(Quarterly_date) - days(200)) %>%
  arrange(Label1, Quarterly_date) %>%
  group_by(Label1) %>%
  mutate(change = Population - lag(Population),
         meanchange = mean(change, na.rm=TRUE),
         Population = Population + meanchange) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(Quarterly_date = floor_date(Quarterly_date + days(120), unit="quarter")) %>%
  select(Label1, Quarterly_date, Population)
quartly_pop <- bind_rows(quartly_pop, nextq)
mortality <- weekly_deaths %>%
  select(Period, Label1, Value) %>%
  mutate(Quarterly_date = floor_date(Period, "quarter")) %>%
  inner_join(quartly_pop, by = c("Label1", "Quarterly_date")) %>% 
  mutate(mortmillion = 1000000 * Value/Population,
         Yr = year(Period)) %>%
  filter(Label1 != "Total") %>%
  arrange(Label1, Period) %>%
  group_by(Label1, Yr) %>%
  mutate(week_number = row_number()) %>%
  ungroup()
week_max <- max(mortality$Period)
week_n_max <- (mortality$week_number[mortality$Period == week_max])[1]

y22 <- mortality %>% 
  filter(Yr == 2022) %>%
  select(week_number, Label1, pop2022=Population)
ytd_aged <- mortality %>% 
  filter(week_number <= week_n_max) %>%
  inner_join(y22, by=c("week_number","Label1")) %>%
  mutate(deaths_22 = pop2022 *Value/Population) %>%
  group_by(Label1, Yr) %>%
  summarise(YTD_mortality = sum(deaths_22), .groups = "drop") %>%
  mutate(YrC = as.character(Yr)) %>%
  select(-Yr)
ytd_tot <- ytd_aged %>%
  group_by(YrC) %>%
  summarise(YTD_mortality = sum(YTD_mortality), .groups = "drop") %>%
  mutate(Label1 = "Total")
post2020 = mean(tail(ytd_tot$YTD_mortality,3))
ytd_tot <- ytd_tot %>%
  bind_rows(data.frame(YrC="2020 to\npresent", YTD_mortality=post2020, Label1="Total"))
ytd_deaths <- bind_rows(ytd_aged, ytd_tot) %>%
  mutate(Label1 = factor(Label1, levels=c("Under 30", "30 to 59",
                                          "60 to 79", "80 and over",
                                          "Total")),
         colchoice = case_when(YrC == "2022" ~ six_cols[2],
                               YrC == "2020 to\npresent" ~ six_cols[3],
                               TRUE ~ six_cols[1])) 


#####
ccases <- read_csv("https://github.com/minhealthnz/nz-covid-data/raw/main/cases/covid-case-counts.csv")
cdeaths <- read_csv("https://github.com/minhealthnz/nz-covid-data/raw/main/cases/weekly-deaths.csv")
deathsc <- cdeaths %>%
  filter(`Week ending` <= week_max,
         `Week ending` >= ymd("2022-01-01")) %>%
  summarise(deas = sum(`Deaths attributable to COVID-19`))
casesc <- ccases %>%
  filter(`Report Date` > ymd("2021-12-26"),
         `Report Date` <= week_max) %>%
  summarise(ceas = sum(`Number of cases reported`))
protective = round(casesc$ceas[1]/deathsc$deas[1],0)
dfcov <- data_frame(xlow=1.05, xhigh=1.4,ylow=ytd_tot$YTD_mortality[12]-deathsc$deas[1], yhigh=ytd_tot$YTD_mortality[12], cate="Total") %>%
  mutate(Label1 = factor(cate, levels=c("Under 30", "30 to 59",
                                          "60 to 79", "80 and over",
                                          "Total")))
########
legpos="none"
theme_davidhood <- function(){
  # theme_minimal(base_family="OpenSans") %+replace% 
  theme_minimal(base_size = 6) %+replace% 
    theme(panel.grid = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size=0.1),
          axis.ticks.y = element_line(size=0.2),
          axis.text.y = element_text(size=6),
          strip.background = element_rect(fill= "#FFFFFF", colour="#EFEFEF"),
          strip.text = element_text(size = 6,
                                    margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")),
          strip.placement = "inside",
          panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
          panel.spacing = unit(1, "lines"),
          plot.title = element_text(size = 8,
                                    lineheight = 1.23,
                                    margin=margin(t = 0, r = 0, b = 10, l = 10, unit = "pt"),
                                    hjust=0),
          plot.subtitle = element_text(size = 7, hjust=0),
          plot.background = element_rect(fill = "#F5F5F5"),
          axis.title = element_text(size=7),
          plot.caption = element_text(margin=margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                                      size=5, hjust=1),
          plot.caption.position = "plot",
          plot.margin = margin(12, 18, 12, 12, "pt"),
          legend.position = legpos)
}

graf <- ggplot(ytd_deaths,aes(y=YTD_mortality, label=YrC)) +
  geom_rect(data=dfcov, aes(xmin=xlow, xmax=xhigh,ymin=ylow,ymax=yhigh, label=NULL, y=NULL), fill=six_cols[2], alpha=0.1) +
  geom_vline(xintercept=1) + 
  geom_text_repel(aes(colour=colchoice), x=1, xlim=c(1.4,1.6), size=2) +
  geom_point(aes(colour=colchoice), x=1, size=1) +
  facet_wrap(~ Label1, ncol=5, scales = "free_y") +
  theme_davidhood() + xlim(0.95,1.6) +
  scale_colour_identity() +
  labs(title=paste0("NZ Year To Date total all cause deaths (2022 equivalent population). Week 1 to Week ", week_n_max, " (inc), by age group"),
       subtitle=paste0("For 2022, week ", week_n_max," is the week ending ",week_max,", 2020: Minimal covid, almost no other diseases\n2021: A little covid, some other diseases return, 2022: Much covid, normal other diseases return.\n","Orange Rectangle is covid deaths within 2022 total (thanks to vaccines saving lives, only 1 in ", protective, " covid cases caused death)\n"),
       x="\n", y="Deaths in 2022 population equivalents\n",
       caption="By @thoughtfulnz on twitter & mastodon.nz\nSource:StatsNZ Open Data weekly deaths, Infoshare population, MoH Github covid cases and covid deaths")
graf
ggsave(filename="~/Desktop/NZcovidGraphs/allex.png", plot=graf,dpi=300, 
       units="in", bg="white", height = 4.5, 
       width=8)
