six_cols <- colorblind_pal()(6)
case_counts <- read_csv("../miscData/mohGithubCovidCaseCounts.csv") 

aggregated <- case_counts %>%
  group_by(Date = `Report Date`) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop")
# want 0 entries not sparce data, so make a zero entry for each date
zeros <- expand.grid(Date = seq.Date(from=min(aggregated$Date),
                                     to=max(aggregated$Date),
                                     by="day"), 
                     Cases = 0)

aggregated <- bind_rows(aggregated, zeros) %>%
  arrange(Date, desc(Cases)) %>%
  group_by(Date) %>%
  slice(1) %>% #for each date keep the higher number
  ungroup() %>%
  mutate(sevenday_mean = roll_meanr(Cases,7),
         meanchange = sevenday_mean - lag(sevenday_mean,1),
         prechange = lag(sevenday_mean,1) - lag(sevenday_mean,2),
         Change = case_when(meanchange > 0 & meanchange > prechange ~
                              "Cases Growing & growing faster",
                            meanchange > 0 & meanchange < prechange ~
                              "Cases Growing & growing slower",
                            meanchange < 0 & meanchange > prechange ~
                              "Cases Falling & falling slower",
                            meanchange < 0 & meanchange < prechange ~
                              "Cases Falling & falling faster"),
         Change = factor(Change)) %>%
  slice((n()-27):n())
aggregated$minimumCase = min(aggregated$Cases)
latestDay = max(aggregated$Date)
latestCases = aggregated$Cases[28]
graf <- ggplot(aggregated, aes(x=Date)) +
  geom_point(aes(y=Cases), alpha=0.35) +
  geom_line(aes(y=sevenday_mean)) +
  scale_colour_manual(values=six_cols, drop=FALSE) +
  scale_fill_viridis_d(end=0.9, option = "inferno") +
  geom_rect(aes(xmin=Date - 0.5, xmax=Date + 0.5,
                fill=Change,
                ymin= minimumCase - 300,
                ymax= minimumCase - 100)) +
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
  labs(title = paste("NZ new covid cases, past 28 days. Most recent day", latestCases, "cases"),
subtitle = paste("Line shows 7 day trend. Current to", latestDay, "\n"),
y="Reported new cases\n",
x= "",
caption="By @thoughtfulnz on twitter & mastodon.nz\nSource data: cases NZ MoH github") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave(filename="~/Desktop/NZcovidGraphs/TotalCases_trend.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")


