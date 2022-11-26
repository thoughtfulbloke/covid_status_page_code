six_cols <- colorblind_pal()(6)
smoothing <- 0.55
raw_hospitalisations <- read_excel("../miscData/daily_hospitalisations.xlsx") %>%
  mutate(in_hospital = as.numeric(`COVID cases in hospital`),
         Date = as.Date(Date)) %>%
  filter(Date > ymd("2022-05-11"))
aggregated <- raw_hospitalisations %>%
  group_by(Date) %>%
  summarise(Cases = sum(in_hospital), .groups="drop") %>%
  mutate(sevenday_mean = roll_meanr(Cases,7, na.rm = TRUE),
         meanchange = sevenday_mean - lag(sevenday_mean,1),
         prechange = lag(sevenday_mean,1) - lag(sevenday_mean,2),
         Change = case_when(meanchange > 0 & meanchange > prechange ~
                              "Hospitalisations Growing & growing faster",
                            meanchange > 0 & meanchange < prechange ~
                              "Hospitalisations Growing & growing slower",
                            meanchange < 0 & meanchange > prechange ~
                              "Hospitalisations Falling & falling slower",
                            meanchange < 0 & meanchange < prechange ~
                              "Hospitalisations Falling & falling faster",
                            meanchange == 0 ~ "Stable"),
         Change = factor(Change,
                         levels=c("Hospitalisations Falling & falling faster",
                                  "Hospitalisations Falling & falling slower",
                                  "Stable",
                                  "Hospitalisations Growing & growing faster",
                                  "Hospitalisations Growing & growing slower"))) %>%
  slice((n()-27):n())
aggregated$minimumCase = min(aggregated$Cases, na.rm=TRUE)
latestDay = max(aggregated$Date)
latestCases = aggregated$Cases[28]
graf <- ggplot(aggregated, aes(x=Date)) +
  geom_point(aes(y=Cases), alpha=0.35) +
  geom_line(aes(y=sevenday_mean)) +
  geom_rect(aes(xmin=Date - 0.5, xmax=Date + 0.5, 
                fill=Change,
                ymin= minimumCase - 40,
                ymax= minimumCase - 20)) +
  scale_fill_viridis_d(end=0.9, option = "inferno", drop=FALSE) +
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
  labs(title = paste("NZ covid cases in hospital, past 28 days"),
       subtitle = paste("Line shows 7 day trend. Current to", latestDay, "\n"),
       y="Nightly hospitalisations\n",
       x= "",
       caption="By @thoughtfulnz on twitter & mastodon.nz\nSource data: hospitalisations NZ MoH github") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave(filename="~/Desktop/NZcovidGraphs/TotalHospital_trend.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")


