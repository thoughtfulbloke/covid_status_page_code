
six_cols <- colorblind_pal()(6)
smoothing <- 0.55
# saving reports from the website as html into a folder
# called news_item_html
DHBpop <-  read_csv("../LongTermData/DHB_population_June.csv")
DHBpop_21 <- DHBpop %>%
  filter(`Year as at 30 June` == 2021, `Age group` == "total",
         Sex == "Total", 
         `District health board` != "New Zealand") %>%
  mutate(DHB = case_when(`District health board` == "Capital & Coast" ~ "Capital and Coast",
                         `District health board` == "Hutt" ~ "Hutt Valley",
                         `District health board` == "Tairāwhiti" ~ "Tairawhiti",
                         `District health board` == "Waitematā" ~ "Waitemata",
                         TRUE ~ `District health board`)) %>%
  group_by(DHB) %>%
  summarise(pop21=sum(Value), .groups="drop")

hospitalisations <- read_excel("../miscData/daily_hospitalisations.xlsx") %>%
  arrange(DHB,Date) %>%
  mutate(in_hospital = as.numeric(`COVID cases in hospital`)) %>%
  group_by(DHB) %>%
  mutate(seven_day = roll_meanr(in_hospital,7, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(seven_day)) %>%
  inner_join(DHBpop_21, by="DHB") %>%
  mutate(percent = 100 *seven_day/pop21) %>%
  ungroup() %>%
  filter(Date > ymd("2022-05-11"))
#########################

theme_upper_panel <- function(){
  theme_minimal(base_family="OpenSans") %+replace% 
    theme(panel.grid = element_blank(),
          plot.title = element_text(size=8),
          axis.line.x = element_blank(),
          axis.line.y = element_line(size=0.1),
          axis.text.y = element_text(size=6),
          axis.ticks.y = element_line(size=0.2),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
          plot.background = element_rect(fill = "#FCFCFC"),
          plot.caption = element_text(margin=margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                                      size=11, hjust=1),
          plot.caption.position = "plot",
          legend.position = "right",
          panel.grid.major.y = element_line(color ="#BBBBBB", size = 0.1,linetype = 1),
          legend.text = element_text(size=4.5),
          legend.key.height = unit(0.6, 'lines'),
          legend.title = element_text(size=6))
  }

theme_lower_panel <- function(){
  theme_upper_panel() %+replace% 
    theme(axis.ticks.x = element_line(size=0.2),
          axis.text.x = element_text(size=6))
}


#########################
unique_DHBs <- unique(hospitalisations$DHB)
DHBSet <- unique_DHBs[1:5]

nz1 <- hospitalisations %>% filter(DHB %in% DHBSet)
grf1 <- ggplot(nz1, aes(x=Date,y=percent)) +
  geom_line(aes(colour=DHB,linetype=DHB)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=DHB, colour=DHB)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste(DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_upper_panel() + 
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf1
DHBSet <- unique_DHBs[6:10]
nz1 <- hospitalisations %>% filter(DHB %in% DHBSet)
grf2 <- ggplot(nz1, aes(x=Date,y=percent)) +
  geom_line(aes(colour=DHB,linetype=DHB)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=DHB, colour=DHB)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste(DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_upper_panel() + 
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf2

DHBSet <- unique_DHBs[11:15]
nz1 <- hospitalisations %>% filter(DHB %in% DHBSet)
grf3 <- ggplot(nz1, aes(x=Date,y=percent)) +
  geom_line(aes(colour=DHB,linetype=DHB)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)),
             aes(shape=DHB, colour=DHB)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste(DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_lower_panel() + 
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf3

DHBSet <- unique_DHBs[16:20]
nz1 <- hospitalisations %>% filter(DHB %in% DHBSet)
grf4 <- ggplot(nz1, aes(x=Date,y=percent)) +
  geom_line(aes(colour=DHB,linetype=DHB)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=DHB, colour=DHB)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste(DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_minimal(base_family="OpenSans") +
  theme_lower_panel() + 
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf4

graf <- grf1 + grf2 + grf3 + grf4  + plot_layout(ncol=2) +
  plot_annotation(
    title = "Rolling 7 day mean (weekends not available) regional covid hospitalisations",
    subtitle = "As percentage of Region population. May 11, 2022 to present",
    caption = "By @thoughtfulnz on twitter & mastodon.nz\nSource: NZ MoH github"
  )
graf

ggsave(filename="~/Desktop/NZcovidGraphs/Region_hospitalisations.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")

