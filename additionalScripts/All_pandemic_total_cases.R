### current prevalence
bad=colorRampPalette(c("#E66100", "white"))(3)
good=colorRampPalette(c("white", "#5D3A9B"))(3)
bad2good = c(bad,good[2:3])
six_cols <- (colorblind_pal()(6))
vcol <- viridis_pal(alpha = 1, begin = 0, end = .96, direction = 1, option = "B")(3)
############

dhbs_2021 <- read_csv("../LongTermData/DHB_population_June.csv") %>%
  filter(`Year as at 30 June` == 2021, Sex == "Total", `Age group` == "total") %>%
         mutate(DHB = case_when(`District health board` == "Capital & Coast" ~ "Capital & Coast/Hutt",
                         `District health board` == "Hutt" ~ "Capital & Coast/Hutt",
                         `District health board` == "Canterbury" ~ "Canterbury/West Coast",
                         `District health board` == "West Coast" ~ "Canterbury/West Coast",
                         `District health board` == "Tairāwhiti" ~ "Tairawhiti",
                         `District health board` == "Waitematā" ~ "Waitemata",
                         TRUE ~ `District health board`)) %>%
  group_by(DHB) %>%
  summarise(pop21=sum(Value), .groups="drop") %>%
  filter(DHB != "New Zealand") %>%
  group_by(DHB) %>%
  summarise(population = sum(pop21), .groups = "drop")

########################

# Cases by Age
case_counts <- read_csv("../miscData/mohGithubCovidCaseCounts.csv", 
                        col_types= cols(
  `Report Date` = col_date(format = ""),
  `Number of cases reported` = col_number(),
  .default = col_character())) 
cases_DHB_Infection <- case_counts %>%
  filter(District != "At the border",
         District != "Unknown") %>%
  mutate(Infections = ifelse(`Infection status` == "First","First","Reinfection")) %>%
  group_by(DHB = District, Infections) %>%
  summarise(n = sum(`Number of cases reported`), .groups="drop") %>%
  inner_join(dhbs_2021, by="DHB") %>%
  mutate(percent = 100 * n/population,
         percent = ifelse(Infections == "First", percent, percent* -1)) 

##################
graf <- ggplot(cases_DHB_Infection, aes(x=percent, y=DHB, colour=Infections, yend=DHB)) +
  geom_segment(xend=0, size=3) +
  scale_colour_colorblind() +
  theme_minimal(base_family="OpenSans",
                base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_line(size=0.2),
        axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_line(size=0.2),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FCFCFC"),
        plot.caption = element_text(margin=margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                                    size=9, hjust=1),
        plot.caption.position = "plot",
        legend.position = "top",
        
        legend.key.size = unit(1, 'cm')) +
  labs(title = "Total all-pandemic reported cases to date\nas percentage of region population.",
subtitle = paste("Current to", max(case_counts$`Report Date`)),
y="",
x= "Percent of region who have reported a postive covid test\n",
caption="By @thoughtfulnz on twitter & mastodon.nz\nSource data: cases NZ MoH, population: StatsNZ") 

ggsave(filename="~/Desktop/NZcovidGraphs/All_pandemic_total_cases.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")

