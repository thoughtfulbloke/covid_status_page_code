### current prevalence and risk for out and about
# as this is for encountering people when out and about with covid
# 1 am assuming people reporting covid create on average 1 day of infectious
# risk (pre-symptomatic) while non-reporting people create 4 days of risk
# outside of their own household (weekends and being too sick to want to go out)
# while asymptomatic people have a longer period of lower (less infectious) risk
# which all boils down to "risk days per mean case"
reporter_risk_equivalent = 1
nonreporter_risk_equivalent = 4
assymptomatic_risk_equivalent = 3

################# 
# Current Underreporting esitmate from wastewater, which includes assympotmatic
overall_underreporting = 1/.35

# making the colourblind friendly fill colours for diverging scale, also
# to be patterned to be functional for pure grey
bad=colorRampPalette(c("#E66100", "white"))(3)
good=colorRampPalette(c("white", "#5D3A9B"))(3)
bad2good = c(bad,good[2:3])
six_cols <- (colorblind_pal()(6))
vcol <- viridis_pal(alpha = 1, begin = 0, end = .96, direction = 1, option = "B")(3)
############
# want geographic order later
factor_geog <- rev(c("Northland",
                     "Waitemata",
                     "Auckland",
                     "Counties Manukau",
                     "Waikato",
                     "Bay of Plenty",
                     "Lakes",
                     "Tairawhiti",
                     "Hawke's Bay",
                     "Taranaki",
                     "MidCentral",
                     "Whanganui",
                     "Capital & Coast/Hutt",
                     "Wairarapa",
                     "Nelson Marlborough",
                     "Canterbury/West Coast",
                     "South Canterbury",
                     "Southern"))
# NZdotStat DHB 
# Subnational population estimates (DHB, DHB constituency), by age and sex, 
# at 30 June 1996-2022 (2015 boundaries)  
dhbs_2022 <- read_csv("../LongTermData/nzdot_subnational_dhb_june.csv") %>%
  filter(`Year at 30 June` == 2022, Sex == "Total people, sex",
         Age != "Total people, age", 
         Area != "Total NZ by DHB/DHB constituency") %>%
  mutate(Age = case_when(Age == "0-4 Years" ~ "0 to 9",
                         Age == "5-9 Years" ~ "0 to 9",
                         Age == "10-14 Years" ~ "10 to 19",
                         Age == "15-19 Years" ~ "10 to 19",
                         Age == "20-24 Years" ~ "20 to 29",
                         Age == "25-29 Years" ~ "20 to 29",
                         Age == "30-34 Years" ~ "30 to 39",
                         Age == "35-39 Years" ~ "30 to 39",
                         Age == "40-44 Years" ~ "40 to 49",
                         Age == "45-49 Years" ~ "40 to 49",
                         Age == "50-54 Years" ~ "50 to 59",
                         Age == "55-59 Years" ~ "50 to 59",
                         Age == "60-64 Years" ~ "60 to 69",
                         Age == "65-69 Years" ~ "60 to 69",
                         Age == "70-74 Years" ~ "70+",
                         Age == "75-79 Years" ~ "70+",
                         Age == "80-84 Years" ~ "70+",
                         Age == "85-89 Years" ~ "70+",
                         Age == "90 Years and over" ~ "70+"),
         DHB = case_when(Area == "Capital and Coast" ~ "Capital & Coast/Hutt",
                         Area == "Hutt Valley" ~ "Capital & Coast/Hutt",
                         Area == "Canterbury" ~ "Canterbury/West Coast",
                         Area == "West Coast" ~ "Canterbury/West Coast",
                         TRUE ~ Area)) %>%
  group_by(DHB, Age) %>%
  summarise(population = sum(Value), .groups = "drop")
########################
# Assume that the long term total regional covid reporting percentages
# reflect underreporting rather than natural resistance
# so have an underreporting multiple based on % difference to mean region

ratio_txt <- "
\"DHB\",\"multimod\"
\"Auckland\",0.996089908031902
\"Bay of Plenty\",1.10585522879211
\"Canterbury/West Coast\",0.835055853588575
\"Capital & Coast/Hutt\",0.885685746033283
\"Counties Manukau\",0.996188778931119
\"Hawke's Bay\",1.00487005804187
\"Lakes\",1.09397297458829
\"MidCentral\",1.01009744219634
\"Nelson Marlborough\",1.04623298508078
\"Northland\",1.19662692225456
\"South Canterbury\",0.892254443992734
\"Southern\",0.908483191965941
\"Tairawhiti\",0.975802980695461
\"Taranaki\",0.932959327136075
\"Waikato\",1.06779436995256
\"Wairarapa\",0.983565983062605
\"Waitemata\",1.04425514928717
\"Whanganui\",1.02420865636864
"
ratios <- read.csv(text=ratio_txt, stringsAsFactors = FALSE)

############################################
## estimate some age underreporting levels based off Wellkiwis study
## to make a multiple based off % difference to mean underreporting 
## but I have concerns elderly are less careful now, so looking for other clues
age_ratio_txt <- "
\"Age\",\"agemod\"
\"0 to 9\", 1.671827
\"10 to 19\", 1.35387
\"20 to 29\", 1.437461
\"30 to 39\", 0.7306502
\"40 to 49\", 0.8359133
\"50 to 59\", 0.619195
\"60 to 69\", 0.7430341
\"70+\", 0.7739938
"
age_ratios <- read.csv(text=age_ratio_txt, stringsAsFactors = FALSE)

########################

# Cases
case_counts <- read_csv("../miscData/mohGithubCovidCaseCounts.csv", 
                        col_types= cols(
  `Report Date` = col_date(format = ""),
  `Number of cases reported` = col_number(),
  .default = col_character())) 
NZ_cases <- case_counts %>%
  filter(District != "At the border",
         District != "Unknown") %>%
  mutate(Variable = ifelse(`Age group` %in% c("70 to 79","80 to 89","90+"),
                           "70+", `Age group`)) %>%
  group_by(DHB = District, Age = Variable, Date=`Report Date`) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop")

### padding with zeros to make non-sparse for rolling functions
NZ_rolled <- expand_grid(DHB = unique(NZ_cases$DHB),
                         Age = unique(NZ_cases$Age),
                         Date = unique(NZ_cases$Date)) %>%
  mutate(Cases = 0) %>%
  bind_rows(NZ_cases) %>%
  arrange(DHB,Age,Date,desc(Cases)) %>%
  group_by(DHB,Age,Date) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(DHB,Age) %>%
  # USing smoothed values as strong daily reporting differences
  mutate(sevenday = roll_meanr(Cases, 7),
         Percent_change = round(100*sevenday/(lag(sevenday,7)+0.00001)-100,2),
         doubling = 70/Percent_change,
         doubling_time = case_when(doubling > 0 & doubling < 2 ~ "doubling in under 2 weeks",
                                   doubling >= 2 & doubling <= 6 ~ "doubling in 2 to 6 weeks",
                                   doubling < 0 & doubling > -2 ~ "halving in under 2 weeks",
                                   doubling <= -2 & doubling >= -6 ~ "halving in 2 to 6 weeks",
                                   TRUE ~ "stable"),
         tcolour = case_when(doubling > 0 & doubling < 2 ~ "white",
                                   doubling >= 2 & doubling <= 6 ~ "black",
                                   doubling < 0 & doubling > -2 ~ "white",
                                   doubling <= -2 & doubling >= -6 ~ "black",
                                   TRUE ~ "black"),
         doubling_time = factor(doubling_time, levels = c("doubling in under 2 weeks",
                                                          "doubling in 2 to 6 weeks",
                                                          "stable",
                                                          "halving in 2 to 6 weeks",
                                                          "halving in under 2 weeks"))) %>%
  ungroup() %>%
  filter(Date > as.Date("2022-08-10")) %>%
  inner_join(dhbs_2022, by = c("DHB", "Age")) %>%
  arrange(DHB,Age,desc(Date)) %>%
  group_by(DHB,Age) %>%
  slice(1) %>% # get the last day of trends
  ungroup() %>% 
  inner_join(ratios, by="DHB") %>%
  inner_join(age_ratios, by="Age") %>%
  mutate(grand_tot_cases = overall_underreporting * sevenday * (multimod + agemod)/2,
         assymptomatics = grand_tot_cases * 0.15,
         unreporters = grand_tot_cases - sevenday - assymptomatics,
         risk_equivalent = sevenday * reporter_risk_equivalent + 
           assymptomatics * assymptomatic_risk_equivalent +
           unreporters * nonreporter_risk_equivalent,
         Risk = 100 * risk_equivalent/population) %>%
  mutate(DHB = factor(DHB, levels=factor_geog))
rolled_max <- max(NZ_rolled$Percent_change)

graf <- ggplot(NZ_rolled, aes(x=DHB,y=Age, fill=doubling_time,
                              label=round(Risk,2))) +
  theme_minimal(base_family="OpenSans",
                base_size = 8) +
  theme(panel.grid = element_blank(),
        axis.line.x = element_blank(),
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
  labs(title = "Risk Recentcast: Estimated number of infectious people per 100 people met.
Fill colour is doubling time based on rate of change in the last week",
subtitle = paste("Current to", NZ_rolled$Date, ". Current mean underreporting multiple estimate", round(overall_underreporting,1),"
For events 100*(1-(1-x/100)^y) is the % chance of at least one infectious attendee, where x is the number below and y is the attendees count.
Any easy way to do this calculation is put the formula into google with the x number from below and your attendee count for y\n"),
y="Age\n",
x= "",
caption="By @thoughtfulnz on twitter & mastodon.nz\nSource data: cases NZ MoH, population: StatsNZ") +
  geom_tile_pattern(aes(fill = doubling_time,
                        pattern_alpha = doubling_time),
                    colour="black",size=0.2, 
                    pattern = 'plasma',
                    pattern_fill = "grey0") + 
  geom_text(aes(colour=tcolour), size=3, fontface=2) +
  scale_fill_manual(values=bad2good,
                    name="Doubling Time", drop=FALSE) +
  scale_pattern_alpha_manual(values = c(.5,.5,0,0,0),
                             name="Doubling Time", drop=FALSE) +
  scale_colour_identity() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
graf

ggsave(filename="~/Desktop/NZcovidGraphs/Prevalence_age_region.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")


