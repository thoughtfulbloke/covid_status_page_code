library(readr)
library(dplyr)
library(RcppRoll)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(patchwork)
case_counts <- read_csv("../miscData/mohGithubCovidCaseCounts.csv", 
                        col_types= cols(
                          `Report Date` = col_date(format = ""),
                          `Number of cases reported` = col_number(),
                          .default = col_character()))

zeros <- expand.grid(Date = unique(case_counts$`Report Date`),
                     District = unique(case_counts$District),
                     Cases = 0) %>%
  filter(!(District %in% c("At the border", "Unknown")))

firsts <- case_counts %>%
  filter(`Case Status` == "Confirmed",
         !(District %in% c("At the border", "Unknown")),
         `Infection status` == "First") %>%
  group_by(Date = `Report Date`,District) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop") %>%
  bind_rows(zeros) %>%
  arrange(District, Date, desc(Cases)) %>%
  group_by(District, Date) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(District) %>%
  mutate(cumulative_firsts = cumsum(Cases),
         cum28 = lag(cumulative_firsts,28),
         cum90 = lag(cumulative_firsts,90),
         pool28to90 = cum28-cum90) %>%
  ungroup() %>%
  select(District, Date, pool28to90)

grafdata <- case_counts %>%
  filter(`Case Status` == "Confirmed",
         !(District %in% c("At the border", "Unknown")),
         `Infection status` == "Reinfection (< 90 days)") %>%
  group_by(Date = `Report Date`,District) %>%
  summarise(Cases = sum(`Number of cases reported`), .groups="drop") %>%
  bind_rows(zeros) %>%
  arrange(District, Date, desc(Cases)) %>%
  group_by(District, Date) %>%
  slice(1) %>%
  ungroup() %>%
  rename(Reinfections = Cases)  %>%
  inner_join(firsts, by = c("Date", "District")) %>%
  mutate(Rate = 10000 * Reinfections/pool28to90) %>%
  arrange(District, Date) %>%
  group_by(District) %>%
  mutate(sevenday = roll_meanr(Rate, 14)) %>%
  ungroup() %>% 
  filter(Date > (max(Date) - days(28)))

threshold95 = (sort(grafdata$sevenday))[498]
########## graphs
unique_DHBs <- unique(grafdata$District)
six_cols <- colorblind_pal()(6)

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
          legend.text = element_text(size=4.5),
          legend.key.height = unit(0.6, 'lines'),
          legend.title = element_text(size=6))
}

theme_lower_panel <- function(){
  theme_upper_panel() %+replace% 
    theme(axis.ticks.x = element_line(size=0.2),
          axis.text.x = element_text(size=6))
}



###
DHBSet <- unique_DHBs[1:5]

nz1 <- grafdata %>% filter(District %in% DHBSet)
grf1 <- ggplot(nz1, aes(x=Date,y=sevenday)) +
  geom_hline(yintercept = threshold95, colour="#BBBBBB", 
             size = 0.1,linetype = 1) +
  geom_line(aes(colour=District,linetype=District)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=District, colour=District)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste("  ",DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_upper_panel() + coord_cartesian(ylim=c(0, max(grafdata$sevenday))) +
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf1

DHBSet <- unique_DHBs[6:10]
nz1 <- grafdata %>% filter(District %in% DHBSet)
grf2 <- ggplot(nz1, aes(x=Date,y=sevenday)) +
  geom_hline(yintercept = threshold95, colour="#BBBBBB", 
             size = 0.1,linetype = 1) +
  geom_line(aes(colour=District,linetype=District)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=District, colour=District)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste("  ",DHBSet[1],"to", DHBSet[5]),
       y="", x="") +
  theme_upper_panel() +  coord_cartesian(ylim=c(0, max(grafdata$sevenday))) +
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf2

DHBSet <- unique_DHBs[11:14]
nz1 <- grafdata %>% filter(District %in% DHBSet)
grf3 <- ggplot(nz1, aes(x=Date,y=sevenday)) +
  geom_hline(yintercept = threshold95, colour="#BBBBBB", 
             size = 0.1,linetype = 1) +
  geom_line(aes(colour=District,linetype=District)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=District, colour=District)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste("  ",DHBSet[1],"to", DHBSet[length(DHBSet)]),
       y="", x="") +
  theme_lower_panel() +  coord_cartesian(ylim=c(0, max(grafdata$sevenday))) +
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf3

DHBSet <- unique_DHBs[15:18]
nz1 <- grafdata %>% filter(District %in% DHBSet)
grf4 <- ggplot(nz1, aes(x=Date,y=sevenday)) +
  geom_hline(yintercept = threshold95, colour="#BBBBBB", 
             size = 0.1,linetype = 1) +
  geom_line(aes(colour=District,linetype=District)) + 
  geom_point(data= nz1 %>% filter(wday(Date, label=TRUE) == "Fri" & 
                                    yday(Date)/ 2 == floor(yday(Date)/2)), 
             aes(shape=District, colour=District)) +
  scale_colour_manual(values=six_cols[c(1:4,6)]) + 
  labs(title=paste("  ",DHBSet[1],"to", DHBSet[length(DHBSet)]),
       y="", x="") +
  theme_lower_panel() +  coord_cartesian(ylim=c(0, max(grafdata$sevenday))) +
  guides(colour = guide_legend(byrow = TRUE),
         linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
grf4


####
graf <- grf1 + grf2 + grf3 + grf4  + plot_layout(ncol=2) +
  plot_annotation(
    title = "Rolling 14 day mean regional 28 to 90 day reinfection rate per 10000 prior cases",
    subtitle = "Grey horizontal line: threshold 99% of entries are below",
    caption = "By @thoughtfulnz on twitter & mastodon.nz\nSource: NZ MoH github"
  )

####
ggsave(filename="~/Desktop/NZcovidGraphs/RegionalReinfections.png", plot = graf,
       height=4.5, width = 8, dpi=300, units = "in", bg = "white")
  
  

