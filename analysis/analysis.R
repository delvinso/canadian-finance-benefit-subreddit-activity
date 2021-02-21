library(tidyverse)
library(hrbrthemes)
library(lubridate)
theme_set(theme_ipsum_rc(base_size = 24) + theme(axis.title = element_text(size=16),
                                                 axis.text.x = element_text(angle = 45, hjust = 1),
                                                 plot.title = element_text(hjust = 0.5),
                                                 plot.subtitle = element_text(hjust = 0.5),
                                                 legend.position = 'top'))
setwd('~/Documents/Projects/pfc')

# ---- posts -----

infiles <- list.files(path =  './data/raw', pattern = '*posts.csv', full.names = TRUE)

posts <- map_dfr(infiles, function(x){ 
  data <- read_csv(x)
  data$subreddit = basename(x) %>% str_split('_') %>% .[[1]] %>% .[1]
  return(data)
  }) %>% 
  arrange(date_month_day)

count_by_day <- posts %>% count(subreddit, date_month_day) 

# announcement dates
# https://en.wikipedia.org/wiki/Federal_aid_during_the_COVID-19_pandemic_in_Canada
# https://www.canada.ca/en/department-finance/economic-response-plan.html
# https://www.canada.ca/en/employment-social-development/news/2020/08/government-of-canada-announces-plan-to-help-support-canadians-through-the-next-phase-of-the-recovery.html

count_by_day <- count_by_day %>% 
  mutate(annotation = case_when(
    date_month_day == "2020-04-06" & subreddit != "CESB" ~  "CERB Launched",
    date_month_day == "2020-06-15" & subreddit != "CESB"~ "CERB Extended for 2 months",
    date_month_day == "2020-08-20" & subreddit != "CESB" ~ "CERB Extended for 1 month",
    date_month_day == "2020-09-21" & subreddit != "CESB"~ "CERB Ended", 
    date_month_day == "2020-09-03" & subreddit != "CESB" ~ "CERB payment delays",
    date_month_day == "2020-05-15" & subreddit == "CESB" ~ "CESB Launched", # announced April 22
    date_month_day == "2020-06-07" & subreddit == "CESB" ~ "CESB Eligibility Period #2", 
    date_month_day == "2020-07-05" & subreddit == "CESB" ~ "CESB Eligibility Period #3", 
    date_month_day == "2020-08-02" & subreddit == "CESB" ~ "CESB Eligibility Period #4", 
    date_month_day == "2020-09-30" & subreddit == "CESB" ~ "CESB Ended",
    date_month_day == "2020-10-12" & subreddit != "CESB" ~ "CRB Launched", # 24 weeks after?
    date_month_day == "2020-09-27" & subreddit != "CESB" ~ "EI Eligibility Changes",
    date_month_day == "2020-12-01" & subreddit != "CESB" ~ "CRA asks for CERB repayments (approximate time)",
    TRUE ~ NA_character_
  )) 


count_by_day %>%
  filter(date_month_day >= "2020-01-01")  %>%
  group_by(subreddit) %>%
  ggplot(aes(x = date_month_day, y = n)) +
  geom_line(aes(colour = subreddit, group = subreddit),
            size = 1, alpha = 0.8)  +
 ggrepel::geom_label_repel(aes(label = annotation),
                           point.padding = 0.2,
                           nudge_x = .01,
                           nudge_y = 100,
                           size = 4,
                           arrow = arrow(length = unit(0.015, "npc"))) + 
  scale_color_brewer("Subreddit", palette = "Set1") +
  guides(color = FALSE) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") +
  facet_grid(subreddit ~ .) + 
  labs(y = "Daily Number of Posts", x = "Date") +
  labs(title = "Canadian Benefit and Finance Subreddit Activity (Submissions) during the Covid-19 Pandemic",
       subtitle = 'Data collected using PushShift and PRAW - 2020-01 to 2020-02',
       caption = 'github.com/delvinso')
  
ggsave(filename = file.path('figures', 'posts_annotated_to_02-01-2021.png'), height = 14, width = 16)



# ---- comments -----

infiles <- list.files(path =  './data/raw', pattern = '*comments*', full.names = TRUE)

comments <- map_dfr(infiles, function(x){
  data <- read_csv(x)
  data$subreddit = basename(x) %>% str_split('_') %>% .[[1]] %>% .[1]
  return(data)
}) %>%
  arrange(date_month_day)

comments <- comments %>%
  filter(!str_detect(body, '\\[deleted\\]|\\[removed\\]')) %>%
  arrange(date_month_day)



count_by_day <- comments %>% count(subreddit, date_month_day)

# announcement dates
# https://en.wikipedia.org/wiki/Federal_aid_during_the_COVID-19_pandemic_in_Canada
# https://www.canada.ca/en/department-finance/economic-response-plan.html
# https://www.canada.ca/en/employment-social-development/news/2020/08/government-of-canada-announces-plan-to-help-support-canadians-through-the-next-phase-of-the-recovery.html
count_by_day <- count_by_day %>% 
  mutate(annotation = case_when(
    date_month_day == "2020-04-06" & subreddit != "CESB" ~  "CERB Launched",
    date_month_day == "2020-06-15" & subreddit != "CESB"~ "CERB Extended for 2 months",
    date_month_day == "2020-08-20" & subreddit != "CESB" ~ "CERB Extended for 1 month",
    date_month_day == "2020-09-21" & subreddit != "CESB"~ "CERB Ended", 
    date_month_day == "2020-09-03" & subreddit != "CESB" ~ "CERB payment delays",
    date_month_day == "2020-05-15" & subreddit == "CESB" ~ "CESB Launched", # announced April 22
    date_month_day == "2020-06-07" & subreddit == "CESB" ~ "CESB Eligibility Period #2", 
    date_month_day == "2020-07-05" & subreddit == "CESB" ~ "CESB Eligibility Period #3", 
    date_month_day == "2020-08-02" & subreddit == "CESB" ~ "CESB Eligibility Period #4", 
    date_month_day == "2020-09-30" & subreddit == "CESB" ~ "CESB Ended",
    date_month_day == "2020-10-12" & subreddit != "CESB" ~ "CRB Launched", # 24 weeks after?
    date_month_day == "2020-09-27" & subreddit != "CESB" ~ "EI Eligibility Changes",
    date_month_day == "2020-12-01" & subreddit != "CESB" ~ "CRA asks for CERB repayments (approximate time)",
    TRUE ~ NA_character_
  )) 

count_by_day %>%
  filter(date_month_day >= "2020-01-01")  %>%
  group_by(subreddit) %>%
  ggplot(aes(x = date_month_day, y = n)) +
  geom_line(aes(#linetype = subreddit, 
    colour = subreddit, group = subreddit),
    size = 0.8, alpha = 0.8)  +
  ggrepel::geom_label_repel(aes(label = annotation),
                            point.padding = 0.2,
                            nudge_x = .01,
                            nudge_y = 1000,
                            # segment.linetype = 6,
                            # segment.curvature = -1e-20,
                            arrow = arrow(length = unit(0.015, "npc"))) + 
  scale_color_brewer(palette = "Set1") + 
  guides(color = FALSE) + 
  scale_x_date(breaks = "months") +
  facet_grid(subreddit ~ .) + 
  labs(y = "Daily Number of Posts", x = "Date") +
  labs(title = "Canadian Benefit and Finance Subreddit Activity (Comments) during the Covid-19 Pandemic",
       subtitle = 'Data collected using PushShift and PRAW - 2020-01 to 2020-02',
       caption = 'github.com/delvinso')

ggsave(filename = file.path('figures', 'comments_annotated_to_02-01-2021.png'), height = 14, width = 16)





                