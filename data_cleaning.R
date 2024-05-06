## Load packages
library(tidyverse)

state_df <- ggplot2::map_data("state")


states_df <- 
  tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))

intrade08 <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/intrade08.csv") %>%
  select(2:7) %>%
  mutate(PriceD = ifelse(is.na(PriceD), 0, PriceD),
         PriceR = ifelse(is.na(PriceR), 0, PriceR)) %>%
  mutate(statename = as.factor(statename),
         democrat_candidate = "Barack Obama",
         republican_candidate = "John McCain",
         pred_winner = case_when(
           PriceD > PriceR ~ "democrat",
           PriceR > PriceD ~ "republican",
           .default = "Tie"
         )
  ) %>%
  rename(#"state" = "statename",
    "democrat" = "PriceD",
    "republican" = "PriceR") %>%
  filter(day < as.Date("2008-11-04"))

intrade12 <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/intrade12.csv") %>%
  select(2:7) %>%
  mutate(PriceD = ifelse(is.na(PriceD), 0, PriceD),
         PriceR = ifelse(is.na(PriceR), 0, PriceR)) %>%
  mutate(statename = as.factor(statename),
         democrat_candidate = "Barack Obama",
         republican_candidate = "Mitt Romney",
         pred_winner = case_when(
           PriceD > PriceR ~ "democrat",
           PriceR > PriceD ~ "republican",
           .default = "Tie"
         )
  )%>%
  rename(#"state" = "statename",
    "democrat" = "PriceD",
    "republican" = "PriceR") %>%
  filter(day < as.Date("2012-11-06"))

polls08 <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/polls08.csv") %>%
  rename("statename" = "state") %>%
  mutate(statename = as.factor(statename), 
         democrat_candidate = "Barack Obama",
         republican_candidate = "John McCain") %>%
  left_join(states_df, by = c("statename" = "abb")) %>%
  rename("democrat" = "Obama",
         "republican" = "McCain",
         "date" = "middate") %>%
  mutate(pred_winner = case_when(
    democrat > republican ~ "democrat",
    republican > democrat ~ "republican",
    .default = "Tie"
  )) %>%
  select(-statename) %>%
  select(state, everything())

polls12 <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/polls12.csv") %>%
  select(2:6) %>%
  rename("statename" = "state") %>%
  mutate(statename = as.factor(statename), 
         democrat_candidate = "Barack Obama",
         republican_candidate = "Mitt Romney") %>%
  left_join(states_df, by = c("statename" = "abb")) %>%
  rename("democrat" = "Obama",
         "republican" = "Romney",
         "date" = "middate") %>%
  mutate(pred_winner = case_when(
    democrat > republican ~ "democrat",
    republican > democrat ~ "republican",
    .default = "Tie"
  )) %>%
  select(-statename) %>%
  select(state, everything())


## '08 election
`2008_results_scrape` <- read_html("https://en.wikipedia.org/wiki/2008_United_States_presidential_election")

`2008_election_tables` <- `2008_results_scrape` %>% html_nodes("table.wikitable")

`2008_results_table` <- `2008_election_tables`[[10]]

`2008_results_df` <- 
  `2008_results_table` %>% 
  html_table(header = T) 

new_names <- c("Column1", 
               "Column2", 
               "Barack_Obama_Democratic", 
               "Barack_Obama_Democratic",
               "Barack_Obama_EV", 
               "John_McCain_Republican", 
               "John_McCain_Republican",
               "John_McCain_EV", 
               "Ralph_Nader_Independent", 
               "Ralph_Nader_Independent",
               "Ralph_Nader_Independent", 
               "Bob_Barr_Libertarian", 
               "Bob_Barr_Libertarian",
               "Bob_Barr_Libertarian", 
               "Chuck_Baldwin_Constitution", 
               "Chuck_Baldwin_Constitution",
               "Chuck_Baldwin_Constitution", 
               "Cynthia_McKinney_Green", 
               "Cynthia_McKinney_Green",
               "Cynthia_McKinney_Green", 
               "Others","Others","Others", 
               "Margin", "Margin",
               "Total_votes", "Total_votes")


# Rename the nameless columns
names(`2008_results_df`)[names(`2008_results_df`) == ""] <- new_names

results_2008_df <-
  `2008_results_df` %>%
  select(1,2,4,7,26) %>%
  rename("state" = "Column1",
         "ev" = "Column2",
         "democrat" = "Barack ObamaDemocratic",
         "republican" = "John McCainRepublican",
         "votes" = "Total votes") %>%
  slice(-1) %>%
  mutate(votes = as.numeric(str_replace_all(votes, ",", "")),
         democrat = as.numeric(str_replace_all(democrat, "%", "")),
         republican = as.numeric(str_replace_all(republican, "%", "")),
         state = as.factor(state)) %>%
  mutate(state = case_when(
    state == "Nebraska†" ~ "Nebraska",
    state == "Maine†" ~ "Maine",
    .default = state
  ),
  winner = case_when(
    democrat > republican ~ "democrat",
    democrat < republican ~ "republican",
    .default = "tie")
  )

ev_2008 <- results_2008_df %>% select(state, ev)



## '12 Election
`2012_results_scrape` <- read_html("https://en.wikipedia.org/wiki/2012_United_States_presidential_election")

`2012_election_tables` <- `2012_results_scrape` %>% html_nodes("table.wikitable")

`2012_results_table` <- `2012_election_tables`[[8]]

`2012_results_df` <- 
  `2012_results_table` %>% 
  html_table(header = T) 

names(`2012_results_df`)[4] <- "Obama_EV"
names(`2012_results_df`)[7] <- "Romney_EV"

`2012_results_df` <-
  `2012_results_df` %>%
  select(1,3,4,6,7,19) %>%
  mutate(ev = ifelse(Obama_EV == "–", Romney_EV, Obama_EV)) %>%
  select(1,2,4,6,7)


new_names <- c("state", "democrat", 
               "republican", "votes")

# Rename the nameless columns
names(`2012_results_df`)[names(`2012_results_df`) == ""] <- new_names

results_2012_df <-
  `2012_results_df` %>%
  rename("state" = "State/District",
         "democrat" = "Barack ObamaDemocratic",
         "republican" = "Mitt RomneyRepublican",
         "votes" = "Total") %>%
  slice(-1) %>%
  mutate(votes = as.numeric(str_replace_all(votes, ",", "")),
         democrat = as.numeric(str_replace_all(democrat, "%", "")),
         republican = as.numeric(str_replace_all(republican, "%", "")),
         state = as.factor(state)) %>%
  filter(state != "ME-1Tooltip Maine's 1st congressional district" &
           state != "ME-2Tooltip Maine's 2nd congressional district" &
           state != "NE-1Tooltip Nebraska's 1st congressional district") %>%
  mutate(state = case_when(
    state == "Nebraska†" ~ "Nebraska",
    state == "Maine†" ~ "Maine",
    state == "District of ColumbiaDistrict of Columbia" ~ "District of Columbia",
    state == "New Jersey[121]" ~ "New Jersey",
    state == "New York[122]" ~ "New York",
    state == "Ohio[123]" ~ "Ohio",
    state == "Wisconsin[124]" ~ "Wisconsin",
    .default = state
  ),
  winner = case_when(
    democrat > republican ~ "democrat",
    democrat < republican ~ "republican",
    .default = "tie")
  )

ev_2012 <- results_2012_df %>% select(state, ev)


intrade_combined <- bind_rows(
  mutate(intrade08, year = 2008),
  mutate(intrade12, year = 2012)
) %>%
  rename("date" = "day",
         "state" = "statename")

polls_combined <- bind_rows(
  mutate(polls08, year = 2008),
  mutate(polls12, year = 2012)
) %>%
  mutate(scale = 100,
         state = as.factor(state))