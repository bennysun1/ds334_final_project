library(tidyverse)
library(shinyWidgets)
library(scales)
library(plotly)
library(shiny)
library(rvest)
library(maps)
library(tigris)
library(mapview)
library(sf)
library(usmap)
library(leaflet)
library(leafpop)

# file that cleans and scrapes data
source("data_cleaning.R")

## Compare probabilities of all states throught 2008 cycle
probabilities_08 <-
  intrade_combined %>%
  filter((year == 2008 & date <= "2008-11-03")) %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(democrat_avg_odds = round(mean(democrat, na.rm = T), digits = 2),
            republican_avg_odds = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  ggplot(aes(x = date,
             y = democrat_avg_odds,
             group = 1)) +
  geom_line(aes(x = date, y = democrat_avg_odds), colour = "blue") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = date, y = republican_avg_odds), colour = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-15")), 
                 colour = "black", 
                 text = "Lehman Brothers go Bankrupt"),linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal()

plotly::ggplotly(probabilities_08,  tooltip = c("x","y","text"))

intrade_combined_election_eve_08 <-
  intrade_combined %>%
  filter((year == 2008 & date <= "2008-11-03") #|
         #(year == 2012 & date == "2012-11-05")
  ) %>%
  #filter(!is.na(democrat) & !is.na(republican)) %>%
  group_by(state) %>%
  slice_max(date) %>%
  left_join(results_2008_df %>% select(state, winner), by = "state") %>%
  rename("actual_winner"="winner") %>%
  mutate(color = ifelse(actual_winner == pred_winner, "#00BA38", "#F8766D")) %>%
  ungroup()

intrade_combined_election_eve_12 <-
  intrade_combined %>%
  filter((year == 2012 & date <= "2012-11-05") #|
         #(year == 2012 & date == "2012-11-05")
  ) %>%
  #filter(!is.na(democrat) & !is.na(republican)) %>%
  group_by(state) %>%
  slice_max(date) %>%
  left_join(results_2012_df %>% select(state, winner), by = "state") %>%
  rename("actual_winner"="winner") %>%
  mutate(color = ifelse(actual_winner == pred_winner, "#00BA38", "#F8766D")) %>%
  ungroup()



polls_combined %>%
  filter((year == 2008 & date <= "2008-11-03")) %>%
  group_by(date) %>%
  summarise(democrat_avg = mean(democrat),
            republican_avg = mean(republican)) %>%
  ungroup() %>% 
  ggplot(aes(x = date,
             y = democrat_avg)) +
  geom_line(aes(x = date, y = democrat_avg), colour = "blue") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = date, y = republican_avg), colour = "red") +
  geom_vline(xintercept = as.numeric("2008-09-15")) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal()


polls_combined_election_eve_08 <-
  polls_combined %>%
  filter((year == 2008 & date <= "2008-11-03")) %>%
  group_by(state, date, year) %>%
  summarise(democrat_avg = mean(democrat),
            republican_avg = mean(republican)) %>%
  ungroup() %>%
  group_by(state) %>%
  slice_max(date) %>%
  mutate(pred_winner = case_when(
    democrat_avg > republican_avg ~ "democrat",
    republican_avg > democrat_avg ~ "republican",
    .default = "Tie"
  )) %>%
  left_join(results_2008_df %>% select(state, winner), by = "state") %>%
  rename("actual_winner"="winner") %>%
  mutate(color = ifelse(actual_winner == pred_winner, "#00BA38", "#F8766D"))

polls_combined_election_eve_12 <-
  polls_combined %>%
  filter((year == 2012 & date <= "2012-11-05")) %>%
  group_by(state, date, year) %>%
  summarise(democrat_avg = mean(democrat),
            republican_avg = mean(republican)) %>%
  ungroup() %>%
  group_by(state) %>%
  slice_max(date) %>%
  mutate(pred_winner = case_when(
    democrat_avg > republican_avg ~ "democrat",
    republican_avg > democrat_avg ~ "republican",
    .default = "Tie"
  )) %>%
  left_join(results_2012_df %>% select(state, winner), by = "state") %>%
  rename("actual_winner"="winner") %>%
  mutate(color = ifelse(actual_winner == pred_winner, "#00BA38", "#F8766D"))

polls_eve_combined_08_12 <-
  bind_rows(polls_combined_election_eve_08, polls_combined_election_eve_12)



probabilities_08 <-
  intrade_combined %>%
  filter((year == 2008 & date <= "2008-11-03")) %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(democrat_avg_odds = round(mean(democrat, na.rm = T), digits = 2),
            republican_avg_odds = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  ggplot(aes(x = date,
             y = democrat_avg_odds)) +
  geom_line(aes(x = date, y = democrat_avg_odds), colour = "blue") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = date, y = republican_avg_odds), colour = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-03")),
                 text = paste0("Obama Wins Iowa Democratic Caucus\n",
                               "Date: 2008-01-03")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-02-05")),
                 text = paste0("2008 Super Tuesday\n",
                               "Date: 2008-02-05")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-15")),
                 text = paste0("Lehman Brothers File for Bankruptcy\n",
                               "Date: 2008-09-15")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-06")),
                 text = paste0("Fannie Mae and Freddie Mac Seized\n",
                               "Date: 2008-09-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-29")),
                 text = paste0("McCain Announces Palin as VP\n",
                               "Date: 2008-08-29")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-27")),
                 text = paste0("Obama Wins Nomination\nAnnounces Biden as VP\n",
                               "Date: 2008-08-27")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
                 text = paste0("2012 Iowa Caucuses\n",
                               "Date: 2012-08-11")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-03-06")),
                 text = paste0("Super Tuesday 2012\n",
                               "Date: 2012-03-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-25")),
                 text = paste0("RNC Declares Romney as Republican Nominee\n",
                               "Date: 2012-04-25")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
                 text = paste0("Mitt Romney Announces Paul Ryan as VP\n",
                               "Date: 2012-08-11")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-10-29")),
                 text = paste0("Hurricane Sandy\n",
                               "Date: 2012-10-29")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Average Odds of Winning Presidency") +
  theme_minimal()

plotly::ggplotly(probabilities_08,  tooltip = c("x","y","text"))

probabilities_12 <-
  intrade_combined %>%
  filter((year == 2012 & date <= "2012-11-05")) %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(democrat_avg_odds = round(mean(democrat, na.rm = T), digits = 2),
            republican_avg_odds = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  ggplot(aes(x = date,
             y = democrat_avg_odds)) +
  geom_line(aes(x = date, y = democrat_avg_odds), colour = "blue") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = date, y = republican_avg_odds), colour = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-03")),
                 text = paste0("Obama Wins Iowa Democratic Caucus\n",
                               "Date: 2008-01-03")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-02-05")),
                 text = paste0("2008 Super Tuesday\n",
                               "Date: 2008-02-05")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-15")),
                 text = paste0("Lehman Brothers File for Bankruptcy\n",
                               "Date: 2008-09-15")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-06")),
                 text = paste0("Fannie Mae and Freddie Mac Seized\n",
                               "Date: 2008-09-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-29")),
                 text = paste0("McCain Announces Palin as VP\n",
                               "Date: 2008-08-29")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-27")),
                 text = paste0("Obama Wins Nomination\nAnnounces Biden as VP\n",
                               "Date: 2008-08-27")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
                 text = paste0("2012 Iowa Caucuses\n",
                               "Date: 2012-08-11")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-03-06")),
                 text = paste0("Super Tuesday 2012\n",
                               "Date: 2012-03-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-25")),
                 text = paste0("RNC Declares Romney as Republican Nominee\n",
                               "Date: 2012-04-25")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
                 text = paste0("Mitt Romney Announces Paul Ryan as VP\n",
                               "Date: 2012-08-11")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-10-29")),
                 text = paste0("Hurricane Sandy\n",
                               "Date: 2012-10-29")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Average Odds of Winning Presidency") +
  theme_minimal()

plotly::ggplotly(probabilities_12,  tooltip = c("x","y","text"))


election_results_combined <-
  bind_rows(results_2008_df %>%
              filter(state != "U.S. Total") %>%
              mutate(year = 2008), 
            results_2012_df %>%
              filter(state != "U.S. Total") %>%
              mutate(year = 2012)) %>%
  mutate(dem_ev = ifelse(winner == "democrat", as.numeric(ev), 0),
         rep_ev = ifelse(winner == "republican", as.numeric(ev), 0))

all_states_list <-
  inner_join(election_results_combined, 
             intrade_combined_election_eve_08, 
             polls_combined_election_eve_08, 
             by = "state") %>%
  distinct(state) %>%
  pull()

intrade_08_ev <-
  intrade_combined_election_eve_08 %>%
  filter(state %in% all_states_list) %>%
  left_join(ev_2008, by = "state") %>%
  mutate(na_ev = as.numeric(ifelse(pred_winner == "Tie", ev, 0)),
         dem_ev = as.numeric(ifelse(pred_winner == "democrat", ev, 0)),
         rep_ev = as.numeric(ifelse(pred_winner == "republican", ev, 0))) %>%
  group_by(year) %>%
  summarise(dem_ev_sum = sum(dem_ev),
            rep_ev_sum = sum(rep_ev),
            na_ev_sum = sum(na_ev)) %>%
  rename("Democrat" = "dem_ev_sum",
         "Republican" = "rep_ev_sum",
         "NA" = "na_ev_sum",
         "Year" = "year") %>%
  pivot_longer(cols = c("Democrat","Republican","NA"), 
               values_to = "electoral_votes", 
               names_to = "Party") %>%
  mutate(Year = as.factor(Year))

polls_o8_ev <-
  polls_combined_election_eve_08 %>%
  filter(state %in% all_states_list) %>%
  left_join(ev_2008, by = "state") %>%
  mutate(na_ev = as.numeric(ifelse(pred_winner == "Tie", ev, 0)),
         dem_ev = as.numeric(ifelse(pred_winner == "democrat", ev, 0)),
         rep_ev = as.numeric(ifelse(pred_winner == "republican", ev, 0))) %>%
  group_by(year) %>%
  summarise(dem_ev_sum = sum(dem_ev),
            rep_ev_sum = sum(rep_ev),
            na_ev_sum = sum(na_ev)) %>%
  rename("Democrat" = "dem_ev_sum",
         "Republican" = "rep_ev_sum",
         "NA" = "na_ev_sum",
         "Year" = "year") %>%
  pivot_longer(cols = c("Democrat","Republican","NA"), 
               values_to = "electoral_votes", 
               names_to = "Party") %>%
  mutate(Year = as.factor(Year))



summary_results_ev <-
  election_results_combined %>%
  filter(state %in% all_states_list) %>%
  group_by(year) %>%
  summarise(dem_ev_sum = sum(dem_ev),
            rep_ev_sum = sum(rep_ev)) %>%
  rename("Democrat" = "dem_ev_sum",
         "Republican" = "rep_ev_sum",
         "Year" = "year") %>%
  pivot_longer(cols = c("Democrat","Republican"), 
               values_to = "electoral_votes", 
               names_to = "Party") %>%
  mutate(Year = as.factor(Year))

middle_val <- 
  summary_results_ev %>%
  filter(Year == 2008) %>%
  summarise(middle_val = sum(electoral_votes)/2) %>%
  pull()


stacked_df <-
  bind_rows(summary_results_ev %>%
              mutate(Source = "Official Election Results"), 
            intrade_08_ev %>%
              mutate(Source = "Betting Predicted Results"),
            polls_o8_ev %>%
              mutate(Source = "Poll Predicted Results")) %>%
  mutate(Party = fct_relevel(Party, "Republican", "NA","Democrat"),
         source = as.factor(Source),
         Source = fct_relevel(Source, 
                              "Official Election Results", 
                              "Betting Predicted Results",
                              "Poll Predicted Results")) %>%
  filter(Year == 2008) %>%
  group_by(Source) %>%
  ungroup()

p1 <-
  ggplot(stacked_df,
         aes(x = Source, 
             y = electoral_votes, 
             fill = Party)) +
  geom_histogram(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("Republican" = "#E81B23",
                               "Democrat" = "#00AEF3",
                               "NA"="grey")) +
  geom_hline(yintercept = middle_val, colour = "black", linetype = "dashed", alpha = 0.8) +
  coord_flip() +
theme_minimal()


ggplotly(p1)






