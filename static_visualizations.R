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

us_geo <- tigris::states(cb = TRUE, resolution = '20m')

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








## NC 2008 Time Series Betting
nc_08_betting <-
  probabilities <-
  intrade_combined %>%
  filter(year == 2008) %>%
  filter(date >= as.Date("2008-01-01")) %>%
  filter(state == "North Carolina") %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(Democrat = round(mean(democrat, na.rm = T), digits = 2),
            Republican = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  rename("Date" = "date") %>%
  ggplot(aes(x = Date,
             y = Democrat)
  ) +
  geom_line(aes(x = Date, 
                y = Democrat),
            colour = "#00AEF3") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = Date, 
                y = Republican),
            colour = "#E81B23") +
  
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-11-04")),
                 text = paste0("2008 Election Day\n",
                               "Date: 2008-11-04")), 
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-11-06")),
                 text = paste0("2012 Election Day\n",
                               "Date: 2012-11-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Closing Price of Party Nominee's Market",
       title = "2008 North Carolina Intrade Contract Closing Prices") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

nc_08_betting <-
  plotly::ggplotly(nc_08_betting,  
                   tooltip = c("x",
                               "y",
                               "text"))


nc_08_polls <-
  probabilities <-
  polls_combined %>%
  filter(year == 2008) %>%
  filter(state == "North Carolina") %>%
  filter(date >= as.Date("2008-01-01")) %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(Democrat = round(mean(democrat, na.rm = T), digits = 2),
            Republican = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  rename("Date" = "date") %>%
  ggplot(aes(x = Date,
             y = Democrat)
  ) +
  geom_line(aes(x = Date, 
                y = Democrat),
            colour = "#00AEF3") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = Date, 
                y = Republican),
            colour = "#E81B23") +
  
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-11-04")),
                 text = paste0("2008 Election Day\n",
                               "Date: 2008-11-04")), 
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-11-06")),
                 text = paste0("2012 Election Day\n",
                               "Date: 2012-11-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Closing Price of Party Nominee's Market",
       title = "2008 Missouri Polling Support") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

nc_08_polls <-
  plotly::ggplotly(nc_08_polls,  
                   tooltip = c("x",
                               "y",
                               "text"))










intrade_classification_data <-
  intrade_combined %>%
  filter((year == 2008 & date <= as.Date("2008-11-03"))) %>%
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
  mutate(color = as.factor(case_when(
    pred_winner == "Tie" ~ "Tie",
    actual_winner == pred_winner ~ "Correct", 
    .default = "Incorrect")
  )) %>%
  ungroup()

intrade_classification_data$color <- 
  fct_relevel(intrade_classification_data$color, "Correct", "Incorrect", "Tie")

intrade_classification_data <- 
  inner_join(us_geo, intrade_classification_data, by = c("NAME" = "state"))


intrade_map_08 <-
mapview(intrade_classification_data, zcol = "color", color = "white", 
        col.regions = c("seagreen3", "red"),
        label = glue::glue("{intrade_classification_data$NAME}"),
        popup = popupTable(intrade_classification_data, 
                           zcol = c("NAME",
                                    "date", 
                                    "democrat_avg", 
                                    "republican_avg", 
                                    "pred_winner", 
                                    "actual_winner",
                                    "color"),
                           feature.id = F, 
                           row.numbers = F)
)



intrade_classification_data_12 <-
  intrade_combined %>%
  filter((year == 2012 & date <= as.Date("2012-11-05"))) %>%
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
  mutate(color = as.factor(case_when(
    pred_winner == "Tie" ~ "Tie",
    actual_winner == pred_winner ~ "Correct", 
    .default = "Incorrect")
  )) %>%
  ungroup()

intrade_classification_data_12$color <- 
  fct_relevel(intrade_classification_data_12$color, "Correct", "Incorrect", "Tie")

intrade_classification_data_12 <- 
  inner_join(us_geo, intrade_classification_data_12, by = c("NAME" = "state"))


intrade_map_12 <-
  mapview(intrade_classification_data_12, zcol = "color", color = "white", 
          col.regions = c("seagreen3", "red"),
          label = glue::glue("{intrade_classification_data_12$NAME}"),
          popup = popupTable(intrade_classification_data_12, 
                             zcol = c("NAME",
                                      "date", 
                                      "democrat_avg", 
                                      "republican_avg", 
                                      "pred_winner", 
                                      "actual_winner",
                                      "color"),
                             feature.id = F, 
                             row.numbers = F)
  )






pollster_classification_data_12 <-
  intrade_combined %>%
  filter((year == 2012 & date <= as.Date("2012-11-05"))) %>%
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
  mutate(color = as.factor(case_when(
    pred_winner == "Tie" ~ "Tie",
    actual_winner == pred_winner ~ "Correct", 
    .default = "Incorrect")
  )) %>%
  ungroup()

pollster_classification_data_12$color <- 
  fct_relevel(pollster_classification_data_12$color, "Correct", "Incorrect", "Tie")

pollster_classification_data_12 <- 
  inner_join(us_geo, pollster_classification_data_12, by = c("NAME" = "state"))


intrade_map_12 <-
  mapview(pollster_classification_data_12, zcol = "color", color = "white", 
          col.regions = c("seagreen3", "red"),
          label = glue::glue("{pollster_classification_data_12$NAME}"),
          popup = popupTable(pollster_classification_data_12, 
                             zcol = c("NAME",
                                      "date", 
                                      "democrat_avg", 
                                      "republican_avg", 
                                      "pred_winner", 
                                      "actual_winner",
                                      "color"),
                             feature.id = F, 
                             row.numbers = F)
  )







poll_classification_data_08 <-
  polls_combined %>%
  filter((year == 2008 & date <= as.Date("2008-11-03"))) %>%
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
  mutate(color = as.factor(case_when(
    pred_winner == "Tie" ~ "Tie",
    actual_winner == pred_winner ~ "Correct", 
    .default = "Incorrect")
  )) %>%
  ungroup()

poll_classification_data_08$color <- 
  fct_relevel(poll_classification_data_08$color, "Correct", "Incorrect", "Tie")

poll_classification_data_08 <- 
  inner_join(us_geo, poll_classification_data_08, by = c("NAME" = "state"))


polls_map_08 <-
  mapview(poll_classification_data_08, zcol = "color", color = "white", 
          col.regions = c("seagreen3", "red"),
          label = glue::glue("{poll_classification_data_08$NAME}"),
          popup = popupTable(poll_classification_data_08, 
                             zcol = c("NAME",
                                      "date", 
                                      "democrat_avg", 
                                      "republican_avg", 
                                      "pred_winner", 
                                      "actual_winner",
                                      "color"),
                             feature.id = F, 
                             row.numbers = F)
  )







## NC 2008 Time Series Betting
nc_12_betting <-
  probabilities <-
  intrade_combined %>%
  filter(year == 2012) %>%
  filter(date >= as.Date("2012-01-01")) %>%
  filter(state == "North Carolina") %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(Democrat = round(mean(democrat, na.rm = T), digits = 2),
            Republican = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  rename("Date" = "date") %>%
  ggplot(aes(x = Date,
             y = Democrat)
  ) +
  geom_line(aes(x = Date, 
                y = Democrat),
            colour = "#00AEF3") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(x = Date, 
                y = Republican),
            colour = "#E81B23") +
  
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-11-04")),
                 text = paste0("2008 Election Day\n",
                               "Date: 2008-11-04")), 
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-11-06")),
                 text = paste0("2012 Election Day\n",
                               "Date: 2012-11-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Closing Price of Party Nominee's Market",
       title = "2012 North Carolina Intrade Contract Closing Prices") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

nc_12_betting <-
  plotly::ggplotly(nc_12_betting,  
                   tooltip = c("x",
                               "y",
                               "text"))


# nc_12_polls <-
#   probabilities <-
#   polls_combined %>%
#   filter(year == 2012) %>%
#   filter(state == "North Carolina") %>%
#   filter(date >= as.Date("2012-01-01")) %>%
#   filter(democrat + republican > 30) %>%
#   group_by(date) %>%
#   summarise(Democrat = round(mean(democrat, na.rm = T), digits = 2),
#             Republican = round(mean(republican, na.rm = T), digits = 2)) %>%
#   ungroup() %>%
#   rename("Date" = "date") %>%
#   ggplot(aes(x = Date,
#              y = Democrat)
#   ) +
#   geom_line(aes(x = Date, 
#                 y = Democrat),
#             colour = "#00AEF3") +
#   geom_hline(yintercept = 50, colour = "black") +
#   geom_line(aes(x = Date, 
#                 y = Republican),
#             colour = "#E81B23") +
#   
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-03")),
#                  text = paste0("Obama Wins Iowa Democratic Caucus\n",
#                                "Date: 2008-01-03")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-02-05")),
#                  text = paste0("2008 Super Tuesday\n",
#                                "Date: 2008-02-05")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-15")),
#                  text = paste0("Lehman Brothers File for Bankruptcy\n",
#                                "Date: 2008-09-15")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-09-06")),
#                  text = paste0("Fannie Mae and Freddie Mac Seized\n",
#                                "Date: 2008-09-06")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-29")),
#                  text = paste0("McCain Announces Palin as VP\n",
#                                "Date: 2008-08-29")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-08-27")),
#                  text = paste0("Obama Wins Nomination\nAnnounces Biden as VP\n",
#                                "Date: 2008-08-27")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2008-11-04")),
#                  text = paste0("2008 Election Day\n",
#                                "Date: 2008-11-04")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
#                  text = paste0("2012 Iowa Caucuses\n",
#                                "Date: 2012-08-11")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-03-06")),
#                  text = paste0("Super Tuesday 2012\n",
#                                "Date: 2012-03-06")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-25")),
#                  text = paste0("RNC Declares Romney as Republican Nominee\n",
#                                "Date: 2012-04-25")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-11")),
#                  text = paste0("Mitt Romney Announces Paul Ryan as VP\n",
#                                "Date: 2012-08-11")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-10-29")),
#                  text = paste0("Hurricane Sandy\n",
#                                "Date: 2012-10-29")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   geom_vline(aes(xintercept = as.numeric(as.Date("2012-11-06")),
#                  text = paste0("2012 Election Day\n",
#                                "Date: 2012-11-06")), 
#              colour = "black", linetype = "dashed", alpha = 0.8) +
#   scale_y_continuous(limits = c(0,100)) +
#   labs(x = "Date",
#        y = "Closing Price of Party Nominee's Market",
#        title = "2012 North Carolina Polling Support") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))

nc_12_polls <- polls_combined %>%
  filter(year == 2012) %>%
  filter(state == "North Carolina") %>%
  filter(date >= as.Date("2012-01-01")) %>%
  filter(democrat + republican > 30) %>%
  group_by(date) %>%
  summarise(Democrat = round(mean(democrat, na.rm = T), digits = 2),
            Republican = round(mean(republican, na.rm = T), digits = 2)) %>%
  ungroup() %>%
  rename("Date" = "date")

# Create the plot
nc_12_polls_plot <- ggplot2::ggplot(nc_12_polls, aes(x = Date)) +
  geom_line(aes(y = Democrat), colour = "#00AEF3") +
  geom_hline(yintercept = 50, colour = "black") +
  geom_line(aes(y = Republican), colour = "#E81B23") +
  # Add vertical lines for events
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-01-03")),
                 text = paste0("Obama Wins Iowa Democratic Caucus\n",
                               "Date: 2008-01-03")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2008-11-04")),
                 text = paste0("2008 Election Day\n",
                               "Date: 2008-11-04")), 
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
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-11-06")),
                 text = paste0("2012 Election Day\n",
                               "Date: 2012-11-06")), 
             colour = "black", linetype = "dashed", alpha = 0.8) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Date",
       y = "Closing Price of Party Nominee's Market",
       title = "2012 North Carolina Polling Support") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

nc_12_polls <-
  plotly::ggplotly(nc_12_polls_plot,  
                   tooltip = c("x",
                               "y",
                               "text"))






