# 🇺🇸 Data Visualization Final Project 🇺🇸

This project aims to analyze whether betting markets provide a superior benchmark for candidate support than polls in 🦅 US presidential 🦅 elections. Many hypothesize betting markets should be a more accurate reflection of public sentiment because of the efficient market hypothesis, which argues market prices reflect all available information. The analysis is facilitated through a Shiny app that provides interactive visualizations and insights into election trends and predictions comparing data from betting markets with polling data.

## Data Sources

The analysis utilizes three primary data sources:

1. **Betting Markets Data**: Data from Intrade, which where people trade contracts regarding whether a Democrat 🫏 or Republican 🐘 presidential candidate will win a certain state for the 2008 and 2012 election cycles. The market price of a contract varies depending on its sales. This data provide insights into the predicted outcomes of the elections based on the betting behavior of participants with market prices ranging from 0 to 100.

2. **Polling Data**: Data from various polling organizations for the 2008 and 2012 Election cycle, which capture public opinion on candidates and issues leading up to the elections.

4. **Election Result Data**: Official 2008 and 2012 election result data scraped from Wikipedia that describes the vote counts/percentages and the electoral votes awarded to Democrat and Republican candidates. 

## Shiny App

The Shiny app included in this project offers the following functionality:

- **Time Series Plot Tab**: Users can visualize time series data of average Democrat and Republican support over a selected date range. They can also filter the data by election cycle, date, state, and data source (betting data or polling data). The plot includes relevant historical events that occurred during the selected election cycle to provide context for changes in support over time.

- **Classification Map Tab**: Users can view a map that classifies states based on their predicted winners. The map updates based on the selected election cycle and date, highlighting correct, incorrect, and tied predictions.

- **Electoral College Plot Tab**: This tab displays a stacked bar plots representing the distributions of electoral college votes over time. Users can compare predicted results from both betting markets and polls with actual election results.

**Helpful Links**:
<br />
[Link to Shiny App](https://bsunshine25.shinyapps.io/ds334_final_project/)
<br />
[Source Code for Shiny App](app.R)
<br />
[Written Report](final_project_report.html)
[Link to Blog Post 04](https://bennysun1.github.io/ds334blog/posts/blog_post_04/)