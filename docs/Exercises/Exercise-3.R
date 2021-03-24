## ---------------------------------------------------------------------------------------
# Exercise-3 (Public Health Data Visualization )
#-----------------------------------------------------------------------------------------
# Up till now we have worked in web-browser and our code was running through shiny run-time.

# Let us work on RStudio now

# We will visualize COVID-19 data

# Let us load required libraries

# For data manipulation
library(tidyverse)

# For working with date and time
library(lubridate)

# For calculating rolling mean or moving average
library(zoo)

#-----------------------------------------------------------------------------------------

# Let us import district wise data of COVID-19 cases

dist <-
  read_csv("https://api.covid19india.org/csv/latest/districts.csv")

# We need to clean some data, format Date as Date in YMD format with lubricate package,
##remove districts which are Unassigned or Unknown and calculate 'new_cases'
## calculate rolling mean for last 7 days
#### don't be scared by these steps you will eventually learn

dist_cleaned <- dist %>%
  mutate(Date = ymd(Date)) %>%
  filter(!District == "Unknown") %>%
  filter(!District == "Unassigned") %>%
  arrange(State, District, Date) %>%
  group_by(State, District) %>%
  mutate(
    new_cases = Confirmed - lag(Confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad = TRUE, align = "right")
  ) %>%
  filter(!is.na(new_cases),!is.na(ave_new_cases)) %>%
  filter(new_cases >= 0) %>%
  filter(ave_new_cases >= 0) %>%
  ungroup()

#----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Plot new_cases by Date for a district

dist_cleaned %>%
  filter(District == "Nagpur") %>%
  ggplot(aes(x = Date,
             y = ave_new_cases)) +
  geom_col()

#----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# This plot is fine but we need further customization
# Let us first assign a name "district_plot" to this plot. This is done by using "<-" sign 

district_plot <- dist_cleaned %>%
  filter(District == "___") %>%
  ggplot(aes(x = Date,
             y = new_cases)) +
  geom_col() 
#----------------------------------------------------------------------------------------
# Now if you run above code, you will not see anything in console
# To see this plot just type "district_plot" in the console
# Now let's add labels layer
district_plot<-district_plot +
  labs(
    title = "Trend of COVID-19 Cases for District ______",
    x = "----",
    y = "-----",
    caption = "Data source- https://covid19india.org"
  ) 
#----------------------------------------------------------------------------------------
# Let us change the theme to `theme_light`
district_plot<- district_plot +
  ___

#----------------------------------------------------------------------------------------
# X-axis shows names of months, we need use "scale_x_date" and define breaks
district_plot<- district_plot +
  scale_x_date(date_breaks = "2 week", date_labels = " %d %b %Y") 

#----------------------------------------------------------------------------------------
# X-axis labels need to be rotated. Code is already added, run it and call plot name in console 
district_plot<- district_plot+
  theme(axis.text.x = element_text(angle = 90))
#----------------------------------------------------------------------------------------
# We can add a layer of line  showing rolling mean or average (hint- geom_line(), map "ave_new_cases")
district_plot<- district_plot+
geom_line(aes(__=___))
#----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# Let us rewrite entire code at once
# We will give colour to our bars. Call "colours()" in console to see names of all colours
# Choose a colour of your choice and replace "royalblue" from geom_col

district_plot <- dist_cleaned %>%
  filter(District == "Nagpur") %>%
  ggplot(aes(x = Date,
             y = new_cases)) +
  geom_col(fill="royalblue", alpha=0.5) +
  geom_line(aes(y=ave_new_cases), colour="brown")+
  labs(
    title = "Trend of COVID-19 Cases for District Nagpur",
    x = NULL,
    y = NULL,
    caption = "Data source- https://covid19india.org"
  ) +
  theme_light()+
  scale_x_date(date_breaks = "2 week", date_labels = " %d %b %Y") +
  theme(axis.text.x = element_text(angle = 90))

district_plot
  
# Now we will save the plot

ggsave(filename = "District____COVID_Epidemic_Curve.png", plot = district_plot, width = 10, height = 6)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Plot new_cases by Date for all districts in given state
districts_within_state_plot<-dist_cleaned %>%
  filter(State == "____") %>%
  ggplot(aes(x = ___,
             y = ___)) +
  ____() +
  facet_wrap(facets = "District", scales = "free_y") +
  theme_light()+
  labs(
    title = "Trend of COVID-19 Cases for Districts in ______",
    x = NULL,
    y = NULL,
    caption = "Data source- https://covid19india.org"
  )
districts_within_state_plot

ggsave(filename = "District wise Epidemic_Curve.png", plot = districts_within_state_plot, width = 12, height = 10)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#
# Plot new_cases by Date for all States
states_plot<- dist_cleaned %>%
  ggplot(aes(x = ___,
             y = ___)) +
  ____(fill = "skyblue") +
  facet_wrap(facets = "State", scales = "free_y") +
  theme_light()
states_plot

ggsave(filename = "State wise Epidemic_Curve.png", plot = states_plot, width = 12, height = 10)

#----------------------------------------------------------------------------------------  
