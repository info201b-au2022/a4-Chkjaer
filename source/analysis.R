library(tidyverse)


# The functions might be useful for A4
source("../source/a4-helpers.R")


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

#Loads data including an important state abbreviations data set
incarceration_trends <- read.csv("C:/Users/Christian/Documents/info201/assignments/a4-chkjaer/data/incarceration-trends-master/incarceration_trends.csv")
state_abbs <- read.csv("C:/Users/Christian/Documents/info201//assignments/a4-chkjaer/data/incarceration-trends-master/abbr-name.csv", header = FALSE)
state_abbs <- state_abbs %>% rename(state_abb = V1, state = V2)
state_abbs$state = tolower(state_abbs$state)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#adds new columns based on the percent of the selected population that is black.
#Also adds a location column which combines state and county into one value
incarceration_trends_1 <- incarceration_trends %>%
  mutate("percent_prison_adm_black" = black_prison_adm / total_prison_adm * 100) %>%
  mutate("percent_prison_pop_black" = black_prison_pop / total_prison_pop * 100) %>%
  mutate("percent_jail_pop_black" =  black_jail_pop / total_jail_pop * 100) %>%
  mutate("Location" = paste(county_name, ", ", state, sep = ""))
  
#Where were the 10 highest black prison populations each year?
highest_black_prison_populations <- incarceration_trends_1 %>% 
  group_by(year) %>%
  arrange(year) %>%
  select('Location', percent_prison_pop_black, region) %>%
  top_n(10, percent_prison_pop_black)
 
#What year was percent of prison admissions of Black individuals across all counties the highest?
year_highest_black_prison_admissions <- incarceration_trends_1 %>%
  group_by(year) %>%
  summarise(
    avg_percent_black_prison_admissions = mean(percent_prison_adm_black, na.rm = TRUE)
  ) %>% top_n(1, avg_percent_black_prison_admissions) 

# What location imprisoned the highest percent of black people in 2016
location_highest_percent_prison_pop_black <- incarceration_trends_1 %>%
  filter(year == 2016) %>% 
  filter(percent_prison_pop_black == max(percent_prison_pop_black, na.rm = TRUE)) # %>%
  #pull(Location)


## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function wrangles data to find total jail populations for each year
get_year_jail_pop <- function() {
  incarceration_trends_year_jail_pop <- incarceration_trends %>% group_by(year) %>%
  summarise(
    yearly_jail_pop = sum(total_jail_pop, na.rm = TRUE)
  )
return(incarceration_trends_year_jail_pop)   
}

# This function returns a chart of the total jail populations each year, by calling
# the get_year_jail_pop function 
plot_jail_pop_for_us <- function() {
  data <- get_year_jail_pop()
  chart <- ggplot(data = data) +
    geom_col(mapping = aes(x = year, y = yearly_jail_pop)) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "The Jailing system has seen unprecedented and horrifying growth"
    )
  return(chart)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# This function wrangles data to find total jail populations for each state
get_jail_pop_by_states <- function(states) {
  state_jail_pop_trends <- incarceration_trends %>% 
    filter(state %in% states) %>% group_by(year, state) %>%
    summarise(
      state_jail_pop = sum(total_jail_pop, na.rm = TRUE)
    )
  return(state_jail_pop_trends)   
}

# This function returns a chart of the total jail populations for each state, by calling
# the get_jail_pop_bystates function
plot_jail_pop_by_states <- function(states) {
  data <- get_jail_pop_by_states(states)
  chart <- ggplot(data = data) +
    geom_smooth(mapping = aes(x = year, y = state_jail_pop, color = state)) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      captions = "Larger states, regardless of political demographic, contribute to this increase"
    )
  return(chart)   
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#Black vs White prison population growth from 1985
#This function wrangles data to find the black and white prison populations per year
get_black_white_year_prison_pop <- function() {
  incarceration_trends_year_prison_pop <- incarceration_trends %>% group_by(year) %>%
    filter(year <= 2016 & year >= 1985) %>%
    summarise(
      Black = sum(black_prison_pop, na.rm = TRUE),
      White = sum(white_prison_pop, na.rm = TRUE),
    ) %>% pivot_longer(cols = c('Black','White'),
                       names_to = "Race",
                       values_to = "Prison_Population")
  return(incarceration_trends_year_prison_pop)   
}

#This function returns a chart of the black and white prison populations per year
#by calling the get_black_white_year_prison_pop
plot_black_white_year_prison_pop <- function() {
  data <- get_black_white_year_prison_pop()
  chart <- ggplot(data) +
    geom_smooth(aes(x = year, y = Prison_Population, color = Race)) +
    labs(
      title = "Increase of Prison Population in U.S. (1985-2016)",
      x = "Year",
      y = "Total Prison Population",
      caption = "2016 was the first time in 30 years the Black prison population has been lower than the White prison population"
    )
  return(chart)   
} 

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
states_incarceration <- incarceration_trends_1 %>% rename(state_abb = state) %>%
  group_by(state_abb) %>%
  summarize(
    state_black_incarceration = mean(percent_prison_pop_black, na.rm = TRUE)
  ) %>% left_join(state_abbs, by = "state_abb")

state_shape <- map_data("state") %>% rename(state = region) %>% left_join(states_incarceration, by ="state")

U.S. <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = state_black_incarceration),
    color = "Black",
    size = .1,
  ) + 
  coord_map() +
  scale_fill_continuous(low = "Yellow", high = "Red") +
  labs(fill = "Average Percent of Prison Population that is Black",
       caption = "The South severely disproportionately imprisons Blacks") +
  theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

## Load data frame ---- 


