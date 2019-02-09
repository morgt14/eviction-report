# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
install.packages("lubridate")
library("lubridate")
install.packages("tidyr")
library("tidyr")
install.packages("ggmap")
library("ggmap")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)

# Compute some values of interest and store them in variables for the report


# How many evictions were there?
num_evictions <- nrow(evictions)
38732
num_features <- ncol(evictions)
29
# Create a table (data frame) of evictions by zip code (sort descending)
colnames(evictions)
by_zip <- evictions %>% 
  group_by(Eviction.Notice.Source.Zipcode) %>% 
  count() %>% 
  arrange(-n)

# Create a plot of the number of evictions each month in the dataset
as.Date("10/6/17", format = "%m/%d/%y")
by_month <- evictions %>% 
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>% 
  mutate(month = floor_date(date, unit="month")) %>% 
  group_by(month) %>% 
  count()

# Store plot in a variable
plot_by_month <- ggplot(data = by_month) + 
  geom_line(mapping = aes(x = month, y = n), color = "blue") + 
  labs(x = "Date", y = "Number of Evictions", title = "Evictions Over Time in SF")

# Map evictions in 2017
base_plot <- qmplot(
  data = evictions_2017,       
  x = long,
  y = lat,
  geom = "blank",
  maptype = "toner-background",
  darken = .7,
  legend = "topleft"
)

# Format the lat/long variables, filter to 2017
evictions_2017 <- evictions %>% 
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>% 
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>% 
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),
    long = as.numeric(gsub("\\)", "", long))
  )

# Create a maptile background
evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm"))

# Add a layer of points on top of the map tiles
