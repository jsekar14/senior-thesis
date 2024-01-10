# Load the necessary libraries
library(dplyr)
library(lubridate)
library(viridis)

rm(list = ls())

# Read the CSV files into a data frame
# aq_2023_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2023.csv") %>%
#   mutate(`Date` = as.Date(`Date`, format = "%m/%d/%Y")) %>%
#   filter(format(Date, "%m") <= "03")  %>%
#   group_by(Date, Site.ID) %>%
#   summarize(Daily.Mean.PM2.5.Concentration = max(Daily.Mean.PM2.5.Concentration, na.rm = TRUE),
#             SITE_LATITUDE = first(SITE_LATITUDE),
#             SITE_LONGITUDE = first(SITE_LONGITUDE))
aq_2023_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2023.csv")
aq_2022_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2022.csv")
aq_2021_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2021.csv")
aq_2020_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2020.csv")
aq_2019_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2019.csv")
aq_2018_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2018.csv")
aq_2017_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2017.csv")
aq_2016_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2016.csv")
aq_2015_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2015.csv")
aq_2014_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2014.csv")
aq_2013_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2013.csv")
aq_2012_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2012.csv")
aq_2011_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2011.csv")
aq_2010_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2010.csv")
aq_2009_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2009.csv")
aq_2008_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2008.csv")
aq_2007_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2007.csv") 
aq_2006_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2006.csv") 
aq_2005_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2005.csv")
aq_2004_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2004.csv")
aq_2003_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2003.csv") 
aq_2002_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2002.csv")
# aq_2003_data <- read.csv("/Users/janani/Documents/thesis/epa_data/epa_2003.csv") %>%
#   mutate(`Date` = as.Date(`Date`, format = "%m/%d/%Y")) %>%
#   filter(format(Date, "%m") >= "02")  %>%
#   group_by(Date, Site.ID) %>%
#   summarize(Daily.Mean.PM2.5.Concentration = max(Daily.Mean.PM2.5.Concentration, na.rm = TRUE),
#             SITE_LATITUDE = first(SITE_LATITUDE),
#             SITE_LONGITUDE = first(SITE_LONGITUDE))

# combine CSVs for each year 
test <- rbind(aq_2002_data, aq_2003_data)
all_aq_data = rbind(aq_2023_data, aq_2022_data, aq_2021_data, aq_2020_data, aq_2019_data,
                    aq_2018_data, aq_2017_data, aq_2016_data, aq_2015_data, aq_2014_data,
                    aq_2013_data, aq_2012_data, aq_2011_data, aq_2010_data, aq_2009_data,
                    aq_2008_data, aq_2007_data, aq_2006_data, aq_2005_data, aq_2004_data,
                    aq_2003_data, aq_2002_data) %>%
        mutate(Daily.Mean.PM2.5.Concentration = if_else(Daily.Mean.PM2.5.Concentration < 0, 0, Daily.Mean.PM2.5.Concentration)) %>%
        group_by(Date, Site.ID) %>%
        summarize(Daily.Mean.PM2.5.Concentration = max(Daily.Mean.PM2.5.Concentration, na.rm = TRUE),
                  SITE_LATITUDE = first(SITE_LATITUDE),
                  SITE_LONGITUDE = first(SITE_LONGITUDE)) %>%
        ungroup()

# format date column 
all_aq_data$Date <- as.Date(all_aq_data$Date, format = "%m/%d/%Y") 
all_aq_data$year <- as.numeric(format(all_aq_data$Date, "%Y"))
all_aq_data$month <- as.numeric(format(all_aq_data$Date, "%m"))
all_aq_data$day <- as.numeric(format(all_aq_data$Date, "%d"))


# remove individual data frames from session since we have the aggregate
rm(aq_2002_data,
   aq_2003_data,
   aq_2004_data,
   aq_2005_data,
   aq_2006_data,
   aq_2007_data,
   aq_2008_data,
   aq_2009_data,
   aq_2010_data,
   aq_2011_data,
   aq_2012_data,
   aq_2013_data,
   aq_2014_data,
   aq_2015_data,
   aq_2016_data,
   aq_2017_data,
   aq_2018_data,
   aq_2019_data,
   aq_2020_data,
   aq_2021_data,
   aq_2022_data,
   aq_2023_data)


# custom function to convert calendar years into may-may school years
get_school_year_start <- function(date) {
  date <- as.Date(date)
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  
  if (month >= 3) {
    return(year)
  } else {
    return(year - 1)
  }

}


# convert calendar year dates into school year dates
aq_filter <- all_aq_data %>%
  mutate(school_year = sapply(Date, get_school_year_start))

# count days with very unhealthy within a given site ID and year
aq_filter <- aq_filter %>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_vuh = sum((Daily.Mean.PM2.5.Concentration >= 150.5))) %>%
  mutate(count = n()) 

aq_filter <- aq_filter %>%
 # filter(count >= 275)
  filter(count > 0)

aq_filter <- aq_filter %>%
  filter(school_year > 2001 & school_year < 2023)

write.csv(aq_filter, "all_aq_data.csv")

aq_cleaned <- read.csv("/Users/janani/Documents/thesis/interpolated_aq_180.csv")

aq_cleaned <- aq_cleaned %>%
  filter(max_consecutive_missing < 3) %>%
  group_by(Site.ID, school_year) %>%
  mutate(count = n()) %>%
  filter(count > 360)

# get daily average aq from monitor
average_readings <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  summarize(PM2.5_average = mean(Daily.Mean.PM2.5.Concentration, na.rm = TRUE),
            SITE_LATITUDE = first(SITE_LATITUDE),
            SITE_LONGITUDE = first(SITE_LONGITUDE),
            count = n()) 


# count days with moderate within a given site ID and year
aq_cleaned <- aq_cleaned%>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_good = sum(Daily.Mean.PM2.5.Concentration < 12.1, na.rm = TRUE))

# count days with moderate within a given site ID and year
aq_cleaned <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_mod = sum(Daily.Mean.PM2.5.Concentration >= 12.1 & Daily.Mean.PM2.5.Concentration <35.4, na.rm = TRUE))

# count days with somewhat unhealthy within a given site ID and year
aq_cleaned <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_uh1 = sum(Daily.Mean.PM2.5.Concentration >= 35.5 & Daily.Mean.PM2.5.Concentration < 55.4, na.rm = TRUE))

# count days with unhealthy aq within a given site ID and year
aq_cleaned <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_uh2 = sum(Daily.Mean.PM2.5.Concentration >= 55.5 & Daily.Mean.PM2.5.Concentration < 150.4, na.rm = TRUE))

# count days with very unhealthy within a given site ID and year
aq_cleaned <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  mutate(aq_vuh = sum(Daily.Mean.PM2.5.Concentration >= 150.5, na.rm = TRUE))

# just keep one row per site ID per year and drop additional columns
aq_cleaned <- aq_cleaned %>%
  group_by(Site.ID, school_year) %>%
  slice(1) 

print(n_distinct(aq_cleaned$Site.ID))
print(n_distinct(average_readings$Site.ID))

average_readings <- average_readings %>%
  select(c(Site.ID, school_year, PM2.5_average))

aq_cleaned <- inner_join(aq_cleaned, average_readings, by = c("Site.ID", "school_year"))

write.csv(aq_cleaned, "aq_balanced.csv")
# -----------------------------------------------------------------------------

monitor_frequency <- aq_cleaned %>%
  group_by(Site.ID) %>%
  summarise(num_years = n_distinct(year))

ggplot(monitor_frequency, aes(x = num_years)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "white", alpha = 1) +
  labs(x = "Number of Years Recorded", 
       y = "Monitor Count") +
  theme_minimal()

active_years <- aq_cleaned %>%
  select(Site.ID, year) %>%
  distinct() %>%
  mutate(Site.ID = as.character(Site.ID))

# Spread the table to have years as columns
binary_table <- active_years %>%
  mutate(value = 1) %>%
  spread(key = year, value = value, fill = 0)

long_data <- binary_table %>%
  gather(year, active, -Site.ID) %>%
  mutate(year = as.numeric(year))

# Filter out rows where active is 0 since we only want to plot lines for the active years
active_data <- long_data %>%
  filter(active == 1)

active_counts <- active_data %>% 
  group_by(Site.ID) %>% 
  summarise(active_years = sum(active)) %>% 
  ungroup()

# Merge with active_data
active_data <- active_data %>%
  left_join(active_counts, by = "Site.ID") 

breaks <- c(2, 6, 10, 14, 18)
labels <- c("1-4", "5-8", "9-12", "13-16", "17-20") 
ggplot(active_data, aes(x = as.numeric(year), y = Site.ID, group = Site.ID, color = active_years)) +
  geom_line(size = 2) +
  scale_color_viridis_c(name = "Total \nYears \nRecorded:", option = "C", breaks = breaks, labels = labels, direction = -1) +
  labs(x = "Year", y = "Monitor Site ID") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 6),
    legend.title = element_text(hjust = 0.5, size = 10) 
  )




