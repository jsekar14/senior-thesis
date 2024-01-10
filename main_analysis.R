library(ggplot2)
library(stargazer)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(plm)
library(patchwork)
library(sandwich)
library(lmtest)
library(lfe)
library(estimatr)


# ------------------------------------------------------------------------------
# reading in cleaned test data, first the old tests then the new
# ------------------------------------------------------------------------------
rm(list = ls())
clean_04 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_04_v2.csv")[, -1] 
clean_05 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_05_v2.csv")[, -1] 
clean_06 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_06_v2.csv")[, -1] 
clean_07 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_07_v2.csv")[, -1]
clean_08 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_08_v2.csv")[, -1]  
clean_09 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_09_v2.csv")[, -1] 
clean_10 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_10_v2.csv")[, -1] 
clean_11 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_11_v2.csv")[, -1]
clean_12 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_12_v2.csv")[, -1] 
clean_13 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_13_v2.csv")[, -1]

all_state_test_data_old <- na.omit(bind_rows(clean_04,
                                             clean_05,
                                             clean_06,
                                             clean_07,
                                             clean_08,
                                             clean_09,
                                             clean_10,
                                             clean_11,
                                             clean_12,
                                             clean_13)) %>%
  mutate(Percentage.At.Or.Above.Proficient = Percentage.Proficient + Percentage.Advanced)

all_state_test_data_old <- all_state_test_data_old %>% 
  dplyr::mutate(Percentage.Below.Basic = Percentage.Below.Basic + Percentage.Far.Below.Basic) %>%
  dplyr::select(!c(Percentage.Far.Below.Basic)) %>%
  dplyr::rename("Percentage.Standard.Exceeded"= "Percentage.Advanced",
                "Percentage.Standard.Met" = "Percentage.Proficient",
                "Percentage.Standard.Met.and.Above" = "Percentage.At.Or.Above.Proficient",
                "Percentage.Standard.Nearly.Met" = "Percentage.Basic",
                "Percentage.Standard.Not.Met" = "Percentage.Below.Basic") 

rm(clean_04,
   clean_05,
   clean_06,
   clean_07,
   clean_08,
   clean_09,
   clean_10,
   clean_11,
   clean_12,
   clean_13)


clean_15 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_15.csv")[, -1]
clean_16 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_16.csv")[, -1] 
clean_17 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_17.csv")[, -1]
clean_18 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_18.csv")[, -1]
clean_19 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_19.csv")[, -1]
clean_21 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_21.csv")[, -1] 
clean_22 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_22.csv")[, -1]
clean_23 <- read.csv("/Users/janani/Documents/thesis/test_data/clean_23.csv")[, -1]

all_state_test_data_new <- na.omit(bind_rows(clean_15,
                                             clean_16,
                                             clean_17,
                                             clean_18,
                                             clean_19,
                                             clean_21,
                                             clean_22, 
                                             clean_23) %>%
  mutate(Percentage.Standard.Met.and.Above = Percentage.Standard.Exceeded + Percentage.Standard.Met))

rm(clean_15,
   clean_16,
   clean_17,
   clean_18,
   clean_19,
   clean_21, 
   clean_22,
   clean_23)

all_state_test_data_old$Year <- all_state_test_data_old$Year - 1
all_state_test_data_new$Year <- all_state_test_data_new$Year - 1
all_state_test_data <- rbind(all_state_test_data_new, all_state_test_data_old)

# ------------------------------------------------------------------------------
# adding lat and lon coordinates for every school based on mapping table
# ------------------------------------------------------------------------------

school_to_city <- read_excel("/Users/janani/Documents/thesis/school_to_city_mapping.xlsx")
school_to_city$NMCNTY <- substr(school_to_city$NMCNTY, 1, nchar(school_to_city$NMCNTY) - 7)

school_to_city = school_to_city %>%
  dplyr::select("NAME","LAT", "LON", "ZIP") %>%
  dplyr::mutate(ZIP = as.numeric(ZIP)) %>%
  distinct()

tests_and_coords <- na.omit(left_join(all_state_test_data, school_to_city, by = c("School.Name"="NAME", "Zip.Code" = "ZIP")))

write.csv(tests_and_coords, "tests_and_coords.csv")

# ------------------------------------------------------------------------------
# doing the merge with the aq data in python, so that isn't here 
# can start from here if data cleaning is not required
# ------------------------------------------------------------------------------
rm(list = ls())
full_panel = na.omit(read.csv("/Users/janani/Documents/thesis/interpolated_tests_coords_pollution_275_3.csv"))

# generate summary statistics by year for tests 
# (these are cross-entity, cross-grade/cohort means with large standard errors)
summary_test_scores <- full_panel %>%
  group_by(Year) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  )

empty_2013 <- data.frame(Year = 2013,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)

empty_2019 <- data.frame(Year = 2019,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)

summary_test_scores <- rbind(summary_test_scores, empty_2013, empty_2019)
ggplot(summary_test_scores, aes(x = Year)) +
  geom_line(aes(y = mean_percent_passing, color = "Mean Percent At and Above Standards")) +
  geom_line(aes(y = median_percent_passing, color = "Median Percent At and Above Standards")) + 
  geom_errorbar(
    aes(ymin = mean_percent_passing - sd_percent_passing, ymax = mean_percent_passing + sd_percent_passing),
    width = 0.2,  # Adjust the width of error bars as needed
  ) +
  labs(
    title = "Aggregate Mean and Median Percent of Passing\n Scores per Year",
    x = "Year",
    y = "Score",
    color = " "
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))



# ------------------------------------------------------------------------------
# regressions
# ------------------------------------------------------------------------------
panel_filtered <- full_panel %>%
  filter(Year != 2020) %>%
  filter(closest_point_distance <= 25)

schools_with_10_years <- full_panel %>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 5)

panel_filtered <- panel_filtered %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

model_simple <- lm(Percentage.Standard.Met.and.Above ~ aq_avg, data = panel_filtered)
summary(model_simple)

model_simple_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_avg + as.factor(Year) | School.Code | 0 | monitor, data = panel_filtered)
summary(model_simple_fe, robust = TRUE)

model_simple_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_avg + as.factor(Year) | School.Code , data = panel_filtered)
summary(model_simple_fe)

ggplot() +
  stat_summary_bin(data = panel_filtered, aes(x = aq_avg, y = Percentage.Standard.Met.and.Above, color="Average PM2.5 Pollution over School Year"), fun ='mean', bins=20, size=3, geom='point') + 
  geom_abline(intercept = 53.62, slope = -.488, color = "black")+
  labs(
    x = "Average PM2.5 Exposure During School Year",
    y = "Mean Percentage of Students \nMeeting and Exceeding Standards",
    color = " "
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), 
        text = element_text(size=15)) 

# ------------------------------------------------------------------------------
# segmented approach 
# ------------------------------------------------------------------------------
naive_ols <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = panel_filtered)
summary(naive_ols)

stargazer(naive_ols)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 |monitor, data = panel_filtered)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Grade) * as.factor(Year) | School.Code | 0 | monitor, data = panel_filtered)
summary(time_entity_fe, robust = TRUE)

stargazer(time_entity_fe, type="latex", title="Regression Results", label="tab:results", table.placement="H", robust = TRUE, header=FALSE)

ggplot(data = panel_filtered) +
  stat_summary_bin(aes(x = aq_uh1, y = Percentage.Standard.Met.and.Above, color="Total Days Unhealthy for Sensitive Groups"), fun ='mean', bins=30, size=3, geom='point') + 
  stat_summary_bin(aes(x = aq_uh2, y = Percentage.Standard.Met.and.Above, color="Total Days Unhealthy"), fun ='mean', bins=30, size=3, geom='point') + 
  stat_summary_bin(aes(x = aq_vuh, y = Percentage.Standard.Met.and.Above, color="Total Days Very Unhealthy"), fun ='mean', bins=30, size=3, geom='point') +
  scale_color_manual(values = c("Total Days Unhealthy for Sensitive Groups" = "orange", "Total Days Unhealthy" = "red", "Total Days Very Unhealthy" = "maroon")) +
  labs(
    x = "Mean Number of Days Above Threshold",
    color = " "
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 

# ------------------------------------------------------------------------------
rm(list = ls())

gender_04 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_04.csv")[, -1]
gender_05 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_05.csv")[, -1]
gender_06 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_06.csv")[, -1]
gender_07 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_07.csv")[, -1]
gender_08 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_08.csv")[, -1]
gender_09 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_09.csv")[, -1]
gender_10 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_10.csv")[, -1]
gender_11 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_11.csv")[, -1]
gender_12 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_12.csv")[, -1]
gender_13 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_13.csv")[, -1]


all_state_test_data_old <- na.omit(bind_rows(gender_04,
                                             gender_05,
                                             gender_06,
                                             gender_07,
                                             gender_08,
                                             gender_09,
                                             gender_10,
                                             gender_11,
                                             gender_12,
                                             gender_13))
                                  
rm(gender_04,
   gender_05,
   gender_06,
   gender_07,
   gender_08,
   gender_09,
   gender_10,
   gender_11,
   gender_12,
   gender_13)

gender_clean_15 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_15.csv")[, -1] 
gender_clean_16 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_16.csv")[, -1]
gender_clean_17 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_17.csv")[, -1]
gender_clean_18 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_18.csv")[, -1]
gender_clean_19 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_19.csv")[, -1]
gender_clean_21 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_21.csv")[, -1]
gender_clean_22 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_22.csv")[, -1]
gender_clean_23 <- read.csv("/Users/janani/Documents/thesis/gender_test_data/gender_clean_23.csv")[, -1]
all_state_test_data_new <- na.omit(bind_rows(gender_clean_15,
                                             gender_clean_16,
                                             gender_clean_17,
                                             gender_clean_18,
                                             gender_clean_19,
                                             gender_clean_21,
                                             gender_clean_22,
                                             gender_clean_23)) 
rm(gender_clean_15,
   gender_clean_16,
   gender_clean_17,
   gender_clean_18,
   gender_clean_19,
   gender_clean_21, 
   gender_clean_22,
   gender_clean_23)

all_state_test_data_old$Year <- all_state_test_data_old$Year - 1
all_state_test_data_new$Year <- all_state_test_data_new$Year - 1
all_state_test_data <- rbind(all_state_test_data_new, all_state_test_data_old)

rm(all_state_test_data_new, all_state_test_data_old)

# ------------------------------------------------------------------------------
# adding lat and lon coordinates for every school based on mapping table
# ------------------------------------------------------------------------------

school_to_city <- read_excel("/Users/janani/Documents/thesis/school_to_city_mapping.xlsx")
school_to_city$NMCNTY <- substr(school_to_city$NMCNTY, 1, nchar(school_to_city$NMCNTY) - 7)

school_to_city = school_to_city %>%
  dplyr::select("NAME","LAT", "LON", "ZIP") %>%
  dplyr::mutate(ZIP = as.numeric(ZIP)) %>%
  distinct()

tests_and_coords <- na.omit(inner_join(all_state_test_data, school_to_city, by = c("School.Name"="NAME", "Zip.Code" = "ZIP")))

write.csv(tests_and_coords, "gender_tests_and_coords.csv")

# ------------------------------------------------------------------------------
# regressions for gender
# ------------------------------------------------------------------------------
rm(list = ls())
full_panel = na.omit(read.csv("/Users/janani/Documents/thesis/gender_tests_coords_pollution.csv"))

# generate summary statistics by year for tests 
# (these are cross-entity, cross-grade/cohort means with large standard errors)
summary_test_scores <- full_panel %>%
  group_by(Year) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  )

empty_2013 <- data.frame(Year = 2013,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)
empty_2019 <- data.frame(Year = 2019,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)

summary_test_scores_male <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 3)

summary_test_scores_male <- rbind(summary_test_scores_male, empty_2013, empty_2019)

summary_test_scores_female <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 4)

summary_test_scores_female <- rbind(summary_test_scores_female, empty_2013, empty_2019)


# graph separated by gender
ggplot() +
  geom_line(data = summary_test_scores_female, aes(x = Year, y = mean_percent_passing, color = "Female Mean")) +
  geom_point(data = summary_test_scores_female, aes(x = Year, y = mean_percent_passing, color = "Female Mean")) +
  geom_line(data = summary_test_scores_female, aes(x = Year, y = median_percent_passing, color = "Female Median")) +
  geom_point(data = summary_test_scores_female, aes(x = Year, y = median_percent_passing, color = "Female Median")) +  
  geom_line(data = summary_test_scores_male, aes(x = Year, y = mean_percent_passing, color = "Male Mean")) +
  geom_point(data = summary_test_scores_male, aes(x = Year, y = mean_percent_passing, color = "Male Mean")) +
  geom_line(data = summary_test_scores_male, aes(x = Year, y = median_percent_passing, color = "Male Median")) +
  geom_point(data = summary_test_scores_male, aes(x = Year, y = median_percent_passing, color = "Male Median")) +
  labs(
    x = "Year",
    y = "Percentage Meeting and Exceeding Standards",
    color = " "
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------
# regressions
# ------------------------------------------------------------------------------
full_panel_women <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Grade != 13) %>%
  filter(Year != 2020) %>%
  filter(Subgroup == 4)

schools_with_10_years <- full_panel_women%>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_women <- full_panel_women %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

full_panel_men <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Grade != 13) %>%
  filter(Year != 2020) %>%
  filter(Subgroup == 3) 

schools_with_10_years <- full_panel_men%>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_men <- full_panel_men %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

naive_ols_women <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_women)
summary(naive_ols_women)

naive_ols_men <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_men)
summary(naive_ols_men)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~  aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 | monitor, data = full_panel_women)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 | monitor, data = full_panel_men)
summary(time_entity_fe, robust = TRUE)

gender_all <- rbind(full_panel_men, full_panel_women)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Subgroup) + aq_vuh * as.factor(Subgroup) + as.factor(Year) * as.factor(Grade)| School.Code | 0 | monitor, data = gender_all)
summary(time_entity_fe, robust = TRUE)

stargazer(time_entity_fe)

# ------------------------------------------------------------------------------
rm(list = ls())

race_04 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_04.csv")[, -1]
race_05 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_05.csv")[, -1]
race_06 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_06.csv")[, -1]
race_07 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_07.csv")[, -1]
race_08 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_08.csv")[, -1]
race_09 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_09.csv")[, -1]
race_10 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_10.csv")[, -1]
race_11 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_11.csv")[, -1]
race_12 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_12.csv")[, -1]
race_13 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_13.csv")[, -1]


all_state_test_data_old <- na.omit(bind_rows(race_04,
                                             race_05,
                                             race_06,
                                             race_07,
                                             race_08,
                                             race_09,
                                             race_10,
                                             race_11,
                                             race_12,
                                             race_13))

rm(race_04,
   race_05,
   race_06,
   race_07,
   race_08,
   race_09,
   race_10,
   race_11,
   race_12,
   race_13)

race_clean_15 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_15.csv")[, -1] 
race_clean_16 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_16.csv")[, -1]
race_clean_17 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_17.csv")[, -1]
race_clean_18 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_18.csv")[, -1]
race_clean_19 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_19.csv")[, -1]
race_clean_21 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_21.csv")[, -1]
race_clean_22 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_22.csv")[, -1]
race_clean_23 <- read.csv("/Users/janani/Documents/thesis/race_test_data/race_clean_23.csv")[, -1]
all_state_test_data_new <- na.omit(bind_rows(race_clean_15,
                                             race_clean_16,
                                             race_clean_17,
                                             race_clean_18,
                                             race_clean_19,
                                             race_clean_21,
                                             race_clean_22,
                                             race_clean_23)) 
rm(race_clean_15,
   race_clean_16,
   race_clean_17,
   race_clean_18,
   race_clean_19,
   race_clean_21, 
   race_clean_22,
   race_clean_23)

all_state_test_data_old$Year <- all_state_test_data_old$Year - 1
all_state_test_data_new$Year <- all_state_test_data_new$Year - 1
all_state_test_data <- rbind(all_state_test_data_new, all_state_test_data_old)

rm(all_state_test_data_new, all_state_test_data_old)

# ------------------------------------------------------------------------------
# adding lat and lon coordinates for every school based on mapping table
# ------------------------------------------------------------------------------

school_to_city <- read_excel("/Users/janani/Documents/thesis/school_to_city_mapping.xlsx")
school_to_city$NMCNTY <- substr(school_to_city$NMCNTY, 1, nchar(school_to_city$NMCNTY) - 7)

school_to_city = school_to_city %>%
  dplyr::select("NAME","LAT", "LON", "ZIP") %>%
  dplyr::mutate(ZIP = as.numeric(ZIP)) %>%
  distinct()

tests_and_coords <- na.omit(inner_join(all_state_test_data, school_to_city, by = c("School.Name"="NAME", "Zip.Code" = "ZIP")))

write.csv(tests_and_coords, "race_tests_and_coords.csv")

# ------------------------------------------------------------------------------
# regressions for race
# ------------------------------------------------------------------------------

rm(list = ls())
full_panel = na.omit(read.csv("/Users/janani/Documents/thesis/race_tests_coords_pollution.csv"))

# generate summary statistics by year for tests 
# (these are cross-entity, cross-grade/cohort means with large standard errors)
summary_test_scores <- full_panel %>%
  group_by(Year) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  )

empty_2013 <- data.frame(Year = 2013,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)
empty_2019 <- data.frame(Year = 2019,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)

summary_test_scores_black <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 74)

summary_test_scores_black <- rbind(summary_test_scores_black, empty_2013, empty_2019)

summary_test_scores_white <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 80)

summary_test_scores_white <- rbind(summary_test_scores_white, empty_2013, empty_2019)

# graph separated by race
ggplot() +
  geom_line(data = summary_test_scores_black, aes(x = Year, y = mean_percent_passing, color = "Black Mean")) +
  geom_point(data = summary_test_scores_black, aes(x = Year, y = mean_percent_passing, color = "Black Mean")) +
  geom_line(data = summary_test_scores_black, aes(x = Year, y = median_percent_passing, color = "Black Median")) +
  geom_point(data = summary_test_scores_black, aes(x = Year, y = median_percent_passing, color = "Black Median")) +
  geom_line(data = summary_test_scores_white, aes(x = Year, y = mean_percent_passing, color = "White Mean")) +
  geom_point(data = summary_test_scores_white, aes(x = Year, y = mean_percent_passing, color = "White Mean")) +
  geom_line(data = summary_test_scores_white, aes(x = Year, y = median_percent_passing, color = "White Median")) +
  geom_point(data = summary_test_scores_white, aes(x = Year, y = median_percent_passing, color = "White Median")) +
  labs(
    title = "",
    x = "Year",
    y = "Percentage Meeting and Exceeding Standards",
    color = ""
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------
# regressions
# ------------------------------------------------------------------------------
full_panel_white <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Year != 2020) %>%
  filter(Grade != 13) %>%
  filter(Subgroup == 80)

schools_with_10_years <- full_panel_white%>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_white <- full_panel_white %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

full_panel_black <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Year != 2020) %>%
  filter(Grade != 13) %>%
  filter(Subgroup == 74) 

schools_with_10_years <- full_panel_black%>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_black <- full_panel_black %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

naive_ols_white <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_white)
summary(naive_ols_white)

naive_ols_black <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_black)
summary(naive_ols_black)

# this looks good at 10km radius with data going back to 2004

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~  aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 | monitor, data = full_panel_white)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Grade) * as.factor(Year) | School.Code | 0 | monitor, data = full_panel_white)
summary(time_entity_fe, robust = TRUE)

full_panel_white <- full_panel_white %>%
  filter(School.Code %in% full_panel_black$School.Code)

all_race <- rbind(full_panel_black, full_panel_white)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Subgroup) + as.factor(Subgroup) * aq_vuh + as.factor(Grade) * as.factor(Year) | School.Code | 0 | monitor, data = all_race)
summary(time_entity_fe, robust = TRUE)

stargazer(time_entity_fe)

# ------------------------------------------------------------------------------
rm(list = ls())

income_04 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_04.csv")[, -1]
income_05 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_05.csv")[, -1]
income_06 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_06.csv")[, -1]
income_07 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_07.csv")[, -1]
income_08 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_08.csv")[, -1]
income_09 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_09.csv")[, -1]
income_10 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_10.csv")[, -1]
income_11 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_11.csv")[, -1]
income_12 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_12.csv")[, -1]
income_13 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_13.csv")[, -1]


all_state_test_data_old <- na.omit(bind_rows(income_04,
                                             income_05,
                                             income_06,
                                             income_07,
                                             income_08,
                                             income_09,
                                             income_10,
                                             income_11,
                                             income_12,
                                             income_13))

rm(income_04,
   income_05,
   income_06,
   income_07,
   income_08,
   income_09,
   income_10,
   income_11,
   income_12,
   income_13)

income_clean_15 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_15.csv")[, -1] 
income_clean_16 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_16.csv")[, -1]
income_clean_17 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_17.csv")[, -1]
income_clean_18 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_18.csv")[, -1]
income_clean_19 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_19.csv")[, -1]

income_clean_21 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_21.csv")[, -1]
income_clean_22 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_22.csv")[, -1]
income_clean_23 <- read.csv("/Users/janani/Documents/thesis/income_test_data/income_clean_23.csv")[, -1]
all_state_test_data_new <- na.omit(bind_rows(income_clean_15,
                                             income_clean_16,
                                             income_clean_17,
                                             income_clean_18,
                                             income_clean_19,
                                             income_clean_21,
                                             income_clean_22,
                                             income_clean_23)) 
rm(income_clean_15,
   income_clean_16,
   income_clean_17,
   income_clean_18,
   income_clean_19,
   income_clean_21,
   income_clean_22,
   income_clean_23)

all_state_test_data_old$Year <- all_state_test_data_old$Year - 1
all_state_test_data_new$Year <- all_state_test_data_new$Year - 1
all_state_test_data <- rbind(all_state_test_data_new, all_state_test_data_old)

rm(all_state_test_data_new, all_state_test_data_old)

# ------------------------------------------------------------------------------
# adding lat and lon coordinates for every school based on mapping table
# ------------------------------------------------------------------------------

school_to_city <- read_excel("/Users/janani/Documents/thesis/school_to_city_mapping.xlsx")
school_to_city$NMCNTY <- substr(school_to_city$NMCNTY, 1, nchar(school_to_city$NMCNTY) - 7)

school_to_city = school_to_city %>%
  dplyr::select("NAME","LAT", "LON", "ZIP") %>%
  dplyr::mutate(ZIP = as.numeric(ZIP)) %>%
  distinct()

tests_and_coords <- na.omit(inner_join(all_state_test_data, school_to_city, by = c("School.Name"="NAME", "Zip.Code" = "ZIP")))

write.csv(tests_and_coords, "income_tests_and_coords.csv")

# ------------------------------------------------------------------------------
# regressions for income
# ------------------------------------------------------------------------------

rm(list = ls())
full_panel = na.omit(read.csv("/Users/janani/Documents/thesis/income_tests_coords_pollution.csv"))

# generate summary statistics by year for tests 
# (these are cross-entity, cross-grade/cohort means with large standard errors)
summary_test_scores <- full_panel %>%
  group_by(Year) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  )

empty_2013 <- data.frame(Year = 2013,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)
empty_2019 <- data.frame(Year = 2019,
                         mean_percent_passing = NA,
                         median_percent_passing = NA,
                         sd_percent_passing = NA)

summary_test_scores_poor <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 31)

summary_test_scores_poor <- rbind(summary_test_scores_poor, empty_2013, empty_2019)

summary_test_scores_rich <- full_panel %>%
  group_by(Year, Subgroup) %>%
  summarize(
    mean_percent_passing = mean(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    median_percent_passing = median(Percentage.Standard.Met.and.Above, na.rm = TRUE),
    sd_percent_passing = sd(Percentage.Standard.Met.and.Above, na.rm = TRUE)
  ) %>%
  filter(Subgroup == 111)


summary_test_scores_rich <- rbind(summary_test_scores_rich, empty_2013, empty_2019)

# graph separated by race
ggplot() +
  geom_line(data = summary_test_scores_poor, aes(x = Year, y = mean_percent_passing, color = "Economically\n Disadvantaged Mean")) +
  geom_point(data = summary_test_scores_poor, aes(x = Year, y = mean_percent_passing, color = "Economically\n Disadvantaged Mean")) +
  geom_line(data = summary_test_scores_poor, aes(x = Year, y = median_percent_passing, color = "Economically\n Disadvantaged Median")) +
  geom_point(data = summary_test_scores_poor, aes(x = Year, y = median_percent_passing, color = "Economically\n Disadvantaged Median")) +
  geom_line(data = summary_test_scores_rich, aes(x = Year, y = mean_percent_passing, color = "Not Economically\n Disadvantaged Mean")) +
  geom_point(data = summary_test_scores_rich, aes(x = Year, y = mean_percent_passing, color = "Not Economically\n Disadvantaged Mean")) +
  geom_line(data = summary_test_scores_rich, aes(x = Year, y = median_percent_passing, color = "Not Economically\n Disadvantaged Median")) +
  geom_point(data = summary_test_scores_rich, aes(x = Year, y = median_percent_passing, color = "Not Economically\n Disadvantaged Median")) +
  labs(
    title = "Mean and Median Percent of Passing Scores per Year",
    x = "Year",
    y = "Score",
    color = " "
  ) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------
# regressions
# ------------------------------------------------------------------------------
full_panel_rich <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Year != 2020) %>%
  filter(Grade != 13) %>%
  filter(Subgroup == 111) 

schools_with_10_years <- full_panel_rich %>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_rich <- full_panel_rich %>%
  filter(School.Code %in% schools_with_10_years$School.Code)

full_panel_poor <- full_panel %>%
  filter(closest_point_distance < 25) %>%
  filter(Year != 2020) %>%
  filter(Grade != 13) %>%
  filter(Subgroup == 31) 


schools_with_10_years <- full_panel_poor %>%
  group_by(School.Code) %>%
  summarize(number_of_years = n_distinct(Year)) %>%
  ungroup() %>%
  filter(number_of_years >= 8)

full_panel_poor <- full_panel_poor %>%
  filter(School.Code %in% schools_with_10_years$School.Code)


naive_ols_rich <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_rich)
summary(naive_ols_rich)

naive_ols_poor <- lm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh, data = full_panel_poor)
summary(naive_ols_poor)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~  aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 | monitor, data = full_panel_rich)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Grade) * as.factor(Year) | School.Code | 0 | monitor, data = full_panel_rich)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~  aq_uh1 + aq_uh2 + aq_vuh + as.factor(Year) | School.Code | 0 | monitor, data = full_panel_poor)
summary(time_entity_fe, robust = TRUE)

time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh + as.factor(Grade) * as.factor(Year) | School.Code | 0 | monitor, data = full_panel_poor)
summary(time_entity_fe, robust = TRUE)

both <- rbind(full_panel_poor, full_panel_rich)
time_entity_fe <- felm(Percentage.Standard.Met.and.Above ~ aq_uh1 + aq_uh2 + aq_vuh +  as.factor(Year) * as.factor(Grade) + as.factor(Subgroup) + as.factor(Subgroup) * aq_vuh| School.Code | 0 | monitor, data = both)
summary(time_entity_fe, robust = TRUE)

stargazer(time_entity_fe)
