library(ggplot2)
library(stargazer)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(plm)

# skip to line 138 for just regressions

# ------------------------------------------------------------------------------
# code for cleaning original test data 
# commented out because this was done only once then saved
# ------------------------------------------------------------------------------

unclean_19 <- read.csv("/Users/janani/Downloads/raw_tests/ca2019_scores.csv")
unclean_19 <- unclean_19 %>%
rename(Mean.Scaled.Score = Mean.Scale.Score) %>%
rename(Year = Test.Year) %>%
rename(Test = Test.Type) %>%
rename(Subgroup = Subgroup.ID) %>%
filter(Mean.Scaled.Score != "*") %>%
filter(School.Code != 0) %>%
filter(School.Code != 1) %>%
filter(Test == "B") %>%

  mutate(across(c(Mean.Scaled.Score,
                  Percentage.Standard.Exceeded,
                  Percentage.Standard.Met,
                  Percentage.Standard.Met.and.Above,
                  Percentage.Standard.Nearly.Met,
                  Percentage.Standard.Not.Met,
                  Students.Tested), as.numeric))

  # mutate(across(c(Mean.Scaled.Score,
  #                 Percentage.Advanced,
  #                 Percentage.Proficient,
  #                 Percentage.At.Or.Above.Proficient,
  #                 Percentage.Basic,
  #                 Percentage.Below.Basic,
  #                 Percentage.Far.Below.Basic,
  #                 Students.Tested), as.numeric))

  # mutate(across(c(Mean.Scaled.Score,
  #                 CST.CAPA.Percentage.Advanced,
  #                 CST.CAPA.Percentage.Proficient,
  #                 CST.CAPA.Percentage.At.Or.Above.Proficient,
  #                 CST.CAPA.Percentage.Basic,
  #                 CST.CAPA.Percentage.Below.Basic,
  #                 CST.CAPA.Percentage.Far.Below.Basic,
  #                 Students.Tested), as.numeric))

write.csv(unclean_19, "/Users/janani/Downloads/raw_tests/ca2019_scores.csv")

# ------------------------------------------------------------------------------
unclean_03 <- read.csv("/Users/janani/Downloads/raw_tests/ca2003_scores.csv")

clean_03 <- unclean_03 %>%
  filter(Subgroup == 1) %>%
  mutate(Students.Tested = as.numeric(Students.Tested)) %>%
  # mutate(Students.Enrolled = as.numeric(Students.Enrolled)) %>%
  # mutate(Students.Enrolled = as.numeric(CAASPP.Reported.Enrollment)) %>%
  # mutate(Students.Enrolled = as.numeric(STAR.Reported.Enrollment.CAPA.Eligible)) %>%
  # filter(Students.Tested/Students.Enrolled > .75)
  filter(Percent.Tested <= 100) %>%
  filter(Percent.Tested > 80) %>%
  mutate(Percent.Tested = 0.01 * Percent.Tested)
  
data_dict_03 <- read.csv("/Users/janani/Downloads/raw_tests/ca2003_entities.csv") %>%
  dplyr::select(c(County.Code, District.Code, School.Code, School.Name, Zip.Code))

clean_03 <- clean_03 %>%
  group_by(County.Code, District.Code, School.Code, Grade, Year, Subgroup) %>%
  
  # summarize(Percentage.Standard.Exceeded = sum(Percentage.Standard.Exceeded * Students.Tested)/sum(Students.Tested),
  #           Percentage.Standard.Met = sum(Percentage.Standard.Met * Students.Tested)/sum(Students.Tested),
  #           Percentage.Standard.Nearly.Met = sum(Percentage.Standard.Nearly.Met * Students.Tested)/sum(Students.Tested),
  #           Percentage.Standard.Not.Met = sum(Percentage.Standard.Not.Met  * Students.Tested)/sum(Students.Tested),
  #           Percent.Tested = sum(Students.Tested)/sum(Students.Enrolled)
  #           )

  summarize(CST.CAPA.Percentage.Advanced = sum(CST.CAPA.Percentage.Advanced * Students.Tested)/sum(Students.Tested),
            CST.CAPA.Percentage.Proficient = sum(CST.CAPA.Percentage.Proficient * Students.Tested)/sum(Students.Tested),
            CST.CAPA.Percentage.Basic = sum(CST.CAPA.Percentage.Basic * Students.Tested)/sum(Students.Tested),
            CST.CAPA.Percentage.Below.Basic = sum(CST.CAPA.Percentage.Below.Basic * Students.Tested)/sum(Students.Tested),
            CST.CAPA.Percentage.Far.Below.Basic = sum(CST.CAPA.Percentage.Far.Below.Basic * Students.Tested)/sum(Students.Tested),
            Percent.Tested = sum(Percent.Tested * Students.Tested)/sum(Students.Tested)
            )

  # summarize(Percentage.Advanced = sum(Percentage.Advanced * Students.Tested)/sum(Students.Tested),
  #           Percentage.Proficient = sum(Percentage.Proficient * Students.Tested)/sum(Students.Tested),
  #           Percentage.Basic = sum(Percentage.Basic * Students.Tested)/sum(Students.Tested),
  #           Percentage.Below.Basic = sum(Percentage.Below.Basic * Students.Tested)/sum(Students.Tested),
  #           Percentage.Far.Below.Basic = sum(Percentage.Far.Below.Basic * Students.Tested)/sum(Students.Tested),
  #           Percent.Tested = sum(Percent.Tested * Students.Tested)/sum(Students.Tested)
  #           )


clean_03 <- inner_join(clean_03, data_dict_03, by = c("School.Code", "District.Code", "County.Code"))
rm(data_dict_03, unclean_03)

write.csv(clean_03, "clean_03_v2.csv")
 
# clean_04_schools <- clean_04 %>%
#   group_by(School.Code, School.Name) %>%
#   select(c(School.Code, School.Name)) %>%
#   distinct(School.Code, School.Name, .keep_all = FALSE)
# 

rm(list = ls())


# common_schools <- bind_rows(clean_23_schools,
#                             clean_22_schools,
#                             clean_21_schools,
#                             clean_19_schools,
#                             clean_18_schools,
#                             clean_17_schools,
#                             clean_16_schools,
#                             clean_15_schools,
#                             clean_13_schools,
#                             clean_12_schools,
#                             clean_11_schools,
#                             clean_10_schools,
#                             clean_09_schools,
#                             clean_08_schools,
#                             clean_07_schools,
#                             clean_06_schools,
#                             clean_05_schools,
#                             clean_04_schools)
# 
# common_schools <- common_schools %>%
#   group_by(School.Code) %>%
#   mutate(Appearances = n()) %>%
#   filter(Appearances > 10) %>%
#   slice(1)

#-------------------------------------------------------------------------------
# gender
#-------------------------------------------------------------------------------

rm(list = ls())
common_schools <- read.csv("/Users/janani/Documents/thesis/common_schools.csv")

unclean_23 <- read.csv("/Users/janani/Downloads/raw_tests/ca2023_scores.csv")
gender_23 <- unclean_23 %>%
  filter(Subgroup == 3 | Subgroup == 4) 
  # rename(Percentage.Standard.Met.and.Above = Percentage.At.Or.Above.Proficient)

gender_23 <- inner_join(gender_23, common_schools, by = c("Grade", "Year", "School.Code"))

data_dict_23 <- read.csv("/Users/janani/Downloads/raw_tests/ca2023_entities.csv",sep= "^") %>%
  dplyr::select(c(County.Code, District.Code, School.Code, School.Name, Zip.Code))

gender_23 <- gender_23 %>%
  group_by(County.Code, District.Code, School.Code, Grade, Year, Subgroup) %>%
  summarize(Percentage.Standard.Met.and.Above = sum(Percentage.Standard.Met.and.Above * Students.Tested)/sum(Students.Tested),
  )

gender_23 <- inner_join(gender_23, data_dict_23, by = c("School.Code", "District.Code", "County.Code"))
rm(unclean_23, data_dict_23)

write.csv(gender_23, "gender_clean_23.csv")

#-------------------------------------------------------------------------------
# race
#-------------------------------------------------------------------------------

rm(list = ls())
common_schools <- read.csv("/Users/janani/Documents/thesis/common_schools.csv")

unclean_03 <- read.csv("/Users/janani/Downloads/raw_tests/ca2003_scores.csv")
race_03 <- unclean_03 %>%
  filter(Subgroup == 80 | Subgroup == 74 | Subgroup == 78) %>%
  rename(Percentage.Standard.Met.and.Above = CST.CAPA.Percentage.At.Or.Above.Proficient)

race_03 <- inner_join(race_03, common_schools, by = c("Grade", "Year", "School.Code"))

data_dict_03 <- read.csv("/Users/janani/Downloads/raw_tests/ca2003_entities.csv") %>%
  dplyr::select(c(County.Code, District.Code, School.Code, School.Name, Zip.Code))

race_03 <- race_03 %>%
  group_by(County.Code, District.Code, School.Code, Grade, Year, Subgroup) %>%
  summarize(Percentage.Standard.Met.and.Above = sum(Percentage.Standard.Met.and.Above * Students.Tested)/sum(Students.Tested),
  )

race_03 <- inner_join(race_03, data_dict_03, by = c("School.Code", "District.Code", "County.Code"))
rm(unclean_03, data_dict_03)

write.csv(race_03, "race_clean_03.csv")

#-------------------------------------------------------------------------------
# income 
#-------------------------------------------------------------------------------
rm(list = ls())
common_schools <- read.csv("/Users/janani/Documents/thesis/common_schools.csv")

unclean_23 <- read.csv("/Users/janani/Downloads/raw_tests/ca2023_scores.csv")
income_23 <- unclean_23 %>%
  filter(Subgroup == 111 | Subgroup == 31) %>%
  rename(Percentage.Standard.Met.and.Above = Percentage.Standard.Met.and.Above)

income_23 <- inner_join(income_23, common_schools, by = c("Grade", "Year", "School.Code"))

data_dict_23 <- read.csv("/Users/janani/Downloads/raw_tests/ca2023_entities.csv", sep="^") %>%
  dplyr::select(c(County.Code, District.Code, School.Code, School.Name, Zip.Code))

income_23 <- income_23 %>%
  group_by(County.Code, District.Code, School.Code, Grade, Year, Subgroup) %>%
  summarize(Percentage.Standard.Met.and.Above = sum(Percentage.Standard.Met.and.Above * Students.Tested)/sum(Students.Tested),
  )

income_23 <- inner_join(income_23, data_dict_23, by = c("School.Code", "District.Code", "County.Code"))
rm(unclean_23, data_dict_23)

write.csv(income_23, "income_clean_23.csv")
