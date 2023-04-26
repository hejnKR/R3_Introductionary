# Loading Packages --------------------------------------------------------
library(tidyverse)
library(NHANES)
library(snakecase)

# Looking at data --------------------------------------------------------
glimpse(NHANES)

# Selecting data --------------------------------------------------------

# Select one column by its name, without quotes
select(NHANES, Age)

# Select two or more columns by name, without quotes
select(NHANES, Age, Weight, BMI)

# To *exclude* a column, use minus (-)
select(NHANES, -HeadCirc)

# All columns starting with letters "BP" (blood pressure)
select(NHANES, starts_with("BP"))

# All columns ending in letters "Day"
select(NHANES, ends_with("Day"))

# All columns containing letters "Age"
select(NHANES, contains("Age"))

# Save the selected columns as a new data frame
# Recall the style guide for naming objects
nhanes_small <- select(
  NHANES, Age, Gender, BMI, Diabetes,
  PhysActive, BPSysAve, BPDiaAve, Education
)

# View the new data frame
nhanes_small

# Updating colnames to style guide --------------------------------------------------------

# Rename all columns to snake case
nhanes_small <- rename_with(nhanes_small, snakecase::to_snake_case)
# to_snake_case is a function, but as the function is given to anotehr function () are not used (that would make it look for data)

# Have a look at the data frame
nhanes_small

# changing gender to sex
nhanes_small <- rename(nhanes_small, sex = gender)
nhanes_small

# Turning on the pipe --------------------------------------------------------
# These two ways are the same
colnames(nhanes_small)

nhanes_small %>%
  colnames()

nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)

nhanes_small %>%
  select(bp_sys_ave, education)

nhanes_small %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )

# from
select(nhanes_small, bmi, contains("age"))
# to
nhanes_small %>%
  select(bmi, contains("age"))
# from
blood_pressure <- select(nhanes_small, starts_with("bp_"))
rename(blood_pressure, bp_systolic = bp_sys_ave)
# to
nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)

# Filtering --------------------------------------------------------
nhanes_small %>%
  filter(phys_active != "No")

nhanes_small %>%
  filter(bmi >= 25)

nhanes_small %>%
  filter(phys_active == "No" & bmi >= 25)

nhanes_small %>%
  filter(bmi >= 25 | phys_active == "No")

# Arranging data --------------------------------------------------------
nhanes_small %>%
  arrange(age)

nhanes_small %>%
  arrange(desc(age))

nhanes_small %>%
  arrange(education, age)

# Transform data --------------------------------------------------------
# adding new column
nhanes_small %>%
  mutate(age = age * 12)

nhanes_small %>%
  mutate(
    age = age * 12,
    logged_bmi = log(bmi)
  )

nhanes_small %>%
  mutate(old = if_else(age >= 30, "Yes", "No"))

# EX 7.12 -----------------------------------------------------------------
# 1. BMI between 20 and 40 with diabetes
nhanes_small %>%
    # Format should follow: variable >= number or character
    filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes")

# Pipe the data into mutate function and:
nhanes_modified <- nhanes_small %>% # Specifying dataset
    mutate(
        # 2. Calculate mean arterial pressure
        art_pre_mean = (((2*bp_sys_ave) + bp_sys_ave)/3),
        # 3. Create young_child variable using a condition
        young_child = if_else(age < 6, "Yes", "No")
    )

nhanes_modified


# Creating summary statistics ---------------------------------------------
nhanes_small %>%
    summarise(max_bmi = max(bmi))

nhanes_small %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE))

nhanes_small %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE),
              min_bmi = min(bmi, na.rm = TRUE))

bmi_summary <- nhanes_small %>%
    summarise(max_bmi = max(bmi, na.rm = TRUE),
              min_bmi = min(bmi, na.rm = TRUE))

bmi_summary

nhanes_small %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age, na.rm = TRUE),
              mean_bmi = mean(bmi, na.rm = TRUE)
              )

nhanes_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age, na.rm = TRUE),
              mean_bmi = mean(bmi, na.rm = TRUE)
    ) %>%
    ungroup()


# Saving Data -------------------------------------------------------------
readr::write_csv(nhanes_small, here::here("data/nhanes_small.csv"))


