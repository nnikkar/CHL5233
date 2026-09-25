#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to
# a. answer the question
# b. be fully reproducible

#--------------------------------------------------
# Install and load packages
#--------------------------------------------------

install.packages("openintro")
library(openintro)

install.packages("tidyverse")
library(tidyverse)

install.packages("flextable")
library(flextable)

?yrbss


yrbss$Grade <- factor(
  yrbss$grade,
  levels = c("9", "10", "11", "12", "other")
)

yrbss$Gender <- stringr::str_to_title(yrbss$gender)

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)

ft_1 <- as_flextable(z)

ft_1


physical_activity <- aggregate(
  physically_active_7d ~ grade + gender,
  data = yrbss,
  FUN = mean,
  na.rm = TRUE
)

physical_activity$grade <- factor(
  physical_activity$grade,
  levels = c("9", "10", "11", "12", "other")
)

physical_activity$gender <- stringr::str_to_title(
  physical_activity$gender
)

ggplot(
  physical_activity,
  aes(
    x = grade,
    y = physically_active_7d,
    color = gender,
    group = gender
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Physical Activity by Grade and Gender",
    x = "Grade",
    y = "Average Number of Physically Active Days",
    color = "Gender"
  ) +
  theme_minimal()

yrbss$bmi <- yrbss$weight / (yrbss$height^2)


grade12_female <- yrbss |>
  filter(
    grade == "12",
    gender == "female"
  )

ggplot(
  grade12_female,
  aes(
    x = bmi,
    y = physically_active_7d
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Physical Activity and BMI Among Grade 12 Female Students",
    x = "BMI",
    y = "Number of Physically Active Days"
  ) +
  theme_minimal()
