install.packages(c("tidyverse", "lubridate", "ggplot2", "scales", "pracma"))
library(pracma)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lubridate)

# Define the URL for the CSV file
data_url <- "https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/dc5c8788-792f-4f91-a400-036cdf28cfe8/download/schoolrecentcovid2021_2022.csv"

# Read the CSV file into R
school_data <- read_csv(data_url)
# View the first few rows
# head(school_data)

# # Get a summary of the dataset
# summary(school_data)
school_data <- school_data %>%
    mutate(reported_date = as.Date(reported_date, format = "%Y-%m-%d"))
school_data <- school_data %>%
    mutate(collected_date = as.Date(collected_date, format = "%Y-%m-%d"))
daily_cases <- school_data %>%
    group_by(reported_date) %>%
    summarise(total_cases = sum(total_confirmed_cases, na.rm = TRUE))
p <- ggplot(daily_cases, aes(x = reported_date, y = total_cases)) +
    geom_line(color = "steelblue", size = 1) +
    labs(
        title = "Daily COVID-19 Cases in Ontario Schools",
        x = "Date",
        y = "Number of Reported Cases"
    ) +
    theme_minimal()
print(p) # <- This ensures the plot shows up in VS Code
ggsave("ontario_school_cases_plot.jpg", plot = p, width = 10, height = 6)


# Aggregate student and staff cases over time
role_trend <- school_data %>%
    group_by(reported_date) %>%
    summarise(
        Students = sum(confirmed_student_cases, na.rm = TRUE),
        Staff = sum(confirmed_staff_cases, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c("Students", "Staff"), names_to = "Role", values_to = "Cases")

# Plot
p2 <- ggplot(role_trend, aes(x = reported_date, y = Cases, color = Role)) +
    geom_line(linewidth = 1) +
    labs(title = "COVID-19 Cases by Role in Ontario Schools", x = "Date", y = "Cases") +
    theme_minimal()

print(p2)
ggsave("COVID-19 Cases by Role in Ontario Schools.jpg", plot = p2, width = 10, height = 6)

top_boards <- school_data %>%
    group_by(school_board) %>%
    summarise(total_cases = sum(confirmed_student_cases + confirmed_staff_cases + confirmed_unspecified_cases, na.rm = TRUE)) %>%
    arrange(desc(total_cases)) %>%
    slice_head(n = 10)

p3 <- ggplot(top_boards, aes(x = reorder(school_board, total_cases), y = total_cases)) +
    geom_col(fill = "tomato") +
    coord_flip() +
    labs(title = "Top 10 Affected School Boards", x = "School Board", y = "Total Cases") +
    theme_minimal()
# print(p3) # <- This ensures the plot shows up in VS Code
# ggsave("Top 10 affected school boards.jpg", plot = p3, width = 10, height = 6)
cumulative <- daily_cases %>%
    mutate(cumulative_cases = cumsum(total_cases))
p4 <- ggplot(cumulative, aes(x = reported_date, y = cumulative_cases)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    labs(title = "Cumulative COVID-19 Cases in Ontario Schools", x = "Date", y = "Cumulative Cases") +
    theme_minimal()
# print(p4) # <- This ensures the plot shows up in VS Code
# ggsave("Cumulative COVID-19 Cases in Ontario Schools.jpg", plot = p4, width = 10, height = 6)
print(summary(cumulative))
# print(sum(school_data$confirmed_student_cases, na.rm = TRUE))
# print(sum(school_data$confirmed_staff_cases, na.rm = TRUE))
# print(sum(school_data$confirmed_unspecified_cases, na.rm = TRUE))
library(ggplot2)

p5 <- ggplot(cumulative, aes(x = reported_date)) +
    geom_col(aes(y = total_cases), fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = cumulative_cases / 50), color = "red", size = 1) +
    scale_y_continuous(
        name = "Daily Cases",
        sec.axis = sec_axis(~ . * 50, name = "Cumulative Cases")
    ) +
    labs(
        title = "Daily and Cumulative Confirmed COVID-19 Cases",
        subtitle = "Ontario schools data",
        x = "Date",
        caption = "Note: Cumulative cases scaled down by 50 for plotting"
    ) +
    theme_minimal()

# print(p5) # <- This ensures the plot shows up in VS Code
# ggsave("Cumulative and daily COVID-19 Cases in Ontario Schools.jpg", plot = p5, width = 10, height = 6)
week_cases <- daily_cases %>%
    mutate(week = lubridate::week(reported_date)) %>%
    group_by(week) %>%
    summarise(weekly_cases = sum(total_cases))
p6 <- ggplot(week_cases, aes(x = week, y = weekly_cases)) +
    geom_line(color = "darkgreen") +
    labs(
        title = "Weekly COVID-19 Cases in Ontario Schools",
        x = "Week Number",
        y = "Weekly Cases"
    ) +
    theme_minimal()
# print(p6) # <- This ensures the plot shows up in VS Code
# ggsave("Weekly COVID-19 Cases in Ontario Schools.jpg", plot = p6, width = 10, height = 6)
peaks <- findpeaks(daily_cases$total_cases,
    nups = 1,
    ndowns = 1,
    zero = "+",
    sortstr = FALSE,
    minpeakheight = 500,
    minpeakdistance = 5
)

# Get the dates where peaks occur
peak_indexes <- peaks[, 2]
peak_data <- daily_cases[peak_indexes, ]

# Plot the raw daily cases with peaks marked
p7 <- ggplot(daily_cases, aes(x = reported_date, y = total_cases)) +
    geom_col(fill = "skyblue", alpha = 0.6) +
    geom_point(
        data = peak_data, aes(x = reported_date, y = total_cases),
        color = "red", size = 2
    ) +
    labs(
        title = "Daily COVID-19 Cases with Raw Peaks",
        x = "Date", y = "Total Daily Cases"
    ) +
    theme_minimal()

# print(p7) # <- This ensures the plot shows up in VS Code
# ggsave("Daily COVID-19 Cases with Raw Peaks.jpg", plot = p7, width = 10, height = 6)

top_municipalities <- school_data %>%
    group_by(municipality) %>%
    summarise(total_cases = sum(total_confirmed_cases, na.rm = TRUE)) %>%
    arrange(desc(total_cases)) %>%
    slice(1:10)

p8 <- ggplot(top_municipalities, aes(x = reorder(municipality, total_cases), y = total_cases)) +
    geom_col(fill = "tomato") +
    coord_flip() +
    labs(
        title = "Top 10 Municipalities by Total School COVID Cases",
        x = "Municipality", y = "Total Cases"
    ) +
    theme_minimal()
# print(p8) # <- This ensures the plot shows up in VS Code
# ggsave("Top 10 Municipalities by Total School COVID Cases.jpg", plot = p8, width = 10, height = 6)
school_data <- school_data %>%
    mutate(weekday = wday(reported_date, label = TRUE))

weekday_cases <- school_data %>%
    group_by(weekday) %>%
    summarise(total_cases = sum(total_confirmed_cases, na.rm = TRUE))

p9 <- ggplot(weekday_cases, aes(x = weekday, y = total_cases)) +
    geom_col(fill = "purple") +
    labs(title = "Total School COVID Cases by Day of Week") +
    theme_minimal()
print(p9)
ggsave("Total School COVID Cases by Day of Week.jpg", plot = p9, width = 10, height = 6)
