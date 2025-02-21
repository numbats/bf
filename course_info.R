######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2025-03-03"

# Week of mid-semester break
mid_semester_break <- "2025-04-21"

# Schedule
schedule <- tibble(
  Week = seq(12),
  Topic = c(
    "Introduction to forecasting and R",
    "Time series graphics",
    "Time series decomposition",
    "The forecaster's toolbox",
    "Exponential smoothing",
    "Exponential smoothing",
    "ARIMA models",
    "ARIMA models",
    "ARIMA models",
    "Multiple regression and forecasting",
    "Dynamic regression",
    "Revision"
  ),
  Chapter = c(
    "1. Getting started",
    "2. Time series graphics",
    "3. Time series decomposition",
    "5. The forecaster's toolbox",
    "8. Exponential smoothing",
    "8. Exponential smoothing",
    "9. ARIMA models",
    "9. ARIMA models",
    "9. ARIMA models",
    "7. Time series regression models",
    "10. Dynamic regression models",
    "Revision"
  ),
  Chapter_URL = c(
    "https://OTexts.com/fpp3/intro.html",
    "https://OTexts.com/fpp3/graphics.html",
    "https://OTexts.com/fpp3/decomposition.html",
    "https://OTexts.com/fpp3/toolbox.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/expsmooth.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/arima.html",
    "https://OTexts.com/fpp3/regression.html",
    "https://OTexts.com/fpp3/dynamic.html",
    ""
  )
)

# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
  ) |>
  mutate(
    Week = row_number(),
    Week = if_else(Date < mid_semester_break, Week, Week - 1),
    #Week =
  )

# Add calendar to schedule
schedule <- schedule |>
  left_join(calendar, by = "Week") |>
  mutate(
    Week = if_else(Date == mid_semester_break, NA, Week),
    Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
    Chapter = if_else(Date == mid_semester_break, NA, Chapter),
    Chapter_URL = if_else(Date == mid_semester_break, NA, Chapter_URL)
  ) |>
  select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
  7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
  mutate(
    Date = lastmon(Due),
    Moodle = paste0(
      "https://learning.monash.edu/mod/",
      c("quiz", rep("assign", 3)), "/view.php?id=", Moodle
    ),
    Moodle3231 = paste0(
      "https://learning.monash.edu/mod/",
      c("quiz", rep("assign", 3)), "/view.php?id=", Moodle3231
    ),
    File = paste0("assignments/", File)
  )


schedule <- schedule |>
  left_join(assignments, by = "Date")

show_assignments <- function(week) {
  ass <- schedule |>
    filter(
      Week >= week,
      Week < week + 3,
      !is.na(Assignment),
    ) |>
    select(Assignment:File)
  if(NROW(ass) > 0) {
    cat("\n\n## Assignments\n\n")
    for(i in seq(NROW(ass))) {
      cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
          format(ass$Due[i], "%A %d %B.\n"), sep="")
    }
  }
}

show_slides <- function(week) {
  file <- paste0("/week", week, "/week", week, ".pdf")
  embed <- paste0(
      "<embed src='",
      file,
      "' type='application/pdf' width='100%' height=465></embed>"
    )
  button <- paste0("<a href=", file, " class='badge badge-small badge-red'>Download pdf</a>")
  cat(paste0("## Slides for seminar\n\n", embed,"\n", button))
}

show_activity <- function(week, title = TRUE, show_solutions = TRUE) {
  today <- Sys.Date()
  monday <- monday <- schedule |>
    filter(Week == week) |>
    pull(Date) |>
    as.Date()
  # Show slides one week ahead
  if ((monday - today) <= 7 | week <= 1) {
    file <- here::here(paste0("week", week, "/activities.qmd"))
    if (fs::file_exists(file)) {
      cat("\n\n## [Workshop activities](activities.qmd)\n\n")
    }
  }
}

#
#
# show_activity <- function(week, title = TRUE) {
#   file <- here::here(paste0("week",week,"/activities.qmd"))
#   if(!fs::file_exists(file)) {
#     file <- here::here(paste0("week",week,"/activities.md"))
#   }
#   activities <- read_file(file)
#   if(title) {
#     cat("\n\n## Seminar activities\n\n")
#   }
#   cat(activities)
#   cat("\n")
# }

submit <- function(schedule, assignment) {
  ass <- schedule  |>
    filter(Assignment == !!assignment)
  due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
  url <- ass$Moodle
  button1 <- paste0("<br><br><hr><b>Due: ", due, "</b><br>",
                   "<a href=",url," class = 'badge badge-large badge-blue'>",
                   "<font size='+2'>&nbsp;&nbsp;<b>Submit (ETF5231)</b>&nbsp;&nbsp;</font><br></a>")
  cat(button1)
  if (str_detect(ass$Assignment, "IA")) {
  url <- ass$Moodle3231
  button2 <- paste0("<br><br><hr><b>Due: ", due, "</b><br>",
                    "<a href=",url," class = 'badge badge-large badge-blue'>",
                    "<font size='+2'>&nbsp;&nbsp;<b>Submit (ETF3231)</b>&nbsp;&nbsp;</font><br></a>")
  cat(button2)
  }
}
