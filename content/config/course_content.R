course_content <-
  tibble::tribble(
    ~Day, ~Time, ~Title,
    "April 23", "10:00-11:30", "Introduction to GIS",
    "April 23", "11:45-13:00", "Vector Data",
    "April 23", "13:00-14:00", "Lunch Break",
    "April 23", "14:00-15:30", "Mapping",
    "April 23", "15:45-17:00", "Raster Data",
    "April 24", "09:00-10:30", "Advanced Data Import & Processing",
    "April 24", "10:45-12:00", "Applied Data Wrangling & Linking",
    "April 24", "12:00-13:00", "Lunch Break",
    "April 24", "13:00-14:30", "Investigating Spatial Autocorrelation",
    "April 24", "14:45-16:00", "Spatial Econometrics & Outlook"
  ) |>
  knitr::kable() |>
  kableExtra::kable_styling() |>
  kableExtra::column_spec(1, color = "gray") |>
  kableExtra::column_spec(2, color = "gray") |>
  kableExtra::column_spec(3, bold = TRUE) |>
  kableExtra::row_spec(3, color = "gray") |>
  kableExtra::row_spec(8, color = "gray") |>
  kableExtra::row_spec(5, extra_css = "border-bottom: 1px solid")
