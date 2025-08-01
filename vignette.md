# Introduction

**EddyEye Pulse Dashboard** is an interactive dashboard designed for
real-time monitoring and visualization of eddy covariance station data,
with a focus on gas fluxes (CO<sub>2</sub>, CH<sub>4</sub>,
N<sub>2</sub>O, H<sub>2</sub>O) and wind parameters.  
This vignette provides a detailed walk-through of the
**EddyEye_Pulse_Dashboard.Rmd** code structure, explaining the
functionality of each code chunk.

------------------------------------------------------------------------

# 1. YAML Header

**Purpose:** Defines the dashboard’s metadata and layout using the
flexdashboard engine.

``` yaml
---
title: "EddyEye Pulse"
output:
  flexdashboard::flex_dashboard:
    navbar:
      - { icon: "ion-pin", title: "EddyEye Hub", href: "https://github.com/Svyatoslav-stack/Eddy-Covariance-Data-Dashboards", align: right }
    theme: united
    orientation: rows
    vertical_layout: scroll
    smooth_scroll: TRUE
    css: css/styles.css
---
      
```

-   **title:** The display name of the dashboard shown at the top.

-   **flexdashboard:** Specifies that the document uses the
    flexdashboard format, which is a powerful framework for building
    interactive dashboards in R Markdown. For more details, check the
    [flexdashboard
    documentation](https://rstudio.github.io/flexdashboard/index.html)
    or the [CRAN
    page](https://cran.r-project.org/web/packages/flexdashboard/index.html).

-   **navbar:** Adds a custom navigation bar at the top of the
    dashboard. You can include icons, titles, and links. In my case, it
    contains a link to another project, but you can customize it however
    you like.

-   **theme:** Sets the visual theme for the dashboard using Bootswatch
    Bootstrap themes. The example here uses “united”, but you can pick
    any theme [available on Bootswatch](https://bootswatch.com) for a
    different look.

-   **orientation, vertical_layout, smooth_scroll:** These settings
    control the layout and scrolling behavior of the dashboard. For
    example, you can arrange sections in rows or columns and choose
    whether the dashboard scrolls smoothly. See the [flexdashboard
    documentation](https://rstudio.github.io/flexdashboard/index.html)
    for more details on these options.

-   **css:** If provided, this option loads a custom CSS file to further
    adjust the appearance of your dashboard. Use it to fine-tune styles,
    fonts, colors, and other visual elements that are not covered by the
    theme.

# 2. Setup Chunk

**Purpose:** Loads all required libraries and sets default chunk
options.

``` r
knitr::opts_chunk$set(error = TRUE)

library(ggplot2)
library(dplyr)
library(flexdashboard)
library(scattermore)
library(pandoc)
library(reticulate)
library(lubridate)
library(scales)
library(knitr)
library(kableExtra)
library(DBI)
library(odbc)
library(glue)
library(yaml)
```

-   **knitr::opts_chunk$set(error = TRUE):** Ensures that errors in code
    chunks will be displayed in the output, which is useful for
    debugging during development.

-   **ggplot2, dplyr:** Data wrangling and plotting.

-   **flexdashboard:** For dashboard structure.

-   **scattermore:** For fast scatterplots (efficient with large
    datasets).

-   **pandoc, reticulate:** Conversion and Python interop (future
    extensions).

-   **lubridate, scales:** Date-time and scale manipulation.

-   **knitr, kableExtra:** Table rendering.

-   **DBI, odbc:** Database connections.

-   **glue, yaml:** String interpolation and YAML parsing.

# 3. Example Station HF Data Handling

**Purpose:** This code connects to the database, gets high-frequency
measurement data for the last 30 minutes and for the current day, and
adjusts the timestamps to the local time zone.

``` r
cfg <- yaml::read_yaml("Your/path/to/file/config.yaml")
db_cfg   <- cfg$db
path_cfg <- cfg$paths
table_hf <- cfg$tables$hf
cols_hf  <- cfg$columns$hf_pulse

con <- dbConnect(
  odbc(),
  .connection_string = glue(
    "Driver={db_cfg$driver};",
    "Server={db_cfg$server};",
    "Database={db_cfg$database};",
    "UID={db_cfg$uid};",
    "PWD={db_cfg$pwd};",
    "TrustServerCertificate={db_cfg$trust_cert}"
  ),
  timeout = 10
)

res          <- dbGetQuery(con, glue(
  "SELECT MAX(TmStamp) AS last_ts FROM {table_hf};"
))
last_ts_utc  <- as.POSIXct(res$last_ts[1],
                           format = "%Y-%m-%d %H:%M:%S",
                           tz     = "UTC")

period_end_utc   <- floor_date(last_ts_utc, unit = "30 minutes")
period_start_utc <- period_end_utc - minutes(30)

is_summer <- dst(with_tz(last_ts_utc, "Europe/Tallinn"))

day_start_utc <- floor_date(last_ts_utc, unit = "day") -
  hours(ifelse(is_summer, 3, 2))
day_end_utc   <- period_end_utc

fmt <- "%Y-%m-%d %H:%M:%S"
ps  <- format(period_start_utc, fmt, tz = "UTC")
pe  <- format(period_end_utc,   fmt, tz = "UTC")
ds  <- format(day_start_utc,    fmt, tz = "UTC")
de  <- format(day_end_utc,      fmt, tz = "UTC")

hf_half_utc <- dbGetQuery(con, glue("
  SELECT {paste(cols_hf, collapse = ', ')}
    FROM {table_hf}
   WHERE TmStamp >= '{ps}'
     AND TmStamp <  '{pe}';
"))

hf_day_utc <- dbGetQuery(con, glue("
  SELECT {paste(cols_hf, collapse = ', ')}
    FROM {table_hf}
   WHERE TmStamp >= '{ds}'
     AND TmStamp <  '{de}';
"))

dbDisconnect(con)

add_dst_hour <- function(ts_utc) {
  local_ts <- with_tz(ts_utc, "Europe/Tallinn")
  ts_utc + hours(ifelse(dst(local_ts), 3, 2))
}

hf_halfhour <- hf_half_utc %>%
  rename(TIMESTAMP = TmStamp) %>%
  mutate(
    TIMESTAMP = {
      ts0 <- as.POSIXct(TIMESTAMP,
                        format = "%Y-%m-%d %H:%M:%S",
                        tz     = "UTC")
      add_dst_hour(ts0)
    }
  )

hf_day <- hf_day_utc %>%
  rename(TIMESTAMP = TmStamp) %>%
  mutate(
    TIMESTAMP = {
      ts0 <- as.POSIXct(TIMESTAMP,
                        format = "%Y-%m-%d %H:%M:%S",
                        tz     = "UTC")
      add_dst_hour(ts0)
    }
  )
```

**Operation:** The code starts by reading your configuration file using
the yaml package. This file includes all the database details (like
server, login, table names, and columns to use). You don’t need to
hard-code these details, just keep them in your config file. **There’s
an example config.yaml file in the repository.**

Using this config, the code builds a database connection with dbConnect
and odbc, using the parameters filled in from your config. It then finds
out the most recent time stamp in your measurement table by running a
SQL query, and converts this value into a POSIX date-time in UTC.

To work with the freshest possible data, the code calculates a 30-minute
window based on the latest available time. It does this by rounding the
latest timestamp down to the nearest half-hour, and setting up a window
from 30 minutes before that up to the end of that half-hour. This way,
you are always looking at a full block of data with no overlap.

The next steps in the script (not shown here) will use this window to
actually pull the data for analysis and visualization, and will also
adjust all timestamps to the local time zone (Europe/Tallinn) so times
are shown correctly for your site.

# 4. Alarm Section

**Purpose:** This code checks the recent data for missing values or
suspiciously repeated values and creates alarm files if problems are
found.

``` r
process_alarm <- function(data, row_threshold, value_threshold, folder_general, folder_oneobs) {
  total_missing_rows <- row_threshold - nrow(data)
  total_na_per_column <- sapply(data, function(col) sum(is.na(col)))
  good_values_per_column <- sapply(total_na_per_column, function(na_count) row_threshold - na_count - total_missing_rows)
  
  alarming_columns_by_missing <- names(data)[good_values_per_column < 16000]
  if (length(alarming_columns_by_missing) > 0) {
    missing_alarm_file_path <- file.path(folder_general, paste0("alarm_", format(Sys.time(), "%d.%m.%Y %H.%M.%S"), ".txt"))
    file.create(missing_alarm_file_path)
    alarming_details <- sapply(alarming_columns_by_missing, function(col) paste0(col, ": ", good_values_per_column[col], " observations"))
    writeLines(alarming_details, missing_alarm_file_path)
  }
  
  alarming_columns_by_value <- names(data)[sapply(names(data), function(col) any(table(data[[col]]) > value_threshold))]
  if (length(alarming_columns_by_value) > 0) {
    value_alarm_file_path <- file.path(folder_oneobs, paste0("alarm_", format(Sys.time(), "%d.%m.%Y %H.%M.%S"), ".txt"))
    file.create(value_alarm_file_path)
    alarming_details_by_value <- sapply(alarming_columns_by_value, function(col) paste0(col, ": ", good_values_per_column[col], " observations"))
    writeLines(alarming_details_by_value, value_alarm_file_path)
  }
}

process_alarm(
  data = hf_halfhour,
  row_threshold = 18000,
  value_threshold = 9000,
  folder_general = path_cfg$alarm_folder_general,
  folder_oneobs = path_cfg$alarm_folder_oneobs
)
```

**Operation:** This code defines the process_alarm function and runs it
on the most recent half-hour of data. The function checks how many
**good** (not missing) values there are in each column and compares this
number to a set threshold. In our setup, we expect 10 measurements per
second, so there should be 18000 values per half-hour. If a column has
too many missing values, the function writes an alarm text file in a
designated folder. The function also checks if any value in a column is
repeated too many times (for example, if a sensor is stuck and always
returns the same number). If this happens, it creates a separate alarm
file in another folder. Each alarm file lists the columns with problems
and the number of good values found. This makes it easy to quickly
notice if your station has stopped sending data or if a particular
sensor is malfunctioning. The original idea was to use Microsoft Power
Automate to monitor these folders and, if an alarm file appears,
automatically send an email with the alarm file attached. This last
automation step is not included in the code.

# 5. Time Parameters

**Purpose:** This code determines when the dashboard was last refreshed
and the time of the most recent data point in the current dataset. It
then displays these times in a summary table.

``` r
last_updated <- format(Sys.time(), "%d %B %Y, %H:%M")

if (nrow(hf_day) > 0) {
  last_dataset_timestamp <- hf_day %>%
    summarise(last_timestamp = max(TIMESTAMP, na.rm = TRUE)) %>%
    pull(last_timestamp)
  
  if (is.na(last_dataset_timestamp)) {
    station_time <- NA
  } else {
    station_time <- format(last_dataset_timestamp, "%d %B %Y, %H:%M")
  }
} else {
  station_time <- NA
}

data <- data.frame(
  Parameter = c("Dashboard Refreshed", "Last Observation Time"),
  Value = c(last_updated, station_time)
)

data %>%
  kable(format = "html", align = "c", col.names = NULL) %>%
  kable_styling(position = "center", full_width = TRUE, bootstrap_options = "striped")
```

**Operation:** This code saves the current system time in the
last_updated variable, which represents when the dashboard was last
generated or refreshed. Then, it checks if there is any data available
for the current day. If there is, it finds the latest timestamp in the
data and saves it as station_time. If there are no data points or the
value is missing, station_time is set to NA. The results are put into a
simple table with two rows: one shows when the dashboard was updated,
and the other shows the time of the latest observation from the
measurement station. This table is rendered in the dashboard so you can
always see how fresh the data is.

# 6. Number of Observations in the Last 30 Minutes

**Purpose:** This code calculates how many valid measurements were
received for each key parameter in the last half hour, and displays this
as a table.

``` r
ideal_total <- 18000
num_rows <- nrow(hf_halfhour)

data_count <- data.frame(
  Parameter = c("CO~2~ Wet Mole Fraction",
                "CO~2~ Dry Mole Fraction",
                "H~2~O Mole Fraction", 
                "CH~4~ Wet Mole Fraction",
                "CH~4~ Dry Mole Fraction",
                "N~2~O Wet Mole Fraction",
                "N~2~O Dry Mole Fraction",
                "Wind Speed X", 
                "Wind Speed Y",
                "Wind Speed Z",
                "Wind Temperature"),
  Present = c(
    sum(!is.na(hf_halfhour$CO2_wet)),
    sum(!is.na(hf_halfhour$CO2_dry)),
    sum(!is.na(hf_halfhour$H2O)),
    sum(!is.na(hf_halfhour$CH4_wet)),
    sum(!is.na(hf_halfhour$CH4_dry)),
    sum(!is.na(hf_halfhour$N2O_wet)),
    sum(!is.na(hf_halfhour$N2O_dry)),
    sum(!is.na(hf_halfhour$WindSpeed_X)),
    sum(!is.na(hf_halfhour$WindSpeed_Y)),
    sum(!is.na(hf_halfhour$WindSpeed_Z)),
    sum(!is.na(hf_halfhour$Wind_Temp))
  )
) %>%
  mutate(
    `Present (%)` = round((Present / ideal_total) * 100, 1),
    Missing = ideal_total - Present,
    `Missing (%)` = round((Missing / ideal_total) * 100, 1)
  )

data_count %>%
  kable(format = "html", align = "c",
        col.names = c("Parameter", "Present (Obs)", "Present (%)", "Missing (Obs)", "Missing (%)")) %>%
  kable_styling(position = "center", full_width = TRUE, bootstrap_options = "striped")
```

**Operation:** This code first sets the expected number of data points
for a half hour (18000, since measurements come in at 10 Hz). For each
main parameter—like CO2 wet/dry mole fraction, H2O, CH4, N2O, wind
speeds, and wind temperature. It counts how many actual (non-missing)
values arrived in the most recent half hour. It then calculates both the
percentage of present data and the percentage of missing data for each
parameter. All this information is combined into a table that shows, for
each parameter, the number of present observations, percent present,
number missing, and percent missing. The table is rendered right on the
dashboard so you can instantly spot if a sensor is not reporting data or
if there are gaps in the last 30 minutes.

# 7. CO<sub>2</sub> Wet and Dry Mole Fraction Dynamics

**Purpose:** This code draws a time series plot for the wet and dry CO₂
mole fractions for the current day.

``` r
p1 = ggplot(data = hf_day, aes(x = TIMESTAMP)) +
  geom_scattermore(aes(y = CO2_wet), pointsize = 1, color = "#276FBF") +
  geom_scattermore(aes(y = CO2_dry), pointsize = 1, color = "maroon") +
  scale_y_continuous(
    name = expression(CO[2]~wet),
    sec.axis = sec_axis(trans = ~ ., name = expression(CO[2]~dry))
  ) +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  labs(title = NULL, x = NULL) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "#276FBF"),
    axis.text.y.left = element_text(color = "#276FBF"),
    axis.ticks.y.left = element_line(color = "#276FBF"),
    axis.title.y.right = element_text(color = "maroon"),
    axis.text.y.right = element_text(color = "maroon"),
    axis.ticks.y.right = element_line(color = "maroon")
  )

p1
```

**Operation:** This code uses ggplot2 to plot the wet and dry CO₂ mole
fractions from the current day’s data. The x-axis is the timestamp, and
the y-axis shows the measured values. Two sets of points are drawn: wet
values are blue, and dry values are maroon. The plot uses a secondary
y-axis for the dry values, but both are scaled the same way for easy
comparison. The x-axis is labeled in hours and minutes, and the axis
labels and ticks use colors that match the points for clarity. The plot
shows the dynamics of both CO₂ measurements throughout the day, making
it easy to spot trends, jumps, or missing periods in the data. All the
other time series plots in this dashboard: H₂O, CH₄, N₂O, wind speeds
(X, Y, Z), and wind temperature are created using the same logic. Each
plot displays the measurement values over time, making it easy to
quickly check for abnormal values, missing data, or sensor malfunctions.

# 8. Clean Environment

**Purpose:** This code removes all objects from the R environment after
the dashboard code has finished running.

``` r
rm(list=ls())
```

**Operation:** This command deletes all variables, functions, and data
frames from the current R session’s memory. In this dashboard it is used
at the end of each dashboard to free up memory and prevent leftover data
from affecting future runs.

# Summary

This dashboard provides a quick and clear way to monitor real-time data
from an eddy covariance station. It automatically loads the latest data
from your database, checks data quality, and visualizes key gas and wind
measurements for the current day. The alert system helps you notice
missing data or potential sensor problems right away, while the summary
tables and time series plots give a full picture of station performance.
The whole process is automated and easy to use for anyone familiar with
R and environmental monitoring data.

# Contacts

If you have questions, suggestions, or want to report an issue, please
visit the project’s [GitHub
page](https://github.com/Svyatoslav-stack/EddyEye-Pulse-Dashboard). You
can also reach me out by [email](svyatoslav.rogozin@ut.ee).
