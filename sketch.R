# load packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)
library(grid)

# inputs -----------------------------------------------------------------------

now <- Sys.time()

# top
moon_phase <- "crescent"

day_change_length <- 1
day_change_direction <- "longer"
day_change_text <- glue("Day gets\n{day_change_direction} by\n{day_change_length} minute")

sunrise <- "08:44"
sunrise_text <- glue("Sunrise: {sunrise}")
sunset <- "15:43"
sunset_text <- glue("Sunset: {sunset}")


# middle
year  <- year(now)
year_text <- glue("Year: {year}")
month_no <- month(now)
month_text <- glue("Month: {month_no}")
day_of_year <- yday(now)
day_of_year_text <- glue("Day: {day_of_year}")

month_name <- toupper(as.character(month(now, label = TRUE, abbr = FALSE)))

day_of_month <- day(now)
time_local <- glue("{hour(now)}:{minute(now)}")
time_TR <- glue("{hour(now)+3}:{minute(now)}") # need better tz conversion
time_FR <- glue("{hour(now)+1}:{minute(now)}") # need better tz conversion
time_ET <- glue("{hour(now)-5}:{minute(now)}") # need better tz conversion
time_PT <- glue("{hour(now)-8}:{minute(now)}") # need better tz conversion

day_name <- toupper(as.character(wday(now, label = TRUE, abbr = FALSE)))
holiday <- "Boxing Day"
birthday <- "First Last"
birthday_text <- glue("🎈{birthday}")

chance_of_precipitation <- 0.8

function_pkg <- "dplyr"
function_name <- "summarise"
function_title <- "Summarise each group to fewer rows"
function_text <- glue("{function_pkg}::{function_name}()\n{function_title}")

# plot -------------------------------------------------------------------------

ggplot() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_hline(yintercept = 0.9) +
  geom_hline(yintercept = 0.1) +
  # top
  geom_text(aes(x = 0.0, y = 0.98), label = moon_phase, hjust = "left") +
  geom_text(aes(x = 0.5, y = 0.98), label = day_change_text, hjust = "center") +
  geom_text(aes(x = 1, y = 1), label = sunrise_text, hjust = "right") +
  geom_text(aes(x = 1, y = 0.94), label = sunset_text, hjust = "right") +
  # middle
  geom_text(aes(x = 0.0, y = 0.88), label = year_text, hjust = "left") +
  geom_text(aes(x = 0.33, y = 0.88), label = month_text, hjust = "center") +
  geom_text(aes(x = 0.66, y = 0.88), label = day_of_year_text, hjust = "center") +
  geom_text(aes(x = 1, y = 0.88), label = "???", hjust = "right") +
  geom_text(aes(x = 0.5, y = 0.8), label = month_name, hjust = "center", vjust = "center", size = 12) +
  geom_text(aes(x = 0.2, y = 0.55), label = day_of_month, hjust = "center", vjust = "center", size = 50) +
  geom_text(aes(x = 0.5, y = 0.22), label = day_name, hjust = "center", vjust = "bottom", size = 10) +
  geom_text(aes(x = 0.5, y = 0.17), label = holiday, hjust = "center", vjust = "bottom") +
  geom_text(aes(x = 0.5, y = 0.12), label = birthday_text, hjust = "center", vjust = "bottom") +
  # bottom
  geom_text(aes(x = 0, y = 0), label = function_text, hjust = "left", vjust = "bottom") +
  # theme
  theme(
    plot.background = element_rect(color = "black"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# switch out background grob
# https://stackoverflow.com/questions/48199791/rounded-corners-in-ggplot2
g <- ggplotGrob(p)
bg <- g$grobs[[1]]
round_bg <- grid::roundrectGrob(
  x = bg$x, y = bg$y, width = bg$width, height = bg$height,
  r = unit(0.1, "snpc"),
  just = bg$just, name = bg$name, gp = bg$gp, vp = bg$vp
)
g$grobs[[1]] <- round_bg
plot(g)


#FONT: TEKO MEDIUM 500 - https://fonts.google.com/specimen/Teko?category=Sans+Serif,Display&thickness=5&width=2&preview.text=PERSEMBE%206%20SUBAT&preview.text_type=custom