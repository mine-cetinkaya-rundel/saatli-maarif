# load packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)
library(grid)
library(suncalc)
library(cowplot)
library(gggibbous)
library(magick)
library(showtext)
library(grid)

# add fonts --------------------------------------------------------------------

font_add(family = "HelveticaCompressed", regular = "fonts/HelveticaComp.ttf")
font_add(family = "HelveticaNeue85Heavy", regular = "fonts/Helvetica-Neue-LT-Std-85-Heavy_22545.ttf")

# load data --------------------------------------------------------------------

significant_days <- read_csv("significant-days.csv")
birthdays <- read_csv("birthdays.csv")

# top portion inputs -----------------------------------------------------------

moon_stuff_today <- suncalc::getMoonIllumination(date = Sys.Date())
moon_ratio <- moon_stuff_today$fraction
moon_phase_value <- moon_stuff_today$phase
moon_phase_text <- case_when(
  moon_phase_value == 0                            ~ "New Moon",
  moon_phase_value > 0 & moon_phase_value < 0.25   ~ "Waxing Crescent",
  moon_phase_value == 0.25                         ~ "First Quarter",
  moon_phase_value > 0.25 & moon_phase_value < 0.5 ~ "Waxing Gibbous",
  moon_phase_value == 0.25                         ~ "Full Moon",
  moon_phase_value > 0.5 & moon_phase_value < 0.75 ~ "Waning Gibbous",
  moon_phase_value == 0.75                         ~ "Last Quarter",
  moon_phase_value > 0.75                          ~ "Waning Crescent"
)
moon_phase_text <- str_replace(moon_phase_text, " ", "\n")

lat_durham <- 35.9940
lon_durham <- -78.8986
sun_stuff_today <- suncalc::getSunlightTimes(date = Sys.Date(), lat = lat_durham, lon = lon_durham, tz = "EST")
sunrise_today <- format(sun_stuff_today$sunrise, "%H:%M")
sunset_today <- format(sun_stuff_today$sunset, "%H:%M")

sun_stuff_yesterday <- suncalc::getSunlightTimes(date = Sys.Date() - 1, lat = lat_durham, lon = lon_durham, tz = "EST")
sun_out_today <- sun_stuff_today$sunset - sun_stuff_today$sunrise
sun_out_yesterday <- sun_stuff_yesterday$sunset - sun_stuff_yesterday$sunrise
sun_out_diff <- as.numeric(sun_out_today - sun_out_yesterday) * 60
day_change_length <- round(abs(sun_out_diff))
day_change_direction <- if_else(sign(sun_out_diff) == 1, "more", "fewer")
day_change_minute_pluralization <- if_else(day_change_length == 1, "minute", "minutes")
day_change_text <- glue("{day_change_length} {day_change_direction} {day_change_minute_pluralization}\nof daylight")

# middle portion inputs --------------------------------------------------------

now <- now()
today <- today()

year  <- year(today)
year_text <- glue("Year: {year}")
month_no <- month(today)
month_text <- glue("Month: {month_no}")
day_of_year <- yday(today)
day_of_year_text <- glue("Day: {day_of_year}")
year_days <- if_else(leap_year(2020), 366, 365)
elapsed <- round(day_of_year / year_days, 2) * 100
elapsed_text <- glue("Elapsed: {elapsed}%")

month_name <- toupper(as.character(month(today, label = TRUE, abbr = FALSE)))
day_of_month <- day(today)
day_name <- toupper(as.character(wday(today, label = TRUE, abbr = FALSE)))

significant_day <- significant_days %>%
  filter(
    month == month_no,
    day   == day_of_month
  ) %>%
  pull(what)

if(length(significant_day) == 0){
  significant_day = ""
}

birthday <- birthdays %>%
  filter(
    month == month_no,
    day   == day_of_month
  ) %>%
  pull(who)

if(length(birthday) == 0){
  birthday_text = "No birthdays today"
} else {
  birthday_text <- glue("Birthday: {birthday}")
}

# clocks -----------------------------------------------------------------------

draw_clock <- function(tzone = "America/New_York", city = "Durham"){

  # create tibble
  minutes <- tibble(x = 0:60, y = 1)
  hours <- filter(minutes, x %% 5 == 0)

  # determine now
  now <- now(tzone = tzone)

  # find time now
  min_now <- minute(now)

  if(hour(now) >= 12){
    hour_now <- (hour(now) - 12)*5 + min_now/60*5
  } else {
    hour_now <- hour(now)*5 + min_now/60*5
  }

  ggplot() +
    geom_point(data = minutes, aes(x = x, y = y), size = 2) +
    geom_point(data = hours, aes(x = x, y = y),
               size = 4, show.legend = FALSE) +
    geom_point(aes(x = 0, y = 0), size = 4) +
    coord_polar() +
    expand_limits(y = c(0, 1)) +
    theme_void() +
    theme(
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
    ) +
    geom_segment(aes(x = hour_now, xend = hour_now, y = 0, yend = 0.6), size = 2) +
    geom_segment(aes(x = min_now, xend = min_now, y = 0, yend = 0.9), size = 2) +
    annotate(geom = "text", x = 30, y = 0.5, label = city, size = 18)
}

clock_local <- draw_clock()
clock_IST <- draw_clock(tzone = "Europe/Istanbul", city = "Istanbul")
clock_PAR <- draw_clock(tzone = "Europe/Paris", city = "Paris")
clock_LAX <- draw_clock(tzone = "America/Los_Angeles", city = "Los Angeles")

ggsave(clock_local, filename = "img/clock_local.png")
ggsave(clock_IST, filename = "img/clock_IST.png")
ggsave(clock_PAR, filename = "img/clock_PAR.png")
ggsave(clock_LAX, filename = "img/clock_LAX.png")

# bottom portion inputs --------------------------------------------------------

funs <- tribble(
  ~pkg, ~fun, ~description,
  "dplyr", "summarise", "Summarise each group to fewer rows",
  "tidyr", "hoist", "Rectangle a nested list into a tidy tibble",
  "ggplot2", "geom_spoke", "Line segments parameterised by location, direction and distance"
)

function_text <- funs %>%
  slice_sample(n = 1) %>%
  mutate(text = glue("{pkg}::{fun}()\n{description}")) %>%
  pull(text)

# plot -------------------------------------------------------------------------

showtext_auto()

p <- ggplot() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_hline(yintercept = 0.90) +
  geom_hline(yintercept = 0.12) +
  # top
  geom_text(aes(x = 0.0, y = 0.98), label = moon_phase_text, hjust = "left", size = 5) +
  geom_moon(aes(x = 0.2, y = 0.98, ratio = 1), fill = "black") +
  geom_moon(aes(x = 0.2, y = 0.98, ratio = moon_ratio)) +
  geom_text(aes(x = 0.5, y = 0.98), label = day_change_text, hjust = "center", size = 5) +
  geom_text(aes(x = 1, y = 1), label = sunrise_today, hjust = "right", size = 5) +
  geom_text(aes(x = 1, y = 0.94), label = sunset_today, hjust = "right", size = 5) +
  # middle
  geom_text(aes(x = 0.0, y = 0.86), label = year_text, hjust = "left", size = 4) +
  geom_text(aes(x = 0.35, y = 0.86), label = month_text, hjust = "center", size = 4) +
  geom_text(aes(x = 0.6, y = 0.86), label = day_of_year_text, hjust = "center", size = 4) +
  geom_text(aes(x = 1, y = 0.86), label = elapsed_text, hjust = "right", size = 4) +
  geom_text(aes(x = 0.5, y = 0.78), label = month_name, hjust = "center", vjust = "center", size = 12, family = "HelveticaNeue85Heavy") +
  geom_text(aes(x = 0.2, y = 0.55), label = day_of_month, hjust = "center", vjust = "center", size = 70, family = "HelveticaCompressed") +
  geom_text(aes(x = 0.5, y = 0.27), label = day_name, hjust = "center", vjust = "bottom", size = 10, family = "HelveticaNeue85Heavy") +
  geom_text(aes(x = 0.5, y = 0.21), label = significant_day, hjust = "center", vjust = "bottom", size = 4) +
  geom_text(aes(x = 0.5, y = 0.16), label = birthday_text, hjust = "center", vjust = "bottom", size = 4) +
  # bottom
  geom_text(aes(x = 0, y = 0), label = function_text, hjust = "left", vjust = "bottom", size = 4) +
  # theme
  theme(
    plot.background = element_rect(color = "black"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  draw_image("img/icons8-sunrise-80.png", x = 0.87, y = 0.98, width = 0.07, height = 0.07, hjust = 1, vjust = 0) +
  draw_image("img/icons8-sunset-80.png", x = 0.87, y = 0.92, width = 0.07, height = 0.07, hjust = 1, vjust = 0) +
  draw_image("img/clock_local.png", x = 0.5, y = 0.52, width = 0.22, height = 0.22) +
  draw_image("img/clock_IST.png", x = 0.7, y = 0.52, width = 0.22, height = 0.22) +
  draw_image("img/clock_PAR.png", x = 0.5, y = 0.32, width = 0.22, height = 0.22) +
  draw_image("img/clock_LAX.png", x = 0.7, y = 0.32, width = 0.22, height = 0.22)


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

#FONT: TEKO MEDIUM 500 - https://fonts.google.com/specimen/Teko?category=Sans+Serif,Display&thickness=5&width=2&preview.text=PERSEMBE%206%20SUBAT&preview.text_type=custom

ggsave(plot = g, device = "bmp", filename = "saatli-maarif.bmp",
       height = 8.8, width = 5.28, dpi = 100,
       antialias = "none")
