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

# add fonts --------------------------------------------------------------------

font_add(family = "HelveticaCompressed", regular = "/Users/mine/Library/Fonts/HelveticaComp.ttf")
font_add(family = "HelveticaNeue85Heavy", regular = "/Users/mine/Library/Fonts/Helvetica-Neue-LT-Std-85-Heavy_22545.ttf")

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

sun_stuff_today <- suncalc::getSunlightTimes(date = Sys.Date(), lat = 55.9533, lon = -3.1883)
sunrise_today <- format(sun_stuff_today$sunrise, "%H:%M")
sunset_today <- format(sun_stuff_today$sunset, "%H:%M")

sun_stuff_yesterday <- suncalc::getSunlightTimes(date = Sys.Date() - 1, lat = 55.9533, lon = -3.1883)
sun_out_today <- sun_stuff_today$sunset - sun_stuff_today$sunrise
sun_out_yesterday <- sun_stuff_yesterday$sunset - sun_stuff_yesterday$sunrise
sun_out_diff <- as.numeric(sun_out_today - sun_out_yesterday) * 60
day_change_length <- round(abs(sun_out_diff))
day_change_direction <- if_else(sign(sun_out_diff) == 1, "more", "less")
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

time_local <- glue("{hour(now)}:{minute(now)}")
time_TR <- glue("{hour(now)+3}:{minute(now)}") # need better tz conversion
time_FR <- glue("{hour(now)+1}:{minute(now)}") # need better tz conversion
time_ET <- glue("{hour(now)-5}:{minute(now)}") # need better tz conversion
time_PT <- glue("{hour(now)-8}:{minute(now)}") # need better tz conversion


significant_day <- significant_days %>%
  filter(
    month == month_no,
    day   == day_of_month
  ) %>%
  pull(what)

birthday <- birthdays %>%
  filter(
    month == month_no,
    day   == day_of_month
  ) %>%
  pull(who)
birthday_text <- glue("Birthday: {birthday}")

chance_of_precipitation <- 0.8

# bottom portion inputs --------------------------------------------------------

function_pkg <- "dplyr"
function_name <- "summarise"
function_title <- "Summarise each group to fewer rows"
function_text <- glue("{function_pkg}::{function_name}()\n{function_title}")

# plot -------------------------------------------------------------------------

showtext_auto()

ggplot() +
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
  geom_text(aes(x = 0, y = 0), label = function_text, hjust = "left", vjust = "bottom", size = 5) +
  # theme
  theme(
    plot.background = element_rect(color = "black"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  draw_image("img/icons8-sunrise-80.png", x = 0.87, y = 0.98, width = 0.07, height = 0.07, hjust = 1, vjust = 0) +
  draw_image("img/icons8-sunset-80.png", x = 0.87, y = 0.92, width = 0.07, height = 0.07, hjust = 1, vjust = 0)


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

ggsave(device = "bmp", filename = "saatli-maarif.bmp",
       height = 8.8, width = 5.28, dpi = 100,
       antialias = "none")
