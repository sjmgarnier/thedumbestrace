#### ENVIRONMENT ####
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(zoo)
library(lubridate)
library(tweenr)
library(ggplot2)
library(scales)
library(gganimate)


#### CUSTOM FUNCTIONS ####
my_tween_appear <- function (data, time, timerange, nframes) {
  if (missing(timerange) || is.null(timerange)) {
    timerange <- range(data[[time]])
  }
  if (missing(nframes) || is.null(nframes)) {
    nframes <- ceiling(diff(timerange) + 1)
  }
  framelength <- diff(timerange)/(nframes - 1)
  basetime <- min(timerange)
  tweendata <- lapply(seq_len(nframes) - 1, function(f) {
    timepoint <- f * framelength + basetime
    data$.age <- timepoint - data[[time]]
    data$.frame <- f
    data
  })
  tweendata <- do.call(rbind, tweendata)
  attr(tweendata, "framelength") <- framelength
  tweendata
}

my_tween <- function(data, cols, time, timerange, nframes) {
  if (missing(timerange) || is.null(timerange)) {
    timerange <- range(data[[time]])
  }

  if (missing(nframes) || is.null(nframes)) {
    nframes <- ceiling(diff(timerange) + 1)
  }

  tw <- filter(my_tween_appear(data, time, timerange = timerange, nframes = nframes), .age >= 0)

  ap <- bind_cols(lapply(cols, function(y) {
    setNames(data.frame(approx(data[[y]], n = nframes)$y), y)
  }))
  ap[".frame"] <- 1:nrow(ap)

  tw[cols] <- NULL

  left_join(tw, ap, by = ".frame")
}


#### DATA ####
base_path <- "https://raw.githubusercontent.com/openZH/covid_19/master/fallzahlen_kanton_total_csv_v2/COVID19_Fallzahlen"

cantons <- read_csv("scripts/CH_cantons.csv") %>%
  mutate(., Path = paste0(base_path, Prefix, Abbreviation, "_total.csv"))

ch_data <- cantons %>%
  group_by(., Name, Population) %>%
  do(., read_csv(.$Path, col_types = cols()) %>%
       group_by(., abbreviation_canton_and_fl) %>%
       complete(., abbreviation_canton_and_fl, date = seq.Date(as.Date("2020-01-24"), today(), by = "day")) %>%
       ungroup(.)) %>%
  ungroup(.) %>%
  rename(., Date = date,
         Count = ncumul_conf,
         Province_State = Name) %>%
  select(., Date, Province_State, Population, Count) %>%
  group_by(., Province_State) %>%
  arrange(., Date, .by_group = TRUE) %>%
  mutate(., Count = na.locf(na.locf(Count, na.rm = FALSE), fromLast = TRUE)) %>%
  ungroup(.) %>%
  mutate(., Percent = Count / (Population * 1000),
         Day = 1 + as.numeric(Date - min(Date))) %>%
  group_by(., Province_State) %>%
  arrange(., Date, .by_group = TRUE) %>%
  mutate(., Daily = diff(c(0, Count)),
         DailyPercent = diff(c(0, Percent)),
         Daily7 = rollmean(c(0, 0, 0, 0, 0, 0, Daily), k = 7, align = "center"),
         Daily7Percent = rollmean(c(0, 0, 0, 0, 0, 0, DailyPercent), k = 7, align = "center")) %>%
  ungroup(.) %>%
  group_by(., Date) %>%
  arrange(., Count, Province_State) %>%
  mutate(., Rank = as.numeric(1:n())) %>%
  arrange(., Percent, Province_State) %>%
  mutate(., RankPercent = as.numeric(1:n())) %>%
  ungroup(.)


#### CH COUNT ANIMATION ####
n_frames <- max(ch_data$Day) * 3

ch_bar_data <- ch_data %>%
  group_by(., Province_State) %>%
  do(., my_tween(., c("Rank", "RankPercent"), "Day", nframes = n_frames)) %>%
  ungroup(.)

ch_label_data <- ch_bar_data %>%
  group_by(., .frame) %>%
  filter(., Date == max(Date)) %>%
  ungroup(.)

graph <- ggplot() +
  geom_tile(data = ch_bar_data, aes(x = Rank, y = Count - Daily / 2, height = Daily,
                                    fill = Daily7), width = 0.75) +
  geom_text(data = ch_label_data,
            aes(x = Rank, y = Count + 0.01 * max(ch_data$Count), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: openZH",
       title = "Total cases (top 20)",
       subtitle = "{ format(as.Date(ch_label_data$Date[ch_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(ch_data$Rank) - 18.75, max(ch_data$Rank))) +
  scale_x_continuous(breaks = 1:27, labels = 27:1) +
  scale_y_continuous(labels = comma, limits = range(ch_bar_data$Count) * c(1, 1.1)) +
  scale_fill_viridis_c(option = "inferno", begin = 0, end = 0.75, labels = comma) +
  guides(fill = guide_colorbar(title = "Daily cases (7-day average)",
                               title.position = "top",
                               barwidth = unit(300, "points"),
                               barheight = unit(12, "points"),
                               title.hjust = 1)) +
  theme_light(base_size = 16 * 1.5, base_family = "Raleway") +
  theme(legend.position = c(1, 1), legend.direction = "horizontal", legend.justification = c(1, 0),
        legend.title = element_text(size = 10 * 1.5, face = "bold", color = "#ffffff"),
        legend.text = element_text(size = 8 * 1.5, color = "#ffffff"),
        legend.background = element_rect(fill = "#000000",
                                         colour = "#000000",
                                         size = 0, linetype = "solid"),
        plot.title = element_text(face = "bold", color = "#ffffff"),
        plot.subtitle = element_text(face = "bold", color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff"),
        axis.text = element_text(face = "bold", color = "#ffffff"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#000000",
                                        colour = "#000000",
                                        size = 0, linetype = "solid"),
        plot.background = element_rect(fill = "#000000",
                                       colour = "#000000",
                                       size = 0, linetype = "solid")) +
  transition_manual(.frame)

anim <- animate(graph, n_frames, 30, width = 1080, height = 720, bg = "#f5f5f5",
                end_pause = 60, renderer = ffmpeg_renderer(format = "mp4"))

anim_save("race1_ch.mp4", anim, "web/media")


#### CH PERCENT ANIMATION ####
graph <- ggplot() +
  geom_tile(data = ch_bar_data, aes(x = RankPercent, y = Percent - DailyPercent / 2, height = DailyPercent,
                                    fill = Daily7Percent), width = 0.75) +
  geom_text(data = ch_label_data,
            aes(x = RankPercent, y = Percent + 0.01 * max(ch_data$Percent), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: openZH",
       title = "Percent population infected (top 20)",
       subtitle = "{ format(as.Date(ch_label_data$Date[ch_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(ch_data$RankPercent) - 18.75, max(ch_data$RankPercent))) +
  scale_x_continuous(breaks = 1:27, labels = 27:1) +
  scale_y_continuous(labels = percent, limits = range(ch_bar_data$Percent) * c(1, 1.1)) +
  scale_fill_viridis_c(option = "inferno", begin = 0, end = 0.75, labels = percent) +
  guides(fill = guide_colorbar(title = "Daily cases (% of population)",
                               title.position = "top",
                               barwidth = unit(300, "points"),
                               barheight = unit(12, "points"),
                               title.hjust = 1)) +
  theme_light(base_size = 16 * 1.5, base_family = "Raleway") +
  theme(legend.position = c(1, 1), legend.direction = "horizontal", legend.justification = c(1, 0),
        legend.title = element_text(size = 10 * 1.5, face = "bold", color = "#ffffff"),
        legend.text = element_text(size = 8 * 1.5, color = "#ffffff"),
        legend.background = element_rect(fill = "#000000",
                                         colour = "#000000",
                                         size = 0, linetype = "solid"),
        plot.title = element_text(face = "bold", color = "#ffffff"),
        plot.subtitle = element_text(face = "bold", color = "#ffffff"),
        plot.caption = element_text(color = "#ffffff"),
        axis.text = element_text(face = "bold", color = "#ffffff"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#000000",
                                        colour = "#000000",
                                        size = 0, linetype = "solid"),
        plot.background = element_rect(fill = "#000000",
                                       colour = "#000000",
                                       size = 0, linetype = "solid")) +
  transition_manual(.frame)

anim <- animate(graph, n_frames, 30, width = 1080, height = 720, bg = "#f5f5f5",
                end_pause = 60, renderer = ffmpeg_renderer(format = "mp4"))

anim_save("race2_ch.mp4", anim, "web/media")
