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
base_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"

stats <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

us_data <- read_csv(paste0(base_path, "confirmed_US.csv")) %>%
  pivot_longer(., cols = matches("[0-9]+/[0-9]+/[0-9]+"),
               names_to = "Date", values_to = "Count") %>%
  mutate(., Date = mdy(Date))

us_states_data <- us_data %>%
  filter(., !is.na(Admin2)) %>%
  group_by(., Date, Province_State) %>%
  summarize(., Count = sum(Count, na.rm = TRUE)) %>%
  ungroup(.) %>%
  left_join(., filter(stats, as.numeric(FIPS) %in% 1:78), by = "Province_State") %>%
  mutate(., Percent = Count / Population,
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

world_data <- read_csv(paste0(base_path, "confirmed_global.csv")) %>%
  pivot_longer(., cols = matches("[0-9]+/[0-9]+/[0-9]+"),
               names_to = "Date", values_to = "Count") %>%
  mutate(., Date = mdy(Date)) %>%
  rename(., Province_State = `Province/State`,
         Country_Region = `Country/Region`)

world_countries_data <- world_data %>%
  group_by(., Date, Country_Region) %>%
  summarize(., Count = sum(Count, na.rm = TRUE)) %>%
  ungroup(.) %>%
  left_join(., filter(stats, Country_Region == Combined_Key), by = "Country_Region") %>%
  filter(., !is.na(iso3)) %>%
  mutate(., Percent = Count / Population,
         Day = 1 + as.numeric(Date - min(Date))) %>%
  group_by(., Country_Region) %>%
  arrange(., Date, .by_group = TRUE) %>%
  mutate(., Daily = diff(c(0, Count)),
         DailyPercent = diff(c(0, Percent)),
         Daily7 = rollmean(c(0, 0, 0, 0, 0, 0, Daily), k = 7, align = "center"),
         Daily7Percent = rollmean(c(0, 0, 0, 0, 0, 0, DailyPercent), k = 7, align = "center")) %>%
  ungroup(.) %>%
  group_by(., Date) %>%
  arrange(., Count, Country_Region) %>%
  mutate(., Rank = as.numeric(1:n())) %>%
  arrange(., Percent, Country_Region) %>%
  mutate(., RankPercent = as.numeric(1:n())) %>%
  ungroup(.)


#### US COUNT ANIMATION ####
n_frames <- max(us_states_data$Day) * 3

us_bar_data <- us_states_data %>%
  group_by(., Province_State) %>%
  do(., my_tween(., c("Rank", "RankPercent"), "Day", nframes = n_frames)) %>%
  ungroup(.)

us_label_data <- us_bar_data %>%
  group_by(., .frame) %>%
  filter(., Date == max(Date)) %>%
  ungroup(.)

graph <- ggplot() +
  geom_tile(data = us_bar_data, aes(x = Rank, y = Count - Daily / 2, height = Daily,
                                 fill = Daily7), width = 0.75) +
  geom_text(data = us_label_data,
            aes(x = Rank, y = Count + 0.01 * max(us_states_data$Count), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Johns Hopkins CSSE",
       title = "Total cases (top 20)",
       subtitle = "{ format(as.Date(us_label_data$Date[us_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(us_states_data$Rank) - 18.75, max(us_states_data$Rank))) +
  scale_x_continuous(breaks = 1:52, labels = 52:1) +
  scale_y_continuous(labels = comma, limits = range(us_bar_data$Count) * c(1, 1.1)) +
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

anim_save("race1_usa.mp4", anim, "web/media/")


#### US PERCENT ANIMATION ####
graph <- ggplot() +
  geom_tile(data = us_bar_data, aes(x = RankPercent, y = Percent - DailyPercent / 2, height = DailyPercent,
                                 fill = Daily7Percent), width = 0.75) +
  geom_text(data = us_label_data,
            aes(x = RankPercent, y = Percent + 0.01 * max(us_states_data$Percent), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Johns Hopkins CSSE",
       title = "Percent population infected (top 20)",
       subtitle = "{ format(as.Date(us_label_data$Date[us_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(us_states_data$RankPercent) - 18.75, max(us_states_data$RankPercent))) +
  scale_x_continuous(breaks = 1:52, labels = 52:1) +
  scale_y_continuous(labels = percent, limits = range(us_bar_data$Percent) * c(1, 1.1)) +
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

anim_save("race2_usa.mp4", anim, "web/media/")


#### WORLD COUNT ANIMATION ####
n_frames <- max(world_countries_data$Day) * 3

world_bar_data <- world_countries_data %>%
  group_by(., Country_Region) %>%
  do(., my_tween(., c("Rank", "RankPercent"), "Day", nframes = n_frames)) %>%
  ungroup(.)

world_label_data <- world_bar_data %>%
  group_by(., .frame) %>%
  filter(., Date == max(Date)) %>%
  ungroup(.)

graph <- ggplot() +
  geom_tile(data = world_bar_data, aes(x = Rank, y = Count - Daily / 2, height = Daily,
                                 fill = Daily7), width = 0.75) +
  geom_text(data = world_label_data,
            aes(x = Rank, y = Count + 0.01 * max(world_countries_data$Count), label = Country_Region),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Johns Hopkins CSSE",
       title = "Total cases (top 20)",
       subtitle = "{ format(as.Date(world_label_data$Date[world_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(world_countries_data$Rank) - 18.75, max(world_countries_data$Rank))) +
  scale_x_continuous(breaks = 1:186, labels = 188:1) +
  scale_y_continuous(labels = comma, limits = range(world_bar_data$Count) * c(1, 1.1)) +
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

anim_save("race1_world.mp4", anim, "web/media/")


#### WORLD PERCENT ANIMATION ####
graph <- ggplot() +
  geom_tile(data = world_bar_data, aes(x = RankPercent, y = Percent - DailyPercent / 2, height = DailyPercent,
                                 fill = Daily7Percent), width = 0.75) +
  geom_text(data = world_label_data,
            aes(x = RankPercent, y = Percent + 0.01 * max(world_countries_data$Percent), label = Country_Region),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Johns Hopkins CSSE",
       title = "Percent population infected (top 20)",
       subtitle = "{ format(as.Date(world_label_data$Date[world_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(world_countries_data$RankPercent) - 18.75, max(world_countries_data$RankPercent))) +
  scale_x_continuous(breaks = 1:186, labels = 188:1) +
  scale_y_continuous(labels = percent, limits = range(world_bar_data$Percent) * c(1, 1.1)) +
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

anim_save("race2_world.mp4", anim, "web/media/")
