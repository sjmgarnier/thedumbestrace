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
departements <- read_csv("scripts/FR_departements.csv") %>%
  rename(., Province_State = Nom)

old <- read_csv2("https://www.data.gouv.fr/fr/datasets/r/b4ea7b4b-b7d1-4885-a099-71852291ff20") %>%
  filter(., clage_covid == "0") %>%
  select(., dep, jour, nb_pos) %>%
  group_by(., dep, jour) %>%
  summarize(., Count = sum(nb_pos))
new <- read_csv2("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675") %>%
  select(., dep, jour, P) %>%
  group_by(., dep, jour) %>%
  summarize(., Count = sum(P))

fr_data <- bind_rows(filter(old, jour < min(new$jour)), new) %>%
  group_by(., dep) %>%
  arrange(., jour, .by_group = TRUE) %>%
  mutate(., Count = cumsum(Count)) %>%
  ungroup(.) %>%
  rename(., Date = jour,
         Code = dep) %>%
  group_by(., Code) %>%
  complete(., Code, Date = seq.Date(as.Date("2020-03-10"), today(), by = "day")) %>%
  arrange(., Date, .by_group = TRUE) %>%
  mutate(., Count = na.locf(na.locf(Count, na.rm = FALSE), fromLast = TRUE)) %>%
  ungroup(.) %>%
  full_join(., departements) %>%
  select(., Date, Province_State, Count, Population) %>%
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


#### FR COUNT ANIMATION ####
n_frames <- max(fr_data$Day) * 3

fr_bar_data <- fr_data %>%
  group_by(., Province_State) %>%
  do(., my_tween(., c("Rank", "RankPercent"), "Day", nframes = n_frames)) %>%
  ungroup(.)

fr_label_data <- fr_bar_data %>%
  group_by(., .frame) %>%
  filter(., Date == max(Date)) %>%
  ungroup(.)

graph <- ggplot() +
  geom_tile(data = fr_bar_data, aes(x = Rank, y = Count - Daily / 2, height = Daily,
                                    fill = Daily7), width = 0.75) +
  geom_text(data = fr_label_data,
            aes(x = Rank, y = Count + 0.01 * max(fr_data$Count), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Santé publique France",
       title = "Total positive tests (top 20)",
       subtitle = "{ format(as.Date(fr_label_data$Date[fr_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(fr_data$Rank) - 18.75, max(fr_data$Rank))) +
  scale_x_continuous(breaks = 1:104, labels = 104:1) +
  scale_y_continuous(labels = comma, limits = range(fr_bar_data$Count) * c(1, 1.1)) +
  scale_fill_viridis_c(option = "inferno", begin = 0, end = 0.75, labels = comma) +
  guides(fill = guide_colorbar(title = "Daily positive tests (7-day average)",
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

anim_save("race1_fr.mp4", anim, "web/media")


#### FR PERCENT ANIMATION ####
graph <- ggplot() +
  geom_tile(data = fr_bar_data, aes(x = RankPercent, y = Percent - DailyPercent / 2, height = DailyPercent,
                                    fill = Daily7Percent), width = 0.75) +
  geom_text(data = fr_label_data,
            aes(x = RankPercent, y = Percent + 0.01 * max(fr_data$Percent), label = Province_State),
            hjust = 0, family = "Raleway", fontface = "bold", color = "white", size = 6) +
  labs(x = NULL, y = NULL, caption = "#TheDumbestRace - thedumbestrace.com - Data: Santé publique France",
       title = "Percent positive tests (top 20)",
       subtitle = "{ format(as.Date(fr_label_data$Date[fr_label_data$.frame == as.numeric(current_frame)][1]), '%b %d, %Y') }") +
  coord_flip(xlim = c(max(fr_data$RankPercent) - 18.75, max(fr_data$RankPercent))) +
  scale_x_continuous(breaks = 1:104, labels = 104:1) +
  scale_y_continuous(labels = percent, limits = range(fr_bar_data$Percent) * c(1, 1.1)) +
  scale_fill_viridis_c(option = "inferno", begin = 0, end = 0.75, labels = percent) +
  guides(fill = guide_colorbar(title = "Daily positive tests (% of population)",
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

anim_save("race2_fr.mp4", anim, "web/media")
