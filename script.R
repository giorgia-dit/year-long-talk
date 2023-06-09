# LOADING PACKAGES #

install.packages("ggchicklet", repos = "https://cinc.rud.is")

library(tidyverse) 
library(RColorBrewer)
library(sysfonts)
library(showtext)
library(ggsci)
library(lubridate)
library(ggchicklet)
library(png)
library(grid)
library(cowplot)
library(ggnewscale)
library(ggpubr)
library(here)


# LOADING DATA #

df_py <- read.csv(file.choose())

# LOADING UTILITIES #

showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Roboto", "roboto")
font_add_google("Work Sans", "work")


# CLEANING #

df <- df_py %>%
  mutate(Name = as_factor(Name),
         Weekday = as_factor(Weekday),
         Weekday = fct_relevel(Weekday, c(
           "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
         )),
         DateTime = ymd_hms(DateTime, tz = "GMT"),
         DateDay = date(DateTime),
         Month = month(DateTime),
         Week = week(DateTime),
         Hour = hour(DateTime),
         HourCat = fct_shift(as_factor(Hour), 5),
         HourCat = fct_expand(HourCat, "5"),
         HourShift = as.numeric(HourCat, levels = order))

write.csv(df, file=here('df.csv'), row.names = FALSE)


# VISUALIZING #


# BAR PLOT #

bar_df <- df %>%
  filter(Year == 2022) %>%
  group_by(Month, HourShift) %>%
  summarize(Count = n())

xticks_month <- seq(1, 12)
xtext_month <- c("January", "February", "March", "April", "May", "June", "July", 
                  "August", "September", "October", "November", "December")
xtext_month <- sapply(xtext_month, toupper)

ticks_hour <- seq(2, 23, by=5)
text_hour <- c("7AM", "12AM", "5PM", "10PM", "3AM")

barplot <- ggplot(bar_df, aes(Month, Count, fill = HourShift)) +
  geom_col(position = "fill", width = 0.8, alpha = 0.9) +
  scale_fill_gradientn(colours = c("#90CAF9", "#FFAB91", "#FFC107", 
                                   "#D84315", "#C51162", "#1A237E", "#1A237E"), 
                        values = scales::rescale(x = c(1, 5, 7, 13, 15, 19, 23),
                                                 to = c(0,1),
                                                 from = c(1, 23)),
                       breaks = ticks_hour, 
                       labels = text_hour,
                       limits = c(1, 23)
                        ) +
  scale_x_continuous(breaks = xticks_month, labels = xtext_month, trans = "reverse") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    aspect.ratio = 1:1,
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#f7f7f7"),
    legend.background = element_rect(fill = "#f7f7f7"),
    legend.title = element_blank(),
    legend.key.width = unit(0.7, 'cm'), 
    legend.key.height = unit(0.8, 'cm'),
    legend.text = element_text(family = "work", color = "#616161", hjust = 0,
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "work", color = "#BDBDBD", size = 9,
                               margin = unit(c(-15, 0, 0, 0), "pt")),
    axis.text.y = element_text(family = "work", size = 10, color = "#424242", hjust = 1,
                               margin = unit(c(0, -2, 0, 20), "pt"))
  )

ggsave(barplot, filename = here('viz/barplot.png'), width = 15, height = 9.375)


# BUBBLE PLOT

bubble_df <- df %>%
  filter(Year == 2022) %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1),
         DateDay = floor_date(DateTime, unit = "day"),
         DayNum = day(DateDay),
         HourCat = as_factor(ifelse(HourShift > 16, "Night",
                                    ifelse(HourShift > 8, "Afternoon",
                                           "Morning"))),
         HourCat = fct_relevel(HourCat, c("Morning", "Afternoon", "Night"))) %>%
  group_by(Month, DateDay, DayNum, HourCat) %>%
  summarize(Count = n()) %>%
  ungroup()

xticks_daynum <- seq(1,31)
xtext_daynum <- as.character(xticks_daynum)

bubbleplot <- ggplot() +
  geom_segment(data = bubble_df, aes(x = 1, y = Month, xend = 31, yend = Month), color = "#CFD8DC") +
  geom_point(data = bubble_df,
             aes(DayNum, Month, color = HourCat), alpha = 0, size = 8) +
  scale_color_manual(values = c("#FFA726", "#AD1457", "#1A237E")) +
  geom_point(data = bubble_df %>% group_by(Month, DayNum) %>% summarize(Total = sum(Count)),
             aes(DayNum, Month, size = Total), shape = 21, stroke = 1,
             color = "#455A64", fill = "#FAFAFA") +
  scale_size_continuous(range = c(1, 13), limits = c(1, 300)) +
  new_scale("size") +
  geom_point(data = bubble_df %>% filter(HourCat == "Morning"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#FFA726", alpha = 0.5, fill = "#FFA726", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_df %>% filter(HourCat == "Afternoon"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#AD1457", alpha = 0.5, fill = "#AD1457", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_df %>% filter(HourCat == "Night"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#1A237E", alpha = 0.5, fill = "#1A237E", stroke = 0.7,
             show.legend = FALSE) +
  scale_size_continuous(range = c(0.1, 10), limits = c(1, 300)) +
  scale_x_continuous(labels = xtext_daynum, breaks = xticks_daynum) +
  scale_y_continuous(labels = xtext_month, breaks = xticks_month, trans = "reverse") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.5))) +
  labs(title = "One year of chatting",
       subtitle = "2022-longest WhatsApp chat: messages distribution over days and day times") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#F1F8E9"),
    plot.title = element_text(family = "work", size = 15, hjust = 0.5,
                              margin = unit(c(15, 0, 5, 0), "pt"),
                              color = "#424242"),
    plot.subtitle = element_text(family = "work", size = 11, hjust = 0.5,
                                 margin = unit(c(0, 0, 30, 0), "pt"),
                                 color = "#424242"),
    # plot.caption = element_text(family = "work", size = 8, hjust = 1,
    #                             margin = unit(c(20, 0, 0, 0), "pt")),
    legend.background = element_rect(fill = "#F1F8E9"),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(family = "work", color = "#616161", hjust = 0,
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "work", color = "#9E9E9E", size = 9,
                               margin = unit(c(5, 0, 0, 0), "pt")),
    axis.text.y = element_text(family = "work", size = 10, color = "#424242", hjust = 1,
                               margin = unit(c(0, -2, 0, 0), "pt"))
  )

ggsave(bubbleplot, filename = here('viz/bubbleplot.png'), width = 18, height = 9.375)


# DONUT PLOT

donut_df <- df %>%
  filter(Year == 2022) %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1),
         WeekCat = week(DateTime),
         DateDay = floor_date(DateTime, unit = "day"),
         DayNum = day(DateDay),
         HourCat = as_factor(ifelse(HourShift > 16, "Night",
                                    ifelse(HourShift > 8, "Afternoon",
                                           "Morning"))),
         HourCat = fct_relevel(HourCat, c("Morning", "Afternoon", "Night"))) %>%
  group_by(Month, Name) %>%
  summarize(Count = n()) %>%
  ungroup()

donutplot <- ggplot() +
  geom_col(data = donut_df,
           aes(x = Month, y = Count, color = Name, fill = Name),
           position = "fill", alpha = 0.8, lwd = 0.7, width = 0.8) +
  scale_x_continuous(trans = "reverse") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c("#5AB574", "#FA5C5C"),
                    labels = c("Sent", "Received")) +
  scale_color_manual(values = c("#5AB574", "#FA5C5C"),
                     labels = c("Sent", "Received")) +
  geom_label(aes(x = seq(1, 12), y = 0, label = xtext_month),
             angle = 90, hjust = 0.5, size = 2, fill = "#F1F8E9", color = "#424242") +
  coord_polar(theta = "y") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#F1F8E9"),
    plot.title = element_text(family = "work", size = 15, hjust = 0.5,
                              margin = unit(c(15, 0, 5, 0), "pt"),
                              color = "#424242"),
    plot.subtitle = element_text(family = "work", size = 11, hjust = 0.5,
                                 margin = unit(c(0, 0, 30, 0), "pt"),
                                 color = "#424242"),
    legend.background = element_rect(fill = "#F1F8E9"),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(family = "work", color = "#616161", hjust = 0,
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "work", color = "#9E9E9E", size = 9,
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.text.y = element_blank())

ggsave(donutplot, filename = here('viz/donutplot.png'), width = 15, height = 9.375)


# SCATTER

ticks_hour2 <- seq(2, 23, by = 10)
text_hour2 <- c("2AM", "12AM", "10PM")

ticks_hour3 <- seq(1, 23, by = 3)
text_hour3 <- c("1AM", "4AM", "7AM", "10AM", "1PM", "4PM", "7PM", "10PM")

ticks_hour_full <- seq(1, 23)
text_hour_full <- c("1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM",
                "10AM", "11AM", "12AM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM",
                "7PM", "8PM", "9PM", "10PM", "11PM")

scatter_df <- df %>%
  filter(Year == 2022) %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1),
         WeekCat = week(DateTime),
         DateDay = floor_date(DateTime, unit = "day"),
         DayNum = day(DateDay),
         HourCat = as_factor(ifelse(HourShift > 16, "Night",
                                    ifelse(HourShift > 8, "Afternoon",
                                           "Morning"))),
         HourCat = fct_relevel(HourCat, c("Morning", "Afternoon", "Night"))) %>%
  group_by(Hour) %>%
  summarize(Count = n(), AvgLen = mean(WordCount)) %>%
  ungroup()

scatterplot <- ggplot(data = scatter_df) +
  geom_point(aes(x = Hour, y = AvgLen, color = Hour),
             alpha = 0.8, size = 10) +
  scale_size(range = c(1,15)) +
  scale_color_gradientn(colours = c("#1A237E", "#90CAF9", "#FFAB91", "#FFC107", 
                                   "#D84315", "#C51162", "#1A237E"), 
                       values = scales::rescale(x = c(0, 6, 10, 12, 16, 21, 23),
                                                to = c(0,1),
                                                from = c(0, 23)),
                       limits = c(0, 23)
  ) +
  scale_x_continuous(breaks = ticks_hour3, 
                     labels = text_hour3,
                     limits = c(0, 23)) +
  labs(x = "Time", y = "Average number of words") +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(4, 10, 4, 10), "cm"),
    plot.background = element_rect(fill = "#F1F8E9"),
    plot.title = element_text(family = "work", size = 15, hjust = 0.5,
                              margin = unit(c(15, 0, 5, 0), "pt"),
                              color = "#424242"),
    plot.subtitle = element_text(family = "work", size = 11, hjust = 0.5,
                                 margin = unit(c(0, 0, 30, 0), "pt"),
                                 color = "#424242"),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box.margin = unit(c(-45, 0, 0, 0), "pt"),
    legend.key.width = unit("3.1", "cm"),
    legend.key.height = unit("0.2", "cm"),
    legend.text = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "work", color = "#9E9E9E", size = 9,
                               margin = unit(c(20, 0, 0, 0), "pt")),
    axis.title.x = element_text(family = "work", color = "#424242", size = 11,
                                margin = unit(c(25, 0, 0, 0), "pt")),
    axis.title.y = element_text(family = "work", size = 13, color = "#424242",
                                margin = unit(c(0, 10, 0, 0), "pt")),
    axis.text.y = element_text(family = "work", size = 13, color = "#424242", hjust = 1,
                               face = "bold",
                               margin = unit(c(0, 20, 0, 0), "pt"))
    )

ggsave(scatterplot, filename = here('viz/scatterplot.png'), width = 15, height = 9.375)


# background <- ggplot(scatter_s) +
#   geom_point(aes(x = Hour, y = AvgLen), alpha = 0) +
#   theme_void() +
#   theme(plot.background = element_rect(fill = "#F1F8E9"))
# 
# ggsave(background, filename = "background.png", width = 18, height = 9.375)


