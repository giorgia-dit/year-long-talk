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


# LOADING DATA #

alessandra <- read.csv("~/PycharmProjects/whatsapp/export/alessandra.csv")
simone <- read.csv("~/PycharmProjects/whatsapp/export/simone.csv")
francesca <- read.csv("~/PycharmProjects/whatsapp/export/francesca.csv")

# LOADING UTILITIES #

showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Roboto", "roboto")
font_add_google("Work Sans", "work")


# CLEANING #

df_alessandra <- alessandra %>%
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

df_simone <- simone %>%
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

df_francesca <- francesca %>%
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

write.csv(df_alessandra, file="df_alessandra.csv", row.names = FALSE)
write.csv(df_simone, file="df_simone.csv", row.names = FALSE)
write.csv(df_francesca, file="df_francesca.csv", row.names = FALSE)


# VISUALIZING #

# BAR PLOT #

bar_a <- df_alessandra %>%
  filter(Year == 2022) %>%
  group_by(Month, HourShift) %>%
  summarize(Count = n())

bar_s <- df_simone %>%
  filter(Year == 2022) %>%
  group_by(Month, HourShift) %>%
  summarize(Count = n())

bar_f <- df_francesca %>%
  filter(Year == 2022) %>%
  group_by(Month, HourShift) %>%
  summarize(Count = n())

xticks_month <- seq(1, 12)
xtext_month <- c("January", "February", "March", "April", "May", "June", "July", 
                  "August", "September", "October", "November", "December")
xtext_month <- sapply(xtext_month, toupper)

ticks_hour <- seq(2, 23, by=5)
text_hour <- c("7AM", "12AM", "5PM", "10PM", "3AM")

iconpath_1 <- "~/PycharmProjects/whatsapp/png/1.png"
iconpath_2 <- "~/PycharmProjects/whatsapp/png/2.png"
iconpath_3 <- "~/PycharmProjects/whatsapp/png/3.png"
icon_1 <- readPNG(iconpath_1)
icon_2 <- readPNG(iconpath_2)
icon_3 <- readPNG(iconpath_3)
icon_1 <- rasterGrob(icon_1, interpolate=TRUE)
icon_2 <- rasterGrob(icon_2, interpolate=TRUE)
icon_3 <- rasterGrob(icon_3, interpolate=TRUE)


barplot_a <- ggplot(bar_a, aes(Month, Count, fill = HourShift)) +
  # geom_chicklet(position = "fill", radius = grid::unit(3, "pt"),
  #               width = 0.8, alpha = 0.9) +
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
  # labs(title = "What's her name?",
  #      subtitle = "Most popular female baby names in the US over time") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    aspect.ratio = 1:1,
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#f7f7f7"),
    # plot.title = element_text(family = "work", size = 16, hjust = 0.5,
    #                           margin = unit(c(25, 0, 5, 0), "pt")),
    # plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
    #                              margin = unit(c(0, 0, 15, 0), "pt")),
    # plot.caption = element_text(family = "work", size = 8, hjust = 1,
    #                             margin = unit(c(20, 0, 0, 0), "pt")),
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

ggsave(barplot_a, filename = "barplot_a.png", width = 15, height = 9.375)


barplot_s <- ggplot(bar_s, aes(Month, Count, fill = HourShift)) +
  # geom_chicklet(position = "fill", radius = grid::unit(3, "pt"),
  #               width = 0.8, alpha = 0.9) +
  geom_col(position = "fill", width = 0.8, alpha = 0.8) +
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
  labs(y = "Messages count") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    aspect.ratio = 1:1,
    plot.margin = unit(c(4, 2, 4, 2), "cm"),
    plot.background = element_rect(fill = "#F1F8E9"),
    # plot.title = element_text(family = "work", size = 16, hjust = 0.5,
    #                           margin = unit(c(25, 0, 5, 0), "pt")),
    # plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
    #                              margin = unit(c(0, 0, 15, 0), "pt")),
    # plot.caption = element_text(family = "work", size = 8, hjust = 1,
    #                             margin = unit(c(20, 0, 0, 0), "pt")),
    legend.background = element_rect(fill = "#F1F8E9"),
    legend.title = element_blank(),
    legend.key.width = unit(0.7, 'cm'), 
    legend.key.height = unit(0.8, 'cm'),
    legend.text = element_text(family = "work", color = "#616161", hjust = 0,
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "work", color = "#BDBDBD", size = 11,
                              margin = unit(c(10, 0, 0, 0), "pt")),
    axis.text.x = element_text(family = "work", color = "#BDBDBD", size = 9,
                               margin = unit(c(-15, 0, 0, 0), "pt")),
    axis.text.y = element_text(family = "work", size = 10, color = "#424242", hjust = 1,
                               margin = unit(c(0, -2, 0, 20), "pt"))
  )

ggsave(barplot_s, filename = "barplot_s.png", width = 15, height = 9.375)


barplot_f <- ggplot(bar_f, aes(Month, Count, fill = HourShift)) +
  # geom_chicklet(position = "fill", radius = grid::unit(3, "pt"),
  #               width = 0.8, alpha = 0.9) +
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
  # labs(title = "What's her name?",
  #      subtitle = "Most popular female baby names in the US over time") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    aspect.ratio = 1:1,
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    plot.background = element_rect(fill = "#f7f7f7"),
    # plot.title = element_text(family = "work", size = 16, hjust = 0.5,
    #                           margin = unit(c(25, 0, 5, 0), "pt")),
    # plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
    #                              margin = unit(c(0, 0, 15, 0), "pt")),
    # plot.caption = element_text(family = "work", size = 8, hjust = 1,
    #                             margin = unit(c(20, 0, 0, 0), "pt")),
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

ggsave(barplot_f, filename = "barplot_f.png", width = 15, height = 9.375)


# BUBBLE PLOT

bubble_a <- df_alessandra %>%
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

bubbleplot_a <- ggplot() +
  geom_segment(data = bubble_a, aes(x = 1, y = Month, xend = 31, yend = Month), color = "#CFD8DC") +
  geom_point(data = bubble_a,
             aes(DayNum, Month, color = HourCat), alpha = 0, size = 8) +
  scale_color_manual(values = c("#FFA726", "#AD1457", "#1A237E")) +
  geom_point(data = bubble_a %>% group_by(Month, DayNum) %>% summarize(Total = sum(Count)),
             aes(DayNum, Month, size = Total), shape = 21, stroke = 1,
             color = "#455A64", fill = "#FAFAFA") +
  scale_size_continuous(range = c(1, 13), limits = c(1, 300)) +
  new_scale("size") +
  geom_point(data = bubble_a %>% filter(HourCat == "Morning"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#FFA726", alpha = 0.5, fill = "#FFA726", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_a %>% filter(HourCat == "Afternoon"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#AD1457", alpha = 0.5, fill = "#AD1457", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_a %>% filter(HourCat == "Night"),
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

ggsave(bubbleplot_a, filename = "bubbleplot_a.png", width = 18, height = 9.375)


bubble_s <- df_simone %>%
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

bubbleplot_s <- ggplot() +
  geom_segment(data = bubble_s, aes(x = 1, y = Month, xend = 31, yend = Month), color = "#CFD8DC") +
  geom_point(data = bubble_s,
             aes(DayNum, Month, color = HourCat), alpha = 0, size = 8) +
  scale_color_manual(values = c("#FFA726", "#AD1457", "#1A237E")) +
  geom_point(data = bubble_s %>% group_by(Month, DayNum) %>% summarize(Total = sum(Count)),
             aes(DayNum, Month, size = Total), shape = 21, stroke = 1,
             color = "#455A64", fill = "#FAFAFA") +
  scale_size_continuous(range = c(1, 13), limits = c(1, 300)) +
  new_scale("size") +
  geom_point(data = bubble_s %>% filter(HourCat == "Morning"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#FFA726", alpha = 0.5, fill = "#FFA726", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_s %>% filter(HourCat == "Afternoon"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#AD1457", alpha = 0.5, fill = "#AD1457", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_s %>% filter(HourCat == "Night"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#1A237E", alpha = 0.5, fill = "#1A237E", stroke = 0.7,
             show.legend = FALSE) +
  scale_size_continuous(range = c(0.1, 10), limits = c(1, 300)) +
  scale_x_continuous(labels = xtext_daynum, breaks = xticks_daynum) +
  scale_y_continuous(labels = xtext_month, breaks = xticks_month, trans = "reverse") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.5))) +
  labs(title = "A one-year-long talk",
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

ggsave(bubbleplot_s, filename = "bubbleplot_s.png", width = 18, height = 9.375)




bubble_f <- df_francesca %>%
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

bubbleplot_f <- ggplot() +
  geom_segment(data = bubble_f, aes(x = 1, y = Month, xend = 31, yend = Month), color = "#CFD8DC") +
  geom_point(data = bubble_f,
             aes(DayNum, Month, color = HourCat), alpha = 0, size = 8) +
  scale_color_manual(values = c("#FFA726", "#AD1457", "#1A237E")) +
  geom_point(data = bubble_f %>% group_by(Month, DayNum) %>% summarize(Total = sum(Count)),
             aes(DayNum, Month, size = Total), shape = 21, stroke = 1,
             color = "#455A64", fill = "#FAFAFA") +
  scale_size_continuous(range = c(1, 13), limits = c(1, 300)) +
  new_scale("size") +
  geom_point(data = bubble_f %>% filter(HourCat == "Morning"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#FFA726", alpha = 0.5, fill = "#FFA726", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_f %>% filter(HourCat == "Afternoon"),
             aes(DayNum, Month, size = Count),
             position = position_nudge(x = 0, y = 0),
             color = "#AD1457", alpha = 0.5, fill = "#AD1457", stroke = 0.7,
             show.legend = FALSE) +
  geom_point(data = bubble_f %>% filter(HourCat == "Night"),
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

ggsave(bubbleplot_f, filename = "bubbleplot_f.png", width = 18, height = 9.375)


# BACK-TO-BACK PLOT

back_a <- df_alessandra %>%
  filter(Year == 2022) %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1),
         WeekCat = week(DateTime),
         DateDay = floor_date(DateTime, unit = "day"),
         DayNum = day(DateDay),
         Name = as.character(Name),
         HourCat = as_factor(ifelse(HourShift > 16, "Night",
                                    ifelse(HourShift > 8, "Afternoon",
                                           "Morning"))),
         HourCat = fct_relevel(HourCat, c("Morning", "Afternoon", "Night"))) %>%
  group_by(Week, Name) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  mutate(Count = ifelse(Name == " giorgia ditano",
                        Count,
                        -Count))
  
back_a1 <- back_a %>%
  filter(Name == " giorgia ditano")

back_a2 <- back_a %>%
  filter(Name == " alessandra") %>%
  inner_join(back_a1 %>% select(Week), by = "Week")

back_a1 <- back_a1 %>%
  inner_join(back_a2 %>% select(Week), by = "Week")

diff <- back_a1 %>%
  inner_join(back_a2, by = "Week") %>%
  mutate(Diff = Count.x - Count.y) %>%
  select(Week, Diff)


ggplot(data = back_a) +
  geom_segment(data = back_a1,
               aes(x = Count, y = Week,
                   yend = back_a2$Week, xend = back_a2$Count),
               color = "#aeb6bf", size = 1.5, alpha = .5) +
  geom_point(aes(x = Count, y = Week, color = Name), size = 4, show.legend = TRUE)


# DONUT PLOT

donut_a <-df_alessandra %>%
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

donutplot_a <- ggplot() +
  geom_col(data = donut_a,
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
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.text.y = element_blank())

ggsave(donutplot_a, filename = "donutplot_a.png", width = 15, height = 9.375)


donut_s <-df_simone %>%
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

donutplot_s <- ggplot() +
  geom_col(data = donut_s,
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
    plot.margin = unit(c(2.2, 2, 2.2, 2), "cm"),
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
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.text.y = element_blank())

ggsave(donutplot_s, filename = "donutplot_s.png", width = 15, height = 9.375)


donut_f <-df_francesca %>%
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

donutplot_f <- ggplot() +
  geom_col(data = donut_f,
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
                               margin = unit(c(0, 0, 0, 0), "pt")),
    axis.text.y = element_blank())

ggsave(donutplot_f, filename = "donutplot_f.png", width = 15, height = 9.375)


# SCATTER

ticks_hour2 <- seq(2, 23, by = 10)
text_hour2 <- c("2AM", "12AM", "10PM")

ticks_hour3 <- seq(1, 23, by = 3)
text_hour3 <- c("1AM", "4AM", "7AM", "10AM", "1PM", "4PM", "7PM", "10PM")

ticks_hour_full <- seq(1, 23)
text_hour_full <- c("1AM", "2AM", "3AM", "4AM", "5AM", "6AM", "7AM", "8AM", "9AM",
                "10AM", "11AM", "12AM", "1PM", "2PM", "3PM", "4PM", "5PM", "6PM",
                "7PM", "8PM", "9PM", "10PM", "11PM")

scatter_s <-df_simone %>%
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

scatterplot_s <- ggplot(data = scatter_s) +
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
    # plot.caption = element_text(family = "work", size = 8, hjust = 1,
    #                             margin = unit(c(20, 0, 0, 0), "pt")),
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

ggsave(scatterplot_s, filename = "scatterplot_s.png", width = 15, height = 9.375)


background <- ggplot(scatter_s) +
  geom_point(aes(x = Hour, y = AvgLen), alpha = 0) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F1F8E9"))

ggsave(background, filename = "background.png", width = 18, height = 9.375)


