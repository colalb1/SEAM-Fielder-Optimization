# libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(mgcv)
library(broom)
library(GeomMLBStadiums)

# helper function
euclidean = function(a, b) {sqrt(sum((a - b) ^ 2))}

# data
statcast_raw = fread("statcast-all-pitches.csv")
statcast_in_play = data.table(readRDS("statcast_in_play_white_sox"))

# this converts feet to SEAM units of length
converter = 36.11364 / 90





# filtering and cleaning statcast data
statcast_raw = statcast_raw[if_fielding_alignment == "Standard" & of_fielding_alignment == "Standard" & description == "hit_into_play"]
statcast = statcast_raw[, .(hc_x, hc_y, hit_location, events, fielder_3, fielder_4, fielder_5, fielder_6, fielder_7, fielder_8, fielder_9)]
statcast[, hc_x := (hc_x - 125.42) / converter]; statcast[, hc_y := (198.27 - hc_y) / converter]; statcast = na.omit(statcast)

statcast = statcast_in_play[events %in% c(#"single", 
                                          #"double", 
                                          "field_out",
                                          "grounded_into_double_play", 
                                          #"triple", 
                                          "force_out",
                                          "field_error", 
                                          "double_play", 
                                          "fielders_choice",
                                          "fielders_choice_out", 
                                          #"home_run", 
                                          "triple_play"), ]


#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
## Infield Models
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################





############# NOTE TO READER: #############
# All of these models are basically the same chunk of code with an index changed.
# Yes, this would be more efficient if I wrote this as a function; this is more explicit so I don't have to explain as much.
# I will comment on what everything does for the first baseman model, and the rest of the models follow similarly.
###########################################





# first baseman model

# filtering to balls in play where first baseman touched the ball first
statcast_first = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_first = statcast_first[, fielded_indicator := ifelse(hit_location == 3, 1, 0)]

# sampling
statcast_first_sample = statcast_first[sample(.N, .N %/% 10)]

# basic GAM
first_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_first_sample)

# grid I will predict over
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))

# prediction using GAM model
field_predictions = first_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)

# plotting
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("First Baseman Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# third fielder model
statcast_third = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_third = statcast_third[, fielded_indicator := ifelse(hit_location == 5, 1, 0)]
statcast_third_sample = statcast_third[sample(.N, .N %/% 10)]
third_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_third_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = third_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Third Baseman Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# second fielder model
statcast_second = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_second = statcast_second[, fielded_indicator := ifelse(hit_location == 4, 1, 0)]
statcast_second_sample = statcast_second[sample(.N, .N %/% 10)]
second_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_second_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = second_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Second Baseman Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# short fielder model
statcast_short = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_short = statcast_short[, fielded_indicator := ifelse(hit_location == 6, 1, 0)]
statcast_short_sample = statcast_short[sample(.N, .N %/% 10)]
short_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_short)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = short_gam %>%
  augment(type.predict = "response", newdata = field_grid)

odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Shortstop Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")


#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
## Outfield Models
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################


# center fielder model
statcast_center = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_center = statcast_center[, fielded_indicator := ifelse(hit_location == 8, 1, 0)]
statcast_center_sample = statcast_center[sample(.N, .N %/% 10)]
center_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_center_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = center_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Center Fielder Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# left fielder model
statcast_left = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_left = statcast_left[, fielded_indicator := ifelse(hit_location == 7, 1, 0)]
statcast_left_sample = statcast_left[sample(.N, .N %/% 10)]
left_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                 family = binomial,
                 data = statcast_left_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = left_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Left Fielder Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# right fielder model
statcast_right = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_right = statcast_right[, fielded_indicator := ifelse(hit_location == 9, 1, 0)]
statcast_right_sample = statcast_right[sample(.N, .N %/% 10)]
right_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
               family = binomial,
               data = statcast_right_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = right_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  ggtitle("Right Fielder Positioning Shape") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")



#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
## Pitcher and Catcher Models
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################


# pitcher model
statcast_pitcher = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_pitcher = statcast_pitcher[, fielded_indicator := ifelse(hit_location == 1, 1, 0)]
statcast_pitcher_sample = statcast_pitcher[sample(.N, .N %/% 10)]
pitcher_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
               family = binomial,
               data = statcast_pitcher_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = pitcher_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")




# catcher model
statcast_catch = statcast[, .(hc_x, hc_y, hit_location, events)]; statcast_catch = statcast_catch[, fielded_indicator := ifelse(hit_location == 2, 1, 0)]
statcast_catch_sample = statcast_catch[sample(.N, .N %/% 10)]
catch_gam = gam(fielded_indicator ~ s(hc_x) + s(hc_y),
                family = binomial,
                data = statcast_catch_sample)
field_grid = expand.grid(
  hc_x = seq(-300, 300, length.out = 1000),
  hc_y = seq(0, 500, length.out = 1000))
field_predictions = catch_gam %>%
  augment(type.predict = "response", newdata = field_grid)
odd_values = seq(from = 0, to = 1, by = 0.02)
ggplot(field_predictions, aes(x = hc_x, y = hc_y)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  xlab("Field X-axis") +
  ylab("Field Y-axis") +
  scale_fill_gradient("Fielded Prob", low = "blue", high = "white")

