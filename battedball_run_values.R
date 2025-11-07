### Create run values for launch angle-exit velo bins
require(dplyr)
require(baseballr)
require(RPostgres)
require(tidyr)

pw <- {
  "LbrMkt22!"
}


# Connect to pitching database
pitching_db <- DBI::dbConnect(RPostgres::Postgres(),
                              dbname = "pitching",
                              user = "postgres",
                              password = pw,
                              host = "localhost",
                              port = 5432)

## Read in data (use only 2017 for example)
data2023 <- tbl(pitching_db, 'statcast') %>%
  filter(game_year == 2023) %>%
  collect()

## Use baseballr function to calculate run expectancy values. re24 is the run value of the event
data2023 <- run_expectancy_code(data2023, level = "plate appearance")

## Construct bins based on launch angle and exit velocity
data2023 <- data2023 %>%
  mutate(bbe_bin = NA)

# Ground balls
data2023$bbe_bin[data2023$launch_angle < 10 & data2023$launch_speed < 90] <- 0
data2023$bbe_bin[data2023$launch_angle < 10 & between(data2023$launch_speed, 90, 95)] <- 1
data2023$bbe_bin[data2023$launch_angle < 10 & between(data2023$launch_speed, 96, 100)] <- 2
data2023$bbe_bin[data2023$launch_angle < 10 & between(data2023$launch_speed, 101, 105)] <- 3
data2023$bbe_bin[data2023$launch_angle < 10 & data2023$launch_speed > 106] <- 4

# Line drives
data2023$bbe_bin[between(data2023$launch_angle, 10, 24) & data2023$launch_speed < 90] <- 5
data2023$bbe_bin[between(data2023$launch_angle, 10, 24) & between(data2023$launch_speed, 90, 95)] <- 6
data2023$bbe_bin[between(data2023$launch_angle, 10, 24) & between(data2023$launch_speed, 96, 100)] <- 7
data2023$bbe_bin[between(data2023$launch_angle, 10, 24) & between(data2023$launch_speed, 101, 105)] <- 8
data2023$bbe_bin[between(data2023$launch_angle, 10, 24) & data2023$launch_speed > 106] <- 9

# Fly balls
data2023$bbe_bin[between(data2023$launch_angle, 25, 50) & data2023$launch_speed < 90] <- 10
data2023$bbe_bin[between(data2023$launch_angle, 25, 50) & between(data2023$launch_speed, 90, 95)] <- 11
data2023$bbe_bin[between(data2023$launch_angle, 25, 50) & between(data2023$launch_speed, 96, 100)] <- 12
data2023$bbe_bin[between(data2023$launch_angle, 25, 50) & between(data2023$launch_speed, 101, 105)] <- 13
data2023$bbe_bin[between(data2023$launch_angle, 25, 50) & data2023$launch_speed > 106] <- 14

# Pop ups
data2023$bbe_bin[data2023$launch_angle > 50] <- 15

data2023 %>% filter(!is.na(bbe_bin)) %>% group_by(bbe_bin) %>% summarise(meanRV = mean(re24))
