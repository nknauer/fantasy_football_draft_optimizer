library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(tidyverse)

# Load data
setwd("/Users/nknauer/Downloads")
adp_data <- read_csv('adp_2024.csv')
players_data <- read_csv('players_2024.csv')
ppr_data <- read_csv('ppr_2024.csv')
sos_data <- read_csv('FantasyPros_Fantasy_Football_2024_Stength_Of_Schedule.csv')

# Clean and preprocess data
adp_data$Position <- as.character(adp_data$Position)
adp_data$Name <- as.character(adp_data$Name)

name_mapping <- c("Travis Etienne" = "Travis Etienne Jr.",
                  "Kenneth Walker" = "Kenneth Walker III",
                  "Michael Pittman" = "Michael Pittman Jr.",
                  "Patrick Mahomes" = "Patrick Mahomes II",
                  "Gardner Minshew" = "Gardner Minshew II")

adp_data <- adp_data %>%
  mutate(Name = recode(Name, !!!name_mapping))

players_data$player <- iconv(players_data$player, to = "latin1", sub = "")
players_data$player <- iconv(players_data$player, from = "latin1", to = "UTF-8")
players_data$player <- gsub("�", "", players_data$player)
players_data <- players_data %>% filter(grepl("/", player))
players_data <- players_data %>%
  mutate(clean_name = str_extract(player, "^[^/]+"))
players_data$clean_name <- str_trim(players_data$clean_name)

##Clean up Player Data
players_data$position <- str_extract(players_data$player, "(WR|RB|QB|TE)")
players_data$fantasy_team <- str_extract(players_data$player, "(?<=-\\s).*")

##players_data <- head(players_data, 10)

count_of_players_drafted <- 
  players_data %>%
  filter(fantasy_team == 'Atlanta Falcons') %>%
  group_by(position) %>%
  summarize(players_drafted = n())

players_data <- players_data %>%
  mutate(clean_name = ifelse(clean_name == "Patrick Mahomes", "Patrick Mahomes II", clean_name))

ppr_data <- ppr_data %>%
  mutate(Name = recode(Name, !!!name_mapping))

##Clean SOS Data
sos_data[,-1] <- lapply(sos_data[,-1], function(x) {
  as.numeric(gsub(".*(\\d) star matchup.*", "\\1", x))
})

sos_data <- sos_data %>%
  pivot_longer(cols = -Team,  # All columns except 'Team'
               names_to = "Pos",  # Name for the new column that holds the old column names
               values_to = "SOS")  # Name for the new column that holds the values


# Join data
ppr_and_adp <- left_join(ppr_data, adp_data, by = c("Name" = "Name"))
ppr_and_adp <- ppr_and_adp[!is.na(ppr_and_adp$ADP), ]
ppr_and_adp <- left_join(ppr_and_adp, sos_data, by = c("Team.y" = "Team", "Position" = "Pos"))
ppr_and_adp <- ppr_and_adp %>%
  filter(!(Name %in% players_data$clean_name))

ppr_and_adp <- ppr_and_adp %>%
  filter(!is.na(SOS))

# Calculate position-based statistics
position_stats <- ppr_and_adp %>% 
  group_by(Position) %>%
  summarize(
    avg_points = mean(`PPR POINTS`, na.rm = TRUE),
    max_adp_position = max(ADP, na.rm = TRUE),
    std_dev_points = sd(`PPR POINTS`, na.rm = TRUE)
  ) %>%
  ungroup()

# Invert the normalization based on your clarified logic
ppr_and_adp <- ppr_and_adp %>%
  group_by(Position) %>%
  mutate(opponent_strength_normalized = (SOS - 1) / 4) %>%
  ungroup()


# Combine data and calculate score
ppr_and_adp <- inner_join(ppr_and_adp, position_stats, by = "Position")
ppr_and_adp <- ppr_and_adp %>%
  group_by(Position) %>%
  mutate(max_points_difference = max(`PPR POINTS` - avg_points, na.rm = TRUE)) %>%
  ungroup()
ppr_and_adp <- ppr_and_adp %>%
  mutate(score = `PPR POINTS` * 
           (1 - ADP/max_adp_position) * 
           (1 + (`PPR POINTS` - avg_points)/max_points_difference) *
           (1 + (`PPR POINTS` - avg_points)/std_dev_points) *
           (1 + (ppr_and_adp$ADP - min(ppr_and_adp$ADP))/(max(ppr_and_adp$ADP) - min(ppr_and_adp$ADP))) *
           (1 + opponent_strength_normalized)) %>%
  arrange(desc(score))

# Add team need factor
team_need_factors <- c(
  "QB" = 3,
  "RB" = 7,
  "WR" = 7,
  "TE" = 3,
  "K" = 1,
  "D/ST" = 2
)

ppr_and_adp <- ppr_and_adp %>%
  mutate(team_need_factor = team_need_factors[Pos])

ppr_and_adp <- left_join(ppr_and_adp, count_of_players_drafted, by=c('Position' = 'position')) %>%
  mutate(players_drafted = coalesce(players_drafted, 0),
         team_needs = team_need_factor - players_drafted)

ppr_and_adp <- ppr_and_adp %>%
  mutate(final_score = score * team_needs)

# Filter and display results
results <- select(ppr_and_adp, Name, Team = Team.y, Pos, `PPR POINTS`, ADP, avg_points, max_points_difference, score, final_score)
results_new_test <- filter(results, Pos %in% c('WR', 'TE', 'RB', 'QB'))
results_new_test <- results_new_test %>% arrange(desc(final_score))
View(results_new_test)
