# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(stringi)

setwd("/Users/nicholas.knauer/Downloads")
dataset <- read_csv('ADP.csv')
dataset$Position <- as.character(dataset$Position)
dataset$Name <- as.character(dataset$Name)

name_mapping <- c("Travis Etienne" = "Travis Etienne Jr.",
                  "Kenneth Walker" = "Kenneth Walker III",
                  "Michael Pittman" = "Michael Pittman Jr.")

# Assuming your data frame is called 'df', replace the values in the 'Player' column
dataset <- dataset %>%
  mutate(Name = recode(Name, !!!name_mapping))


players <- read_csv('players.csv')
players$player <- iconv(players$player, to = "latin1", sub = "")
players$player <- iconv(players$player, from = "latin1", to = "UTF-8")
players$player <- gsub("�", "", players$player)
players <- players %>% filter(grepl("/", player))
players <- players %>%
  mutate(clean_name = str_extract(player, "^[^/]+"))
players$clean_name <- str_trim(players$clean_name)

ppr <- read_csv('ppr.csv')

# Assuming your data frame is called 'df', replace the values in the 'Player' column
ppr <- ppr %>%
  mutate(Name = recode(Name, !!!name_mapping))



ppr_and_adp <- left_join(ppr, dataset, by = c("Name" = "Name"))
ppr_and_adp <- ppr_and_adp[!is.na(ppr_and_adp$ADP), ]

##Filter for only available players so it can recalculate
ppr_and_adp <- ppr_and_adp %>%
  filter(!(Name %in% players$clean_name))


position_stats <- ppr_and_adp %>% 
  group_by(Position) %>%
  summarize(
    avg_points = mean(`PPR POINTS`, na.rm = TRUE),
    max_adp_position = max(ADP, na.rm = TRUE)
  ) %>%
  ungroup()

ppr_and_adp <- inner_join(ppr_and_adp, position_stats, by = "Position")
ppr_and_adp$max_points_difference <- max(ppr_and_adp$`PPR POINTS` - ppr_and_adp$avg_points, na.rm = TRUE)

ppr_and_adp <- ppr_and_adp %>%
  mutate(score = `PPR POINTS` * 
           (1 - ADP/max_adp_position) * 
           (1 + (`PPR POINTS` - avg_points)/max_points_difference)) %>%
  arrange(desc(score))

results <- select(ppr_and_adp,Name, Pos, `PPR POINTS`, ADP, avg_points, max_points_difference, score)

##Filter Results By position needed
#Bench
#3 RB
#3 WR
#1 TE
results_new <- filter(results, Pos %in% c('WR', 'TE', 'RB', 'QB'))

View(results_new)
