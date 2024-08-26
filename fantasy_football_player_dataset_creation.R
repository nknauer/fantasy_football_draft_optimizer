setwd("/Users/nknauer/Downloads")

install.packages("readxl")
library(readxl)
library(dplyr)

qbs <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "QBs")
rbs <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "RBs")
wrs <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "WRs")
tes <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "TEs")
ks <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "Ks")
defs <- read_excel("fantasy2024rankingsexcel.xlsx", sheet = "DEFs")

qbs <- qbs %>%
  mutate(`Name` = paste(`First Name`, `Last Name`))

rbs <- rbs %>%
  mutate(`Name` = paste(`First Name`, `Last Name`))

wrs <- wrs %>%
  mutate(`Name` = paste(`First Name`, `Last Name`))

tes <- tes %>%
  mutate(`Name` = paste(`First Name`, `Last Name`))

ks <- ks %>%
  mutate(`Name` = paste(`First Name`, `Last Name`))

qbs <- select(qbs, `Last Name`, `First Name`, Team, Bye, Name, Pos, `PPR POINTS`)
rbs <- select(rbs, `Last Name`, `First Name`, Team, Bye = `BYE`, Name, Pos, `PPR POINTS`)
wrs <- select(wrs, `Last Name`, `First Name`, Team, Bye, Name, Pos, `PPR POINTS` = `PPR Points`)
tes <- select(tes, `Last Name`, `First Name`, Team, Bye = `BYE`, Name, Pos, `PPR POINTS`)

combined_df <- rbind(qbs, rbs, wrs, tes)

write.csv(combined_df, 'ppr_2024.csv')


##ADP Clean Up
adp_dataset <- read.csv('FantasyPros_2024_Overall_ADP_Rankings.csv')
adp_dataset$Name <- adp_dataset$Player
adp_dataset$Position <- gsub("[0-9]", "", adp_dataset$POS)
adp_dataset$ADP <- adp_dataset$AVG

write.csv(adp_dataset, 'adp_2024.csv')
