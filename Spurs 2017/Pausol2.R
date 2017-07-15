library(httr)
library(tidyverse)
library(ggmap)
library(png)
library(reshape2)

season <- 2015
context <- "FGA"
url <- paste0("http://stats.nba.com/stats/shotchartdetail?Period=0&VsConference=&LeagueID=00&", 
              "LastNGames=0&TeamID=0&Position=&Location=&Outcome=&",
              "ContextMeasure=", context, "&DateFrom=&StartPeriod=&DateTo=&OpponentTeamID=0&",
              "ContextFilter=&RangeType=&Season=", paste0(season, "-", substr(season + 1, 3, 4)),
              "&AheadBehind=&PlayerID=0&EndRange=&VsDivision=&PointDiff=&RookieYear=",
              "&GameSegment=&Month=0&ClutchTime=&StartRange=&EndPeriod=&SeasonType=Regular+Season",
              "&SeasonSegment=&GameID=&PlayerPosition=")

raw <- content(httr::GET(url, add_headers(
  'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
  'Referer' = 'http://stats.nba.com/player/',
  'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36'
)))

game.2015 <-  data.frame(t(sapply(raw$resultSets[[1]]$rowSet, function(x) unlist(x))))
names(game.2015) <- raw$resultSets[[1]]$headers
game.2015$LOC_X <- as.numeric(levels(game.2015$LOC_X))[game.2015$LOC_X]
game.2015$LOC_Y <- as.numeric(levels(game.2015))[game.2015$LOC_Y]

season <- 2016
context <- "FGA"
url <- paste0("http://stats.nba.com/stats/shotchartdetail?Period=0&VsConference=&LeagueID=00&", 
              "LastNGames=0&TeamID=0&Position=&Location=&Outcome=&",
              "ContextMeasure=", context, "&DateFrom=&StartPeriod=&DateTo=&OpponentTeamID=0&",
              "ContextFilter=&RangeType=&Season=", paste0(season, "-", substr(season + 1, 3, 4)),
              "&AheadBehind=&PlayerID=0&EndRange=&VsDivision=&PointDiff=&RookieYear=",
              "&GameSegment=&Month=0&ClutchTime=&StartRange=&EndPeriod=&SeasonType=Regular+Season",
              "&SeasonSegment=&GameID=&PlayerPosition=")
#raw <- fromJSON(file = url)


raw <- httr::GET(url, add_headers(
  'Accept-Language' = 'en-US,en;q=0.8,af;q=0.6',
  'Referer' = 'http://stats.nba.com/player/',
  'User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36'
))

game <-  data.frame(t(sapply(raw$resultSets[[1]]$rowSet, function(x) unlist(x))))
names(game) <- raw$resultSets[[1]]$headers
game$LOC_X <- as.numeric(levels(game$LOC_X))[game$LOC_X]
game$LOC_Y <- as.numeric(levels(game$LOC_Y))[game$LOC_Y]

Overall - did the Spurs get offense similar to 2015-16? Team PPG increased from 103.5 to 105.3. 
But the overall leauge PPG increased from 106.4 to 108.8. The 2015-16 Spurs were already scoring less than the league average, 
and they lost even more ground in 2016-17. Their offense went from #4 at 110.3 to #9 at 111.1. 
(Their defense also down, as they went from allowing 92.9 PPG to allowing 98.1. 
Though they were still the top defensive rated team. ) 

scatter chart of offensive to defense ratings per player 2016 compared to 2017
spurs_2016 <- read.csv("Duncan Gasol/dat/spurs_advanced_2016.csv", stringsAsFactors = F)
spurs_2016$season <- "2016"
spurs_2017 <- read.csv("Duncan Gasol/dat/spurs_advanced_2017.csv", stringsAsFactors = F)
spurs_2017$season <- "2017"
both_players <- unlist(spurs_2016[spurs_2016$X %in% spurs_2017$X, ]$X)
spurs_both <- rbind(spurs_2016, spurs_2017) %>%
  filter(X %in% both_players)

  
looking at at offense and defense, we see just a couple of players who improved from 2016 to 2017

spurs_wide <- spurs_2016 %>%
  inner_join(spurs_2017, by = "X", suffix = c(".2016", ".2017"))

ggplot(spurs_wide, aes(OWS.2017, OWS.2016, X)) +
  geom_point(aes(x=OWS.2017,
                 y=OWS.2016)) +
  geom_abline(intercept=0, slope=1) +
  geom_label(aes(label=substring(X, 1, 3)))
  
ggplot(spurs_wide, aes(DWS.2017, DWS.2016, X)) +
  geom_point(aes(x=DWS.2017,
                 y=DWS.2016)) +
  geom_abline(intercept=0, slope=1) +
  geom_label(aes(label=substring(X, 1, 3)))

And just for fun, comparing offense against defense confirms what we know about Pop's Spurs: they rest their hat on defense. '

ggplot(spurs_both, aes(x=OWS, y=DWS, group=X)) +
  geom_point() + #aes(color=season)
  geom_line(arrow=arrow(type = "open")) +
  scale_x_continuous(lim=c(-2, 10)) +
  scale_y_continuous(lim=c(-2, 10)) +
  geom_abline(intercept=0, slope=1) +
  geom_label(aes(label=substring(X, 1, 3), color=season))



summarizeDistance <- function(playerName, seasonData) {
  player <- seasonData[seasonData$PLAYER_NAME == playerName, ]
  player_shots <- nrow(player)
  player_games <- length(unique(player$GAME_ID))
  player_makes <- nrow(player[player$SHOT_MADE_FLAG == 1,])
  player_shots_3 <- nrow(player[player$SHOT_TYPE == "3PT Field Goal", ])
  player_makes_3 <- nrow(player[player$SHOT_TYPE == "3PT Field Goal" & player$SHOT_MADE_FLAG == 1, ])
  player_shots_RA <- nrow(player[player$SHOT_ZONE_BASIC == "Restricted Area", ])
  player_makes_RA <- nrow(player[player$SHOT_ZONE_BASIC == "Restricted Area" & 
                                 player$SHOT_MADE_FLAG == 1, ])
  player_shots_non_RA <- nrow(player[player$SHOT_ZONE_BASIC == "In The Paint (Non-RA)", ])
  player_makes_non_RA <- nrow(player[player$SHOT_ZONE_BASIC == "In The Paint (Non-RA)"
                                   & player$SHOT_MADE_FLAG == 1, ])
  player_shots_8_16 <- nrow(player[player$SHOT_ZONE_RANGE == "8-16 ft." ,])
  player_makes_8_16 <- nrow(player[player$SHOT_ZONE_RANGE == "8-16 ft." & player$SHOT_MADE_FLAG == '1',])
  player_shots_16_24 <- nrow(player[player$SHOT_ZONE_RANGE == "16-24 ft." ,])
  player_makes_16_24 <- nrow(player[player$SHOT_ZONE_RANGE == "16-24 ft." & player$SHOT_MADE_FLAG == '1',])
  
  ret <- list(player_shots=player_shots, player_games=player_games, player_makes=player_makes, 
              player_shots_3=player_shots_3, player_makes_3=player_makes_3, 
              player_shots_RA=player_shots_RA, player_makes_RA=player_makes_RA, 
              player_shots_non_RA=player_shots_non_RA, player_makes_non_RA=player_makes_non_RA,
              player_shots_8_16=player_shots_8_16, player_makes_8_16=player_makes_8_16, 
              player_shots_16_24=player_shots_16_24, player_makes_16_24=player_makes_16_24)
  
}

gasol <- summarizeDistance("Pau Gasol", game)
aldridge <- summarizeDistance("LaMarcus Aldridge", game)
gasol.2015 <- summarizeDistance("Pau Gasol", game.2015)
aldridge.2015 <- summarizeDistance("LaMarcus Aldridge", game.2015)


avg <- NULL
avg <- cbind(cbind(Player = "Gasol", pct = gasol$player_makes_RA / gasol$player_shots_RA, metric = "Restricted Area"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge$player_makes_non_RA / aldridge$player_shots_non_RA, metric = "Non-Restricted\nArea"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol$player_makes_non_RA / gasol$player_shots_non_RA, metric = "Non-Restricted\nArea"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge$player_makes_8_16 / aldridge$player_shots_8_16, metric = "8-16 ft"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol$player_makes_8_16 / gasol$player_shots_8_16, metric = "8-16 ft"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol$player_makes_16_24 / gasol$player_shots_16_24, metric = "16-24 ft"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge$player_makes_RA / aldridge$player_shots_RA, metric = "Restricted Area"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge$player_makes_16_24 / aldridge$player_shots_16_24, metric = "16-24 ft"))
avg <- as.data.frame(avg)
avg$pct <- as.numeric(as.character(avg$pct))

avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol.2015$player_makes_RA / gasol.2015$player_shots_RA, metric = "Restricted Area"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge.2015$player_makes_non_RA / aldridge.2015$player_shots_non_RA, metric = "Non-Restricted\nArea"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol.2015$player_makes_non_RA / gasol.2015$player_shots_non_RA, metric = "Non-Restricted\nArea"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge.2015$player_makes_8_16 / aldridge.2015$player_shots_8_16, metric = "8-16 ft"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol.2015$player_makes_8_16 / gasol.2015$player_shots_8_16, metric = "8-16 ft"))
avg <- rbind(avg, cbind(Player = "Gasol", pct = gasol.2015$player_makes_16_24 / gasol.2015$player_shots_16_24, metric = "16-24 ft"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge.2015$player_makes_RA / aldridge.2015$player_shots_RA, metric = "Restricted Area"))
avg <- rbind(avg, cbind(Player = "Aldridge", pct = aldridge.2015$player_makes_16_24 / aldridge.2015$player_shots_16_24, metric = "16-24 ft"))
avg <- as.data.frame(avg)
avg$pct <- as.numeric(as.character(avg$pct))
avg$season <- 2016
avg[((nrow(avg)/2)+1):nrow(avg),]$season <- 2015

ggplot() + 
  # may have to do the reference as another bar, hidden by a white filled bar?
  geom_bar(data = avg[avg$Player != "Duncan" & avg$season == 2015,],
           aes(x = metric, y = pct, group = Player
               , fill=Player
               ), stat = "identity", width = 0.5,
           alpha=.2,
           #fill="white", 
           color="grey80",
           position = "dodge") +
  geom_bar(data = avg[avg$Player != "Duncan" & avg$season == 2016,],
           aes(x = metric, y = pct, fill = Player), stat = "identity", width = 0.4,
           color="black",
           position = "dodge") +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +

  labs(y = "Percentage", x = "Shot Zone", 
       title = "Player Shooting Percentages by Area\n2016-17 Regular Season FG vs 2015-16") +
  theme(
    panel.margin = unit(0, "lines"), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_line(color = "grey80"),
    legend.position = c(0.9, 0.1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8))

As seen in the graph, Aldridge's numbers dropped across the board, while Gasol's mostly improved (similarly represented in their respective eFG%).
Let's see how the team fared against the rest of the league'

spurs <- game[game$VTM == 'SAS' | game$HTM == 'SAS',]
leage <- game[game$VTM != 'SAS' & game$HTM != 'SAS',]



ggplot(game[game$VTM == 'SAS' | game$HTM == 'SAS',], aes(x = LOC_X * -1, y = LOC_Y * -1, z = as.numeric(SHOT_MADE_FLAG) - 1)) + 
  inset_raster(readPNG("../nba-halfcourt.png"), xmin = -250, xmax = 250, ymin = -420, ymax = 50) +
  lims(x = c(-250, 250), y = c(-420, 50)) +
  coord_fixed() +
  theme(line = element_blank(), 
        rect = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0, "cm"), 
        legend.position = c(0.1, 0.14),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.margin = unit(0, "lines"), 
        plot.margin = unit(c(2, 0, 0, 0), "lines"),
        plot.title = element_text(size = 18, margin = margin(t = -20))
  )  +
  ggtitle("Spurs vs League\n2016-17 Regular Season FGA") +
  annotate("text", x = 190, y = -405, size = 6, fontface = 2,
           label = paste0(round(sum(duncan$SHOT_MADE_FLAG ==1)/nrow(duncan)*100, 2), "% Overall")) +
  stat_summary_hex() +
  scale_fill_gradient(low = "darkblue", high = "lightblue", breaks = c(0, .5, 1), 
                      labels = c("0%", "50%", "100%"))
