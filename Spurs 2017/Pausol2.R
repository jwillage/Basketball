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



summarizeDistance <- function(player) {
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
  player_shots_24_plus <- nrow(player[player$SHOT_ZONE_RANGE == "24+ ft." ,])
  player_makes_24_plus <- nrow(player[player$SHOT_ZONE_RANGE == "24+ ft." & player$SHOT_MADE_FLAG == '1',])
  
  player_pct_RA <- player_makes_RA / player_shots_RA 
  player_pct_non_RA <- player_makes_non_RA / player_shots_non_RA
  player_pct_8_16 <- player_makes_8_16 / player_shots_8_16
  player_pct_16_24 <- player_makes_16_24 / player_shots_16_24
  player_pct_24_plus <- player_makes_24_plus / player_shots_24_plus
  
  ret <- list(player_shots=player_shots, player_games=player_games, player_makes=player_makes, 
              player_shots_3=player_shots_3, player_makes_3=player_makes_3, 
              player_shots_RA=player_shots_RA, player_makes_RA=player_makes_RA, 
              player_shots_non_RA=player_shots_non_RA, player_makes_non_RA=player_makes_non_RA,
              player_shots_8_16=player_shots_8_16, player_makes_8_16=player_makes_8_16, 
              player_shots_16_24=player_shots_16_24, player_makes_16_24=player_makes_16_24,
              player_pct_RA=player_pct_RA,player_pct_non_RA=player_pct_non_RA,
              player_pct_8_16=player_pct_8_16, player_pct_16_24=player_pct_16_24,
              player_shots_24_plus=player_shots_24_plus, player_makes_24_plus=player_makes_24_plus,
              player_pct_24_plus=player_pct_24_plus)
  
}

gasol <- summarizeDistance(game[game$PLAYER_NAME == "Pau Gasol", ])
aldridge <- summarizeDistance(game[game$PLAYER_NAME == "LaMarcus Aldridge", ])
gasol.2015 <- summarizeDistance(game.2015[game.2015$PLAYER_NAME ==  "Pau Gasol", ])
aldridge.2015 <- summarizeDistance(game.2015[game.2015$PLAYER_NAME == "LaMarcus Aldridge", ])

# todo clean up below now that percentages have been added in player summaries

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
  geom_bar(data = avg[avg$season == 2015,],
           aes(x = metric, y = pct, group = Player, fill=Player), 
           stat = "identity", width = 0.5, alpha=.2, color="grey80", position = "dodge") +
  geom_bar(data = avg[avg$season == 2016,],
           aes(x = metric, y = pct, fill = Player), stat = "identity", width = 0.4,
           color="black", position = "dodge") +
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

game$xy <- paste(game$LOC_X, game$LOC_Y)
spurs <- game[game$VTM == 'SAS' | game$HTM == 'SAS',]
league <- game[game$VTM != 'SAS' & game$HTM != 'SAS',]

spursCoordSummary <- spurs %>%
    group_by(LOC_X, LOC_Y) %>%
    summarize(makes = sum((SHOT_MADE_FLAG==1)), attempts = n()) %>%
    mutate(pct = makes/attempts)

#  need to weight...possibly.

leagueCoordSummary <- league %>%
  group_by(LOC_X, LOC_Y) %>%
  summarize(makes = sum((SHOT_MADE_FLAG==1)), attempts = n()) %>%
  mutate(pct = makes/attempts)  

combinedCoordSummary <- spursCoordSummary %>%
 inner_join(leagueCoordSummary, by=c("LOC_X" = "LOC_X", "LOC_Y" = "LOC_Y"),            
             suffix=c(".spurs", ".league")) %>%
  mutate(relativePct = pct.spurs - pct.league)
  

ggplot(combinedCoordSummary, aes(x = LOC_X * -1, y = LOC_Y * -1, z = relativePct)) + 
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
  stat_summary_hex(binwidth = 20) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", breaks = c(-1, 0, 1), 
                      labels = c("-100%", "0%", "100%"))

spursShotSummary <- summarizeDistance(spurs)
leagueShotSummary <- summarizeDistance(league)

relativeShotSummary <- mapply(function(x, y)  x - y,
       spursShotSummary,
       leagueShotSummary)

relativeShotSummary[grepl("pct", names(relativeShotSummary))]*100

Relative to the rest of the league, the Spurs overall underperformed scoring in the paint. 
They got better the further out they shot, beating the league by 3.3% from 16-24 ft (eye roll), 
and 1.1% from 3.

And so went the regular the season. Into the playoffs the Spurs kept scoring at around the same rate.
That worked fine against their first two opponents, who SA held to sum(82, 82, 105, 110, 103, 96)/6 (96)
and sum(126, 96, 92, 125, 107, 75)/6 (104) PPG. But it won't cut it when your defense collapses,
as every team's does, when they play the Warriors.

' 


defensive hex plot

