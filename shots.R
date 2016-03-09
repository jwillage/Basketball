library(rjson)
library(dplyr)
library(png)
library(ggplot2)

gameId <- "0923"
season <- 2015
playerId <- "0"
teamId <- "0"
context <- "FGA"
team <- "New York Knicks"

url <- paste0("http://stats.nba.com/stats/shotchartdetail?Period=0&VsConference=&LeagueID=00&", 
              "LastNGames=0&TeamID=", teamId, "&Position=&Location=&Outcome=&",
              "ContextMeasure=", context, "&DateFrom=&StartPeriod=&DateTo=&OpponentTeamID=0&",
              "ContextFilter=&RangeType=&Season=", paste0(season, "-", substr(season + 1, 3, 4)),
              "&AheadBehind=&PlayerID=", playerId,
              "&EndRange=&VsDivision=&PointDiff=&RookieYear=&GameSegment=&Month=0&ClutchTime=&",
              "StartRange=&EndPeriod=&SeasonType=Regular+Season&SeasonSegment=&",
              "GameID=", paste0("00", substr(season, 1, 1), substr(season, 3, 4), "0", gameId))
raw <- fromJSON(file = url)
game <-  data.frame(t(sapply(raw$resultSets[[1]]$rowSet, function(x) unlist(x))))
names(game) <- raw$resultSets[[1]]$headers
game <- filter(game, TEAM_NAME == team)
game$LOC_X <- as.numeric(levels(game$LOC_X))[game$LOC_X]
game$LOC_Y <- as.numeric(levels(game$LOC_Y))[game$LOC_Y]

accuracy <- as.data.frame.matrix(t(table(game$EVENT_TYPE, game$SHOT_ZONE_BASIC)))
accuracy$pct <- round(accuracy$`Made Shot` / rowSums(accuracy) * 100, 2)
loc <- game %>% group_by(SHOT_ZONE_BASIC) %>% summarize(x = mean(LOC_X), y = mean(LOC_Y))
accuracy <- cbind(accuracy, loc[, 2:3])
overall <- round(sum(accuracy$`Made Shot`) / 
                   (sum(accuracy$`Made Shot`) + sum(accuracy$`Missed Shot`)) * 100, 2)

gameInfoUrl <- paste0("http://stats.nba.com/stats/boxscoresummaryv2?GameID=",
                      paste0("00", substr(season, 1, 1), substr(season, 3, 4), "0", gameId))
rawInfo <- fromJSON(file = gameInfoUrl)
date <- substr(rawInfo$resultSets[[6]]$rowSet[[1]][[1]], 1, 10)
home <- rawInfo$resultSets[[6]]$rowSet[[1]][[7]]
away <- rawInfo$resultSets[[6]]$rowSet[[2]][[7]]
homePts <- rawInfo$resultSets[[6]]$rowSet[[1]][[23]]
awayPts <- rawInfo$resultSets[[6]]$rowSet[[2]][[23]]

png("20160305_knicks.png", width = 612, height = 575)
ggplot(game, aes(x = LOC_X * -1, y = LOC_Y * -1)) + 
  inset_raster(readPNG("nba-halfcourt.png"), xmin = -250, xmax = 250, ymin = -420, ymax = 50) +
  geom_point(aes(color = EVENT_TYPE), size = 6, alpha = 0.5) +
  scale_color_manual(labels = c("Make", "Miss"), values = c("GREEN", "RED")) +
  lims(x = c(-250, 250), y = c(-420, 50)) +
  coord_fixed() +
  geom_text(data = data.frame(x = (accuracy$x * -1), y = (accuracy$y * -1), pct = accuracy$pct), 
            aes(x = x, y = y, label = pct, fontface = 2), size = 6, color = "gray26") +
  theme(line = element_blank(), 
        rect = element_blank(), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.length = unit(0, "cm"), 
        legend.position = c(0.1, 0.1),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.margin = unit(0, "lines"), 
        plot.margin = unit(c(2, 0, 0, 0), "lines"),
        plot.title = element_text(size = 18, margin = margin(t = -20))
        )  +
  ggtitle(paste(date, away, awayPts, "@", home, homePts, "\n", team, "shots")) + 
  annotate("text", x = 200, y = -405, label = paste(overall, "Overall"), size = 6, fontface = 2)
dev.off()