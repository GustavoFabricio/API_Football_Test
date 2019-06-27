  #####################################################
 # Simple Analysis of Season 2018-19 Premiere League #
#####################################################

##API de Football data

# This package is required for Accessing APIS (HTTP or HTTPS URLS from Web)
library(httr)
#This package exposes some additional functions to convert json/text to data frame
library(rlist)
#This package exposes some additional functions to convert json/text to data frame
library(jsonlite)
#This library is used to manipulate data
library(dplyr)
#This library is used to plots
library(ggplot2)
#
library(tidyr)
library(reshape)
library(lattice)  
  
#### Creating df with the Premiere League Champions ####

#GET the API
PL.Champions <- GET("http://api.football-data.org/v2/competitions/2021",
                  add_headers("X-Auth-Token"="7795a61302584e5eabff14f977e6c202"))

#Convert API in list
PL.Champions <- content(PL.Champions, as = "text")
PL.Champions <- fromJSON(PL.Champions)

#Convert list in Data frame
df.PL.Champions <- flatten(as.data.frame(PL.Champions['seasons']))


#### Creating df with the Premiere League Standing Season 2018-19####

#GET the API
PL.Standings_S18 <- GET("http://api.football-data.org/v2/competitions/2021/standings?season=2018&standingType=TOTAL"
  , add_headers("X-Auth-Token"="7795a61302584e5eabff14f977e6c202"))

#Convert API in list
PL.Standings_S18 <- content(PL.Standings_S18, as = "text")
PL.Standings_S18 <- fromJSON(PL.Standings_S18)

#Convert list in Data frame
df.PL.Standings_S18 <- PL.Standings_S18[['standings']][['table']][[1]]



#### Creating df with the Premiere League Matches Season 2018-19 ####

#GET the API
PL.Matches_S18 <- GET("http://api.football-data.org/v2/competitions/2021/matches?season=2018"
                , add_headers("X-Auth-Token"="7795a61302584e5eabff14f977e6c202"))

#Convert API in list
PL.Matches_S18 <- content(PL.Matches_S18, as = "text")
PL.Matches_S18 <- fromJSON(PL.Matches_S18)

#Convert list in Data frame
df.PL.Matches_S18 <- PL.Matches_S18[['matches']]



#### Clustering Teams ####
df.PL.Teams <- unnest(df.PL.Standings_S18$team)

##The choice are made by last season and promotion from Championship
df.PL.Teams$class1 <- case_when(df.PL.Teams$name %in% c("Manchester City FC","Liverpool FC","Chelsea FC","Tottenham Hotspur FC","Arsenal FC","Manchester United FC") ~ 'A',
                                df.PL.Teams$name %in% c("West Ham United FC","Leicester City FC","Everton FC","Burnley FC") ~ 'B',
                                df.PL.Teams$name %in% c("Fulham FC","Cardiff City FC","Wolverhampton Wanderers FC","Southampton FC") ~ 'D',
                                TRUE ~ 'C')


#Unnest all columns from df.PL.Matches_S18 (not the best way to do that)

season <- unnest(df.PL.Matches_S18$season)
colnames(season) <- c("season_id", "season_startDate", "season_endDate", "season_currentMatchday")

home <- unnest(df.PL.Matches_S18$homeTeam)
colnames(home) <- c("homeTeam_id", "homeTeam_name") 

away <- unnest(df.PL.Matches_S18$awayTeam)
colnames(away) <- c("awayTeam_id", "awayTeam_name")

score <- bind_cols(as.data.frame(x = df.PL.Matches_S18$score$winner),as.data.frame(x = df.PL.Matches_S18$score$duration))
colnames(score) <- c("winner", "duration")

score_half <- unnest(df.PL.Matches_S18$score$halfTime)
colnames(score_half) <- c("HalfScore_homeT", "HalfScore_awayT")

score_full <- unnest(df.PL.Matches_S18$score$fullTime)
colnames(score_full) <- c("FullScore_homeT", "FullScore_awayT")

df.PL.Matches_S18_v2 <- df.PL.Matches_S18 %>%
                     select(id,utcDate,status,matchday,stage,group,lastUpdated)

df.PL.Matches_S18_v2 <- bind_cols(df.PL.Matches_S18_v2,season, home, away,score , score_half,score_full)

#Remove useless data
rm(season)
rm(home)
rm(away)
rm(score)
rm(score_half)
rm(score_full)


#Join the teams class
df.PL.Matches_S18_v2 <- left_join(df.PL.Matches_S18_v2 ,df.PL.Teams , by = c("homeTeam_id" = "id"))
df.PL.Matches_S18_v2 <- select(df.PL.Matches_S18_v2,-c("crestUrl", "name"))
colnames(df.PL.Matches_S18_v2)[22] <- "homeTeam_class"
names(df.PL.Matches_S18_v2)

df.PL.Matches_S18_v2 <- left_join(df.PL.Matches_S18_v2 ,df.PL.Teams , by = c("awayTeam_id" = "id")) 
df.PL.Matches_S18_v2 <- select(df.PL.Matches_S18_v2,-c("crestUrl", "name"))
colnames(df.PL.Matches_S18_v2)[23] <- "awayTeam_class"
names(df.PL.Matches_S18_v2)


names(df.PL.Matches_S18_v2)


#Grouping the Teams class Matches and calculating the %
Matches_analysis <- df.PL.Matches_S18_v2 %>% 
                    group_by(winner, homeTeam_class, awayTeam_class) %>% 
                    summarise(qtd = n())

Matches_analysis <- df.PL.Matches_S18_v2 %>% 
  group_by(winner, homeTeam_class, awayTeam_class) %>% 
  summarise(
    qtd = n())

Matches_count <- df.PL.Matches_S18_v2 %>% 
  group_by(homeTeam_class, awayTeam_class) %>% 
  summarise(Tot = n())

Matches_analysis <- left_join(Matches_analysis ,Matches_count , by = c("homeTeam_class"="homeTeam_class","awayTeam_class" = "awayTeam_class"))

Matches_analysis$Pct <- Matches_analysis$qtd/Matches_analysis$Tot


#Pivot table (by Column winner keeping %)
Matches_analysis <- cast(Matches_analysis, homeTeam_class+awayTeam_class~winner)



### Preparing data for Heat Map
## Matrix 
m <-data.matrix(x, rownames.force = NA)
rownames(m)=c(paste(Matches_analysis$homeTeam_class, " x ", Matches_analysis$awayTeam_class ))


## Doing a colot Palette
col.l <- colorRampPalette(c('red','white', 'darkblue'))(30)

## Heat Map
levelplot(t(m),col.regions=col.l, xlab="Winner", ylab="Matches", main='Heat Map Winner by Team Class Matches',
          scales=list(x=list(cex=.5), y=list(cex=.5), xlab=list(cex=.1), ylab=list(cex=.1)))


