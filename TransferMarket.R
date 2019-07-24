#Bibliotecas

require(rvest)
require(selectr)
require(xml2)
require(dplyr)
require(tidyr)
require(stringr)

setwd("D:/01_Projetos/04_Premiere_League/API_Football_Test")

getwd()
#C:/Users/gfabrici/Documents/Cursos/Aleatorios/API_Football_ORG/API_Football_Test

cod_england_TM <- read.csv('cod_england_TM.csv', sep = ',',stringsAsFactors = FALSE)

PastSeasons <- seq(2010,2018,1)

#Gerando vetor de Clubes da Premiere League desde 2010 

TeamsFull <- NULL

for (j in PastSeasons) {
  
url = paste("https://www.transfermarkt.pt/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=", j, sep='')


scraped_page <- read_html(url)

Teams <- scraped_page %>% html_nodes("#yw1 .no-border-links") %>% html_text() %>% as.character()
Teams <- Teams[seq(1, length(Teams), by = 2)] %>%  
         tolower() %>% 
         trimws("r", whitespace = "[\\h\\v]") 

Teams <- gsub(' ',"-", Teams, fixed = TRUE)

Teams <- data.frame(Teams)

Teams$Season <- j 

TeamsFull <- bind_rows(TeamsFull,Teams)

}

TeamsFull <- left_join(TeamsFull ,cod_england_TM , by = c("Teams" = "Club"))


####################################

# Criando tabela de jogadores por Clube e temporada desde 2010

Players <- NULL

for (j in seq(1,nrow(TeamsFull),1)) {

    
url = paste("https://www.transfermarkt.pt/", TeamsFull$Teams[j], "/kader/verein/", TeamsFull$Cod[j], "/plus/1/galerie/0?saison_id=", TeamsFull$Season[j], sep='')
  
scraped_page <- read_html(url)
  
PlayerNames  <- scraped_page %>% html_nodes(".spielprofil_tooltip") %>% html_text() %>% as.character()


  
Position <- scraped_page %>% html_nodes(".inline-table tr+ tr td")       %>% html_text() %>% as.character()
Number   <- scraped_page %>% html_nodes(".rn_nummer")       %>% html_text() %>% as.character()
Birth    <- scraped_page %>% html_nodes(".posrela+ .zentriert") %>% html_text() %>% as.character()
Height   <- scraped_page %>% html_nodes("td:nth-child(6)")   %>% html_text() %>% as.character()
Foot     <- scraped_page %>% html_nodes("td:nth-child(7)")   %>% html_text() %>% as.character()
Joined  <- scraped_page %>% html_nodes("td:nth-child(8)")   %>% html_text() %>% as.character()
Value   <- scraped_page %>% html_nodes(".rechts.hauptlink") %>% html_text() %>% as.character()
  
  
df1 <- data.frame(PlayerNames)

df1 <- separate(df1, PlayerNames, into = c("Name", "LastName"), sep = "(?<=^\\S{0,1000})\\s+")

df1 <- df1 %>% filter(!grepl(".", Name, fixed = TRUE))
  
df1 <-distinct(df1, Name, LastName, .keep_all = TRUE)
  
df2 <- data.frame(Position, Number, Birth, Height, Foot, Joined, Value)
  
df <- cbind(df1,df2)
  
df$Season <- TeamsFull$Season[j]
df$Team <- TeamsFull$Club_Match[j]

Players <- bind_rows(Players,df)
}

##############################
##########Tratamento dos dados
##############################


#Birth

Players <- Players %>% separate(Birth, into = c("BirthDate", "Age"), sep = "(?<=^\\S{0,1000})\\s+")
     
Players$Age <- gsub('(','', Players$Age, fixed = TRUE)
Players$Age <- as.numeric(gsub(')','', Players$Age, fixed = TRUE))

#BirthDate

Players$BirthDate <- as.Date(Players$BirthDate, '%d/%m/%Y')

#Height

Players$Height <- gsub(' m','', Players$Height, fixed = TRUE)
Players$Height <- as.numeric(gsub(',','.', Players$Height, fixed = TRUE))

#DataChegada

Players$Joined <- as.Date(Players$Joined, '%d/%m/%Y')


#Value

Players$Value_num <- Players$Value

Players <- Players %>% 
           mutate(Value_num = gsub("€", "", Value)) %>% 
           mutate(Value_num = gsub("M", "", Value_num)) %>%
           mutate(Value_num = gsub(",", ".", Value_num)) %>%
           mutate(Value_num = gsub("mil", "", Value_num)) %>%
           mutate(Value_num = as.numeric(gsub("^\\s+|\\s+$", "", Value_num))) %>%  
           mutate(Value_num = case_when(grepl("M", Players$Value) == TRUE ~ Value_num,
                              grepl("mil", Players$Value) == TRUE ~ Value_num/1000,
                              TRUE ~ 0))


##################################################################################################################

#####Pegando Tabela da Premiere League 2010 - 2018

Standings <- NULL
Leaders <- NULL

for (j in PastSeasons) {
  
  url = paste("https://www.transfermarkt.com.br/premier-league/tabelle/wettbewerb/GB1/saison_id/", j, sep='')

scraped_page <- read_html(url)

Position  <- scraped_page %>% html_nodes(".responsive-table .rechts.hauptlink") %>% html_text() %>% as.character() %>% trimws("r", whitespace = "[\\h\\v]") %>% as.numeric()
Team    <- scraped_page %>% html_nodes(".responsive-table .no-border-links") %>% html_text() %>% as.character() %>% 
            trimws("r", whitespace = "[\\h\\v]") %>% trimws("l", whitespace = "[\\h\\v]")
Games    <- scraped_page %>% html_nodes(".no-border-links+ .zentriert a") %>% html_text() %>% as.numeric()
Win <- scraped_page %>% html_nodes(".zentriert:nth-child(5) a") %>% html_text() %>% as.numeric()
Draw   <- scraped_page %>% html_nodes(".zentriert:nth-child(6) a") %>% html_text() %>% as.numeric()
Lose <- scraped_page %>% html_nodes(".zentriert:nth-child(7) a") %>% html_text() %>% as.numeric()
Goals     <- scraped_page %>% html_nodes("td:nth-child(8)") %>% html_text() %>% as.character()
GoalsDif    <- scraped_page %>% html_nodes("td:nth-child(9)") %>% html_text() %>% as.numeric()
Points   <- scraped_page %>% html_nodes(".zentriert:nth-child(10)") %>% html_text() %>% as.numeric()


TeamLeader <- scraped_page %>% html_nodes(".large-8 .table-header+ table .hauptlink") %>% html_text() %>% as.character() %>% 
              trimws("r", whitespace = "[\\h\\v]") %>% trimws("l", whitespace = "[\\h\\v]")
RoundsLeader <- scraped_page %>% html_nodes(".large-8 .table-header+ table .hauptlink+ .zentriert") %>% html_text() %>% as.numeric()
FistLeader  <- scraped_page %>% html_nodes(".zentriert+ td:nth-child(4)") %>% html_text() %>% as.character()
LastLider  <- scraped_page %>% html_nodes(".large-8 .table-header+ table .zentriert:nth-child(5)") %>% html_text() %>% as.character()
LastLider  <- c(LastLider, vector(mode="character", length=length(FistLeader)-length(LastLider)))


df1 <- data.frame(Position, Team, Games, Win, Draw, Lose, Goals, GoalsDif, Points, stringsAsFactors = FALSE)
df1 <- df1 %>% separate(Goals, into = c("GolsPro", "GolsContra"), sep = ":")
df1$Season <- j 

Standings <- bind_rows(Standings, df1)


df2 <- data.frame(TeamLeader, RoundsLeader, FistLeader, LastLider, stringsAsFactors = FALSE)
df2$Season <- j

Leaders <- bind_rows(Leaders, df2)
}

############################################################################################


########################
#Jogos Premiere League
#######################

Matches <- NULL

for (j in seq(1,nrow(TeamsFull),1)) {

  url = paste("https://www.transfermarkt.co.uk/", TeamsFull$Teams[j], "/spielplan/verein/", TeamsFull$Cod[j], "/saison_id/", TeamsFull$Season[j], "/plus/1#GB1", sep='')
  

scraped_page <- read_html(url)

MatchDay  <- scraped_page %>% html_nodes("td.zentriert:nth-child(1)") %>% html_text() %>% as.character() %>% trimws("r", whitespace = "[\\h\\v]")%>% trimws("l", whitespace = "[\\h\\v]") 
Date      <- scraped_page %>% html_nodes(".responsive-table td:nth-child(2)") %>% html_text() %>% as.character()
Time      <- scraped_page %>% html_nodes(".responsive-table td:nth-child(3)") %>% html_text() %>% as.character()
HomeTeam  <- scraped_page %>% html_nodes(".no-border-links:nth-child(5)") %>% html_text() %>% as.character() %>% 
             trimws("r", whitespace = "[\\h\\v]") %>% trimws("l", whitespace = "[\\h\\v]")
AwayTeam  <- scraped_page %>% html_nodes(".no-border-links:nth-child(7)") %>% html_text() %>% as.character() %>% 
             trimws("r", whitespace = "[\\h\\v]") %>% trimws("l", whitespace = "[\\h\\v]")

Formation  <- scraped_page %>% html_nodes(".zentriert:nth-child(8)") %>% html_text() %>% as.character()
Manager    <- scraped_page %>% html_nodes("td:nth-child(9)") %>% html_text() %>% as.character()
Attendence <- scraped_page %>% html_nodes(".responsive-table td.rechts") %>% html_text() %>% as.numeric()
Result     <- scraped_page %>% html_nodes(".rechts+ td") %>% html_text() %>% as.character()



df1 <- data.frame(MatchDay, Date, Time, HomeTeam, AwayTeam, Formation, Manager, Attendence, Result, stringsAsFactors = FALSE)

df1$TeamMatches <- TeamsFull$Club_Match[j]
df1$Season <- TeamsFull$Season[j]

Matches <- bind_rows(Matches, df1)
}

PremiereMatches <- Matches %>% filter(MatchDay %in% as.character(seq(1,38,1)))

PremiereMatches <- PremiereMatches %>% separate(HomeTeam, c("HomeTeam", "HomeTeamPos"), "[(]" ) %>%
                   mutate(HomeTeamPos = gsub(".)", "", HomeTeamPos)) %>%
                   mutate(HomeTeam = gsub("^\\s+|\\s+$", "", HomeTeam))

PremiereMatches <- PremiereMatches %>% separate(AwayTeam, c("AwayTeam", "AwayTeamPos"), "[(]" ) %>%
                   mutate(AwayTeamPos = gsub(".)", "", AwayTeamPos)) %>% 
                   mutate(AwayTeam = gsub("^\\s+|\\s+$", "", AwayTeam))

PremiereMatches <- PremiereMatches %>% separate(Result, c("HomeTeamScore", "AwayTeamScore"), "[:]" ) %>% 
                   mutate(HomeTeamScore = as.numeric(HomeTeamScore)) %>%
                   mutate(AwayTeamScore = as.numeric(AwayTeamScore))

PremiereMatches$WinnerHA <- NULL

PremiereMatches <- PremiereMatches %>% mutate( WinnerHA = case_when(AwayTeamScore > HomeTeamScore ~ "AWAY",
                                                 AwayTeamScore < HomeTeamScore ~ "HOME",
                                                 AwayTeamScore == HomeTeamScore ~ "DRAW",
                                                 TRUE ~ "X"))

PremiereMatches$WinnerTeam <- NULL

PremiereMatches <- PremiereMatches %>% mutate( WinnerTeam = case_when(AwayTeamScore > HomeTeamScore ~ AwayTeam,
                                                                    AwayTeamScore < HomeTeamScore ~ HomeTeam,
                                                                    AwayTeamScore == HomeTeamScore ~ "DRAW",
                                                                    TRUE ~ "X"))


PremiereMatches <- PremiereMatches[with(PremiereMatches, order(Season, MatchDay, HomeTeam)), ]

unique(PremiereMatches$HomeTeam)
unique(PremiereMatches$Team)


url = "https://www.transfermarkt.com.br/leeds-united-fc/spielplan/verein/399/saison_id/2019/plus/1#GB2"
scraped_page <- read_html(url)

Teams <- scraped_page %>% html_nodes(".no-border-links:nth-child(5)") %>% html_text() %>% as.character()


unique(Teams)


