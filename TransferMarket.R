require(rvest)
require(selectr)
require(xml2)
require(dplyr)
require(tidyr)
require(stringr)

getwd()
#C:/Users/gfabrici/Documents/Cursos/Aleatorios/API_Football_ORG/API_Football_Test

cod_england_TM <- read.csv('cod_england_TM.csv', sep = ';',stringsAsFactors = FALSE)

PastSeasons <- seq(2016,2018,1)

#Gerando vetor de Clubes da Premiere League desde 2010 

Clubs_full <- NULL

for (j in PastSeasons) {
  
url = paste("https://www.transfermarkt.pt/premier-league/startseite/wettbewerb/GB1/plus/?saison_id=", j, sep='')


scraped_page <- read_html(url)

clubs <- scraped_page %>% html_nodes("#yw1 .no-border-links") %>% html_text() %>% as.character()
clubs <- clubs[seq(1, length(clubs), by = 2)] %>%  
         tolower() %>% 
         trimws("r", whitespace = "[\\h\\v]") 

clubs <- gsub(' ',"-", clubs, fixed = TRUE)

clubs <- data.frame(clubs)

clubs$Season <- j 

Clubs_full <- bind_rows(Clubs_full,clubs)

}

Clubs_full <- left_join(Clubs_full ,cod_england_TM , by = c("clubs" = "Club"))


####################################

# Criando tabela de jogadores por Clube e temporada desde 2010

Players <- NULL

for (j in seq(1,60,1)) {

    
url = paste("https://www.transfermarkt.pt/", Clubs_full$clubs[j], "/kader/verein/", Clubs_full$Cod[j], "/plus/1/galerie/0?saison_id=", Clubs_full$Season[j], sep='')
  
scraped_page <- read_html(url)
  
PlayerNames  <- scraped_page %>% html_nodes(".spielprofil_tooltip") %>% html_text() %>% as.character()

  
Position     <- scraped_page %>% html_nodes(".inline-table tr+ tr td")       %>% html_text() %>% as.character()
Number       <- scraped_page %>% html_nodes(".rn_nummer")       %>% html_text() %>% as.character()
Birth        <- scraped_page %>% html_nodes(".posrela+ .zentriert") %>% html_text() %>% as.character()
Altura       <- scraped_page %>% html_nodes("td:nth-child(6)")   %>% html_text() %>% as.character()
Pe_preferido <- scraped_page %>% html_nodes("td:nth-child(7)")   %>% html_text() %>% as.character()
data_chegada <- scraped_page %>% html_nodes("td:nth-child(8)")   %>% html_text() %>% as.character()
Valor        <- scraped_page %>% html_nodes(".rechts.hauptlink") %>% html_text() %>% as.character()
  
  
df1 <- data.frame(PlayerNames)

df1 <- separate(df1, PlayerNames, into = c("Name", "LastName"), sep = "(?<=^\\S{0,1000})\\s+")

df1 <- df1 %>% filter(!grepl(".", Name, fixed = TRUE))
  
df1 <-distinct(df1, Name, LastName, .keep_all = TRUE)
  
df2 <- data.frame(Position, Number, Birth, Altura, Pe_preferido, data_chegada, Valor)
  
df <- cbind(df1,df2)
  
df$season <- Clubs_full$Season[j]
df$clube <- Clubs_full$clubs[j]

Players <- bind_rows(Players,df)
}

##############################
##########Tratamento dos dados
##############################


#Birth

Players <- Players %>% separate(Birth, into = c("BirthDate", "age"), sep = "(?<=^\\S{0,1000})\\s+")
     
Players$age <- gsub('(','', Players$age, fixed = TRUE)
Players$age <- as.numeric(gsub(')','', Players$age, fixed = TRUE))

#BirthDate

Players$BirthDate <- as.Date(Players$BirthDate, '%d/%m/%Y')

#Altura

Players$Altura <- gsub(' m','', Players$Altura, fixed = TRUE)
Players$Altura <- as.numeric(gsub(',','.', Players$Altura, fixed = TRUE))

#DataChegada

Players$data_chegada <- as.Date(Players$data_chegada, '%d/%m/%Y')


#Valor

Players$Valor_num <- Players$Valor

Players <- Players %>% 
           mutate(Valor_num = gsub("€", "", Valor)) %>% 
           mutate(Valor_num = gsub("M", "", Valor_num)) %>%
           mutate(Valor_num = gsub(",", ".", Valor_num)) %>%
           mutate(Valor_num = gsub("mil", "", Valor_num)) %>%
           mutate(Valor_num = as.numeric(gsub("^\\s+|\\s+$", "", Valor_num))) %>%  
           mutate(Valor_num = case_when(grepl("M", x$Valor) == TRUE ~ Valor_num,
                              grepl("mil", x$Valor) == TRUE ~ Valor_num/1000,
                              TRUE ~ 0))


