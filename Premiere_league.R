##API de dados de futebol

# This package is required for Accessing APIS (HTTP or HTTPS URLS from Web)
library(httr)
#This package exposes some additional functions to convert json/text to data frame
library(rlist)
#This package exposes some additional functions to convert json/text to data frame
library(jsonlite)
#This library is used to manipulate data
library(dplyr)




#### Criando Tabela com Campeões da Premiere League

camp_premier <- GET("http://api.football-data.org/v2/competitions/2021",
                  add_headers("X-Auth-Token"="7795a61302584e5eabff14f977e6c202"))


camp_premier <- content(camp_premier, as = "text")
camp_premier <- fromJSON(camp_premier)

df.camp_premier <- flatten(as.data.frame(camp_premier['seasons']))


#### Criando Tabela com classificação Premiere League

for (i in seq(2017,2018,1)) {
i<- 2010
url <- paste("http://api.football-data.org/v2/competitions/2021/standings?season=",i,"&standingType=TOTAL", sep = '')  
teste <- GET("http://api.football-data.org/v2/competitions/2021/standings?season=2018&standingType=TOTAL"
  , add_headers("X-Auth-Token"="7795a61302584e5eabff14f977e6c202"))

teste<- content(teste, as = "text")
teste <- fromJSON(teste)

teste <- teste[['standings']]['table']

df.teste <- flatten(as.data.frame(teste[[1]][1]))

df.teste$ano <- i

#total <- bind_rows(total,df.teste)

}

total <- NULL



