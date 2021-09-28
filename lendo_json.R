#Instalando os pacotes

library(jsonlite)
library(tidyverse)


# Carregando os dados
url <- "https://dadosabertos.poa.br/api/3/action/datastore_search?resource_id=b56f8123-716a-4893-9348-23945f1ea1b9&limit=500000"
dados <- fromJSON(url) 
dados$result$records %>% as.data.frame -> df


# Arrumando as datas
glimpse(df)
df$data_extracao <- as.Date(df$data_extracao)
df$data <- as.Date((df$data))
glimpse(df)


# arrumando o id
df$`_id`<- as.character(df$`_id`)

df <- df %>%
  rename(id =`_id`)


