#Instalando os pacotes----------------------------------------------------------


library(jsonlite)
library(tidyverse)
library(lubridate)
library(chron)

# Carregando os dados-----------------------------------------------------------
url <- "https://dadosabertos.poa.br/api/3/action/datastore_search?resource_id=b56f8123-716a-4893-9348-23945f1ea1b9&limit=500000"
dados <- fromJSON(url) 
dados$result$records %>% as.data.frame -> df

# Arrumando colunas-------------------------------------------------------------

glimpse(df)

# Arrumando as datas
df$data_extracao <- as.Date(df$data_extracao)
df$data <- as.Date((df$data))

# arrumando o id
df$`_id`<- as.character(df$`_id`)

df <- df %>%
  rename(id =`_id`)

# arrumando id acidade
df$idacidente <- as.character(df$idacidente)
df$hora <- chron(times = df$hora)

# arrumando longitude e latitude
df$latitude <- as.character(df$latitude)
df$longitude <- as.character(df$longitude)

# arrumando o numero do logradouro
df$predial1 <- as.character(df$predial1)

# Conhecendo a base-------------------------------------------------------------
# Dimensão
cat("O dataset contém", nrow(df)," linhas e", ncol(df)," colunas.")



descricao <- table(sapply(df,class))
descricao[1]
type(descricao)
names(descricao)
for(i in seq_along(descricao)) {
  cat("Há ", descricao[i], "colunas do tipo ", names(descricao)[i],"\n")
}

glimpse(df)


