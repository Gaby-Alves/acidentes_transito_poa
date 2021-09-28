library(jsonlite)
library(tidyverse)

url <- "https://dadosabertos.poa.br/api/3/action/datastore_search?resource_id=b56f8123-716a-4893-9348-23945f1ea1b9&limit=500000"


teste2 <- jsonlite::fromJSON(url) 

teste2$result$records %>% as.data.frame -> df

str(df)

df$data_extracao <- as.Date(df$data_extracao)
df$data <- as.Date((df$data))
glimpse(df)
