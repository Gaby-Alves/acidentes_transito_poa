url <- "https://dadosabertos.poa.br/api/3/action/datastore_search?resource_id=42f022be-2723-4502-8a9a-05b282bf3ac5"


library(jsonlite)
library(dplyr)

teste2 <- jsonlite::fromJSON(url) 

teste2$result$records %>% as.data.frame -> df
