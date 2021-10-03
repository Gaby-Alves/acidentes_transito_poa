# Carregando os dados-----------------------------------------------------------
url <- "https://dadosabertos.poa.br/api/3/action/datastore_search?resource_id=b56f8123-716a-4893-9348-23945f1ea1b9&limit=500000"
dados <- fromJSON(url) 
dados$result$records %>% as.data.frame -> df

# Observando o dados baixados
glimpse(df)
# Como as colunas não estão com os formatos corretos, temos que arrumar elas.
# Arrumando colunas-------------------------------------------------------------
# arrumando datas
df$data <- as.Date(df$data)
df$data_extracao <- as.Date(df$data_extracao)

# arrumando o id
df <- df %>%
  rename(id =`_id`)
df$idacidente <- as.character(df$idacidente)

# arrumando longitude e latitude
df$latitude <- as.character(df$latitude)
df$longitude <- as.character(df$longitude)

# arrumando o numero do logradouro
df$predial1 <- as.character(df$predial1)

# transformando em factor
# tipo_acident
df$tipo_acid <- as.factor(df$tipo_acid)

# dia da semana
df$dia_sem <- as.factor(df$dia_sem)

# noite_dia
df$noite_dia <- as.factor(df$noite_dia)

#  regiao
df$regiao <- as.factor(df$regiao)

# consorcio
df$consorcio  <- as.factor(df$consorcio)


#Arrumando hora
df$hora <- chron(times = df$hora)

# conferindo o dataset arrumado
glimpse(df)


# Filtrando datas  estranhas ---------------------------------------------------

df <- df %>%
  filter(data <= now())
# Conhecendo a base-------------------------------------------------------------
# Dimensão do data set 

cat("O dataset contém", nrow(df)," linhas e", ncol(df)," colunas.","\n",
    "Com dados a partir de",
    as.character(min(df$data)))

# Criando um for para descrever as colunas do dataset.
descricao <- table(sapply(df,class))
names(descricao)
for(i in seq_along(descricao)) {
  cat("Há ", descricao[i], "colunas do tipo ", names(descricao)[i],"\n")
}

# Verificando todo o data set
glimpse(df)

# Links utilizados para essa sessão.
#https://analisereal.com/2016/02/27/loops-no-r-usando-o-for-2/
#https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r
#https://stackoverflow.com/questions/37702222/how-to-count-no-of-data-types-in-a-data-frame/37702266


# Especificação do problema de pesquisa ----------------------------------------
cat("O propósito deste projeto é a criação de um modelo capaz de prever a quantidade
    de feridos por acidente de trânsito utilizando os dados de mobilidade da cidade
    de Porto Alegre.")

# Conhecendo um resumo dos dados.
summary(df) 

ggplot(df,
aes(x=feridos))+
geom_histogram(bins = 20, fill = "#440154FF")+
ylab("Contagem")+
xlab("Feridos")+
theme_bw()+
facet_wrap(~tipo_acid, scales = "free")

ggplotly(ggplot(df, aes(x=data, y=sum(feridos))) +
  geom_line())
      
      

ggplot(df,
       aes(x=feridos))+
  geom_histogram(bins = 25, fill = "#440154FF")+
  ylab("Contagem")+
  xlab("Feridos")+
  theme_bw()

df %>%
  group_by(feridos) %>%
  ggplot(aes(x=data, y=feridos))+
           geom_line()
