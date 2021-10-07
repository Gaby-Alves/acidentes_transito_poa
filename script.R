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

# Conhecendo um resumo dos dados------------------------------------------------
summary(df) 

# Subset para ver a quantidade de feridos por dia

# Criando uma coluna de ano
df <- df %>%
  mutate(ano = year(data))


# Observando a frequencia temporal dos feridos.
# Feridos por ano
contagem_ano <- df %>%
  group_by(ano) %>%
  summarise(n_feridos = sum(feridos),
            n_acidentes = n_distinct(idacidente)) %>%
  mutate(perc_feridos = n_feridos/sum(n_feridos)*100,
         perc_acident = n_acidentes/sum(n_acidentes)*100,
         fer_vs_acid=(n_feridos/n_acidentes))

kable(contagem_ano)%>%
kable_styling(bootstrap_options = "condensed", full_width = T)

ggplot(contagem_ano) +
  geom_line(aes(x=ano, y=n_feridos,color="Feridos")) +
  geom_point(aes(x=ano, y=n_feridos))+
  geom_text_repel(aes(x=ano, y=n_feridos,label=n_feridos)) +
  geom_line(aes(x=ano, y=n_acidentes, color="Acidentes"))+
  geom_point(aes(x=ano, y=n_acidentes)) +
  geom_text_repel(aes(x=ano, y=n_acidentes, label=n_acidentes ))+
  scale_color_manual(name="Número", values = c("Feridos" = "#eb8055ff",
                                               "Acidentes" = "#593d9cff"))+
  ylab("Contagem") +
  xlab("Ano") + 
  theme_bw()+
  theme(legend.position = "top")

#https://stackoverflow.com/questions/40833809/add-legend-to-geom-line-graph-in-r

# Visualizando feridos por ano

contagem_mes <- df %>%
  mutate(mes = month(data)) %>%
  group_by(mes) %>%
  summarise(n_feridos = sum(feridos),
            n_acidentes = n_distinct(idacidente)) %>%
  mutate(perc_feridos = n_feridos/sum(n_feridos)*100,
         perc_acid = n_acidentes/sum(n_acidentes)*100,
         fer_vs_acid = n_feridos/n_acidentes)

ggplot(contagem_mes) +
  geom_line(aes(x=mes, y=n_feridos,color="Feridos")) +
  geom_point(aes(x=mes, y=n_feridos))+
  geom_text_repel(aes(x=mes, y=n_feridos,label=n_feridos)) +
  geom_line(aes(x=mes, y=n_acidentes, color="Acidentes"))+
  geom_point(aes(x=mes, y=n_acidentes)) +
  geom_text_repel(aes(x=mes, y=n_acidentes, label=n_acidentes ))+
  scale_color_manual(name="Número", values = c("Feridos" = "#eb8055ff",
                                               "Acidentes" = "#593d9cff"))+
  ylab("Contagem") +
  xlab("Mês") + 
  theme_bw()+
  theme(legend.position = "top")


df %>%
  group_by(ano) %>%
  summarise(n_acidentes = n_distinct(idacidente)) %>%
  mutate(percent = n_acidentes/sum(n_acidentes)*100) %>%
  ggplot(aes(x=ano, y=n_acidentes)) +geom_line()




df %>%
  mutate(mes = month(data)) %>%
  group_by(data) %>%
  

  
df %>%
  group_by(data,noite_dia) %>%
  summarise(n_feridos=sum(feridos)) %>%
  ggplot(aes(data,n_feridos))+geom_line()+facet_wrap(~noite_dia) + theme_bw()

df %>%
  group_by(data,tipo_acid) %>%
  summarise(n_feridos=sum(feridos)) %>%
  ggplot(aes(data,n_feridos))+geom_line()+facet_wrap(~tipo_acid) + theme_bw()


df %>%
  group_by(data,noite_dia) %>%
  summarise(n_feridos=sum(feridos)) %>%
  ggplot(aes(x="",y=n_feridos))+geom_boxplot()+facet_wrap(~noite_dia) + theme_bw()
