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
df$dia_sem <- factor(df$dia_sem, levels = c("SEGUNDA-FEIRA", "TERÇA-FEIRA", "QUARTA-FEIRA", "QUINTA-FEIRA", "SEXTA-FEIRA", "SÁBADO", "DOMINGO"))

# noite_dia
df$noite_dia <- as.factor(df$noite_dia)

#  regiao
df$regiao <- as.factor(df$regiao)

# consorcio
df$consorcio  <- as.factor(df$consorcio)


#Arrumando hora
df$hora <- chron(times = df$hora)

# predial as factor
df$predial1 <- as.factor(df$predial1)
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
  group_by(ano) %>% # agrupando por ano
  summarise(n_feridos = sum(feridos), # pegando as contagens
            n_acidentes = n_distinct(idacidente)) %>%
  mutate(perc_feridos = n_feridos/sum(n_feridos)*100, # fazendo as %
         perc_acident = n_acidentes/sum(n_acidentes)*100,
         fer_vs_acid=(n_feridos/n_acidentes)) # relação feridos acidades

kable(contagem_ano)%>%
kable_styling(bootstrap_options = "condensed", full_width = T) # kable com visualização

ggplot(contagem_ano) +
  geom_line(aes(x=ano, y=n_feridos,color="Feridos")) + #plotando line com feridos, usndo color como label
  geom_point(aes(x=ano, y=n_feridos))+ # plotando point com feridos
  geom_text_repel(aes(x=ano, y=n_feridos,label=n_feridos)) + #trazendo os valores com ggrepel para evitar sobreposição
  geom_line(aes(x=ano, y=n_acidentes, color="Acidentes"))+ # plotando line com acidentes, usando color como label
  geom_point(aes(x=ano, y=n_acidentes)) + # plotandos os pontos com acidentes
  geom_text_repel(aes(x=ano, y=n_acidentes, label=n_acidentes ))+ # ggrepel
  scale_color_manual(name="Número", values = c("Feridos" = "#eb8055ff",
                                               "Acidentes" = "#593d9cff"))+ #alterando os labels e cores
  ylab("Contagem") +
  xlab("Ano") + 
  theme_bw()+
  theme(legend.position = "top")

#https://stackoverflow.com/questions/40833809/add-legend-to-geom-line-graph-in-r

# Visualizando feridos por ano

contagem_mes <- df %>% 
  mutate(mes = month(data,label = T)) %>% #selecionando mês com nome
  group_by(mes) %>% #agrupando por mes 
  summarise(n_feridos = sum(feridos), #contando os valores
            n_acidentes = n_distinct(idacidente)) %>% 
  melt(id.vars="mes") # mudando a base para usar no ggplot


ggplot(contagem_mes) +
  geom_col(aes(x=mes, y= value, fill = variable), position = "dodge") + 
  theme_bw() +
  xlab("Mês") +
  ylab("Contagem")+
  scale_fill_manual(name="Número", # Nome do label
                      labels= c("Feridos",
                                "Acidentes"),  # Renomeando os valores
                      values = c("n_feridos" = "#eb8055ff",
                                "n_acidentes" = "#593d9cff"))+ # colorindo as series'
  theme(legend.position = "top") 


#https://stackoverflow.com/questions/13239499/how-to-replace-numeric-dates-with-month-names-for-a-column-in-r/13239837
# https://stackoverflow.com/questions/42820677/ggplot-bar-plot-side-by-side-using-two-variables -- para reshape2
# https://stackoverflow.com/questions/49909285/how-to-change-the-color-bar-of-chart-bars-obtained-with-ggplot2 cor do plot


# Feridos por dia da semana

contagem_dia_sem <- df %>%
  group_by(dia_sem) %>%
  summarise(n_feridos = sum(feridos),
            n_acidente = n_distinct(idacidente)) %>%
  melt(id.vars = "dia_sem") %>%
  mutate(dia_sem_reorder = fct_relevel(dia_sem,  "DOMINGO","SÁBADO", "SEXTA-FEIRA", "QUINTA-FEIRA", "QUARTA-FEIRA", "TERÇA-FEIRA","SEGUNDA-FEIRA"))


ggplot(contagem_dia_sem) +
  geom_col(aes(x=dia_sem_reorder, y= value, fill = variable), position = "dodge") +
  geom_text(aes(x=dia_sem_reorder, y = value, label = value, group = variable),
            position = position_dodge(width = .9), hjust = 1, size = 3,
            inherit.aes = TRUE)+
  theme_bw() +
  scale_fill_manual(name = "Número",
                    labels= c("Feridos",
                              "Acidentes"),  # Renomeando os valores
                    values = c("n_feridos" = "#eb8055ff",
                               "n_acidente" = "#2A788EFF"))+ # colorindo as series'
  theme(legend.position = "top")+
  ylab("Contagem") +
  xlab("Dia da Semana")+
  coord_flip() 

# https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html -- trabalhando com factors  
# https://stackoverflow.com/questions/6017460/position-geom-text-on-dodged-barplot -- trabalhando com text e ggplot
# https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want -- ggplot text

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



# MODELANDO --------------------------------------------------------------------
# feridos = tipo de acidente + turno do dia + regiao 

# Dummizando

df_selected <- df %>%
  select(idacidente, data, feridos, auto:outro, ups, tipo_acid, dia_sem, noite_dia, regiao)
df_dummizado <- 
  dummy_columns(.data =df_selected, 
                select_columns =c("tipo_acid", "noite_dia", "regiao", "dia_sem", "ups"),
                remove_selected_columns = T,
                remove_most_frequent_dummy = T)

#limpando o nome das colunas
colnames(df_dummizado)[18] <- "tipo_acid_COLISAO" 
colnames(df_dummizado)[34] <-  "dia_sem_TERCA-FEIRA"
colnames(df_dummizado)[32] <- "dia_sem_SABADO"
colnames(df_dummizado)[20] <- "tipo_acid_INCENDIO"
colnames(df_dummizado)[21] <- "tipo_acid_NAO_CADASTRADO"
colnames(df_dummizado)
# MODELAGEM PARA DADOS DE CONTAGEM --------------------------------------------
# Primeiro GLM Poisson

modelo_poisson <- glm(formula = feridos ~ . -idacidente -data,
                data=df_dummizado,
                family = "poisson")
summary(modelo_poisson)

modelo_poisson_step <- step(modelo_poisson, k = qchisq(p = 0.05, df = 1, lower.tail = F))

overdisp(x= df_dummizado,
         dependent.position = 3,
         predictor.position = 4:36)
summary(modelo_poisson_step)
