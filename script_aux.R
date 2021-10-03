
dados$result$records %>% as.data.frame -> df


class(df$data)
df$data <- substr(df$data,  1,10)


# Arrumando as datas
df$data_extracao <- as.Date(df$data_extracao)
df$data <- as.Date(df$data) # base  R
df$data <- ymd_hms(df$data, tz = NULL) #lubridate
df$data <- anydate(df$data) # anytime package


df$hora <- chron(times = df$hora)



ggplot(df,
       aes(x=data, y= feridos)) +
  geom_line()



df <- df %>%
  filter(data <= now())


