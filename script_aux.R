
dados$result$records %>% as.data.frame -> df


class(df$data)
df$data <- substr(df$data,  1,10)

df$data <- as.Date(df$data)
df$data <- anydate(df$data)
df$data <- ymd(df$data)
ggplot(df,
       aes(x=data, y= feridos)) +
  geom_line()
