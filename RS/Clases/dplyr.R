library(readr)
library(dplyr)
library(tidyverse)
library(highcharter)

df <- read_delim("2010-2019-top.csv",";", 
                 escape_double = FALSE,
                 trim_ws = TRUE)
str(df)

glimpse(df)
names(df)[names(df) == "top genre"]  
names(df)[which(names(df) == "top genre")]
#rename(df, top_genre = `top genre`)

rename_with(df, ~(gsub(" ", "_", .x)))


#filtrabamos variables o columnas
head(select(df,artist, year))

df %>% 
  select(artist,year) %>%
  head()

df %>% 
  select(1,2) %>%
  head()


# quiero todas las columnas menos 1
df %>% 
  select(-X1)

#text a factores
df <- mutate_if(df, is.character, as.factor)
glimpse(df)


#filtrar
df %>%
  filter(year == 2010) %>%
  select(artist, title, year)


df %>% 
  select(year,artist)%>% 
  group_by(year) %>%
  summarise(n=n_distinct(artist))

sum(n_distinct(df$artist))

df %>% 
  select(title)%>% 
  summarise(n=n_distinct(title))

#cuantas canciones se repiten por año
df %>% 
  select(year,title)%>% 
  group_by(title) %>%
  summarise(count = n()) %>%
  filter(count > 1)


### que artistas han tenido mas de una cancion popular

df %>% 
  group_by(artist, title) %>% 
  count() %>% 
  filter(n>1) %>% 
  group_by(artist) %>% 
  summarise(artistas = n()) %>% 
  filter(artistas >1)
