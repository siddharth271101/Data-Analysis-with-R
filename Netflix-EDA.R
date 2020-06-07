library(tidyverse)# metapackage with lots of helpful functions
library(ggrepel)
library(vcd)
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(tm)

df_netflix =read.csv(file = 'netflix/netflix_titles.csv', na.strings = c("NA", ""), stringsAsFactors=F)
head(df_netflix)

glimpse(df_netflix)

df_netflix$date_added <- as.Date(df_netflix$date_added, format = "%B %d, %Y")

df_netflix%>% group_by(type) %>% mutate(count = n()) %>% 
  ggplot(aes(x =reorder(type,count))) +
  geom_bar(fill = c("dodgerblue2","firebrick2")) +
  labs(
    title = 'Content produced in each type'
  )

unique(df_netflix$duration)

to_remove <- c('1 Season','2 Seasons','5 Seasons','3 Seasons','7 Seasons','4 Seasons','8 Seasons','6 Seasons','9 Seasons','14 Seasons','10 Seasons','12 Seasons','15 Seasons','11 Seasons','13 Seasons')
df_netflix_d <- df_netflix[!(df_netflix$duration %in% to_remove), ] 
df_netflix_d$duration <- str_remove_all(df_netflix_d$duration, " min")
df_netflix_d <- transform(df_netflix_d, duration = as.numeric(duration))

ggplot(df_netflix_d,aes(x = duration)) +
  geom_density(  
    fill = 'steelblue',
    bw = 2.0,
    alpha = 0.7 ) +
  scale_x_continuous(limits = c(0,200),expand = c(0,0)) +
  geom_rug(alpha = 0.05)  +
  labs(
    title = 'Distribution of Movie duration'
  )


df_netflix_d%>%
  select(duration)%>%
  group_by(duration)%>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  top_n(10) %>%
  ggplot(aes(reorder(duration,count),count))+
  geom_col(fill = 'steelblue',color = 'black') +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = 'Top 10 Movies by duration'
  )


na_count <-sapply(df_netflix, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)     
na_count


getmode <- function(values) {
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}

df_netflix$rating[is.na(df_netflix$rating)] <- getmode(df_netflix$rating)


df_netflix=distinct(df_netflix,title,country,type,release_year, .keep_all= TRUE)
colnames(df_netflix)[colSums(is.na(df_netflix)) > 0]


df_netflix1 <- df_netflix[!is.na(df_netflix$date_added), ]


df_by_date <- df_netflix1 %>% 
  group_by(date_added,type) %>% 
  summarise(addedToday = n()) %>% 
  ungroup() %>%
  group_by(type) %>% 
  mutate(Total_Number_of_Shows = cumsum(addedToday), label = if_else(date_added == max(date_added,na.rm = T), as.character(type), NA_character_))


df_by_date  %>% 
  ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + 
  geom_line(size = 1) + 
  theme_bw(base_size = 20) + 
  scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = label), size = 8,na.rm = TRUE, nudge_y = 100) +
  labs(
    title = 'Content added over the years'
  )


posn_d <- position_dodge(width = 0.6)
df_netflix2 <- df_netflix[!is.na(df_netflix$date_added), ]

df_netflix2%>%
  mutate(month_added = month(as.POSIXlt(df_netflix2$date_added, format="%B %d, %Y")))%>%
  
  mutate(count = n()) %>%
  ggplot(aes(reorder(month_added,month_added),fill = type))+
  geom_bar(position = posn_d,alpha = 0.6) +
  labs(
    title = 'Monthly produce of content'
  )


unique(df_netflix$rating)


# # df_by_rating_full <- 
posn_d <- position_dodge(width = 0.5)
df_netflix%>% group_by(rating) %>% mutate(count = n()) %>% 
  ggplot(aes(x =reorder(rating,count),fill = type)) +
  geom_bar(position = posn_d,alpha = 0.6) +
  labs(
    title = 'Amount of Content produced in each rating'
  )




df_netflix %>% 
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in)%>% group_by(listed_in) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% top_n(20)%>%
  ggplot( aes(x = count, y = reorder(listed_in,count)))+
  geom_point() +
  labs(
    title = 'Content produced in each genre'
  )



df_netflix1 <- df_netflix[!is.na(df_netflix$country), ]

df_netflix1 %>% 
  mutate(country = strsplit(as.character(country), ", ")) %>%
  unnest(country)%>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  top_n(10)%>%
  ggplot(aes(reorder(country,count),count))+
  geom_col(fill = '#003366', color = '#add8e6')+
  theme(
    panel.grid = element_blank(),
  ) +
  labs(
    title = 'Countries with maximum content creation'
  )


df_netflix3 <- df_netflix[!is.na(df_netflix$cast), ]




df_netflix3 %>% 
  mutate(cast = strsplit(as.character(cast), ", "))%>%
  unnest(cast)%>%
  group_by(cast) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)%>%
  ggplot(aes(reorder(cast,count),count))+
  geom_col(fill = "coral")+
  theme(
    panel.grid = element_blank()
  ) +
  coord_flip()   +
  labs(
    title = 'Actors with most movies'
  )



df_netflix4 <- df_netflix[!is.na(df_netflix$director), ]
df_netflix4 %>% 
  mutate(director = strsplit(as.character(director), ", ")) %>%
  unnest(director)%>%
  group_by(director) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)%>%
  ggplot(aes(reorder(director,count),count))+
  geom_col(fill = '#a678de')+
  theme(
    panel.grid = element_blank(),
  ) +
  coord_flip() +
  labs(
    title = 'Directors with most movies'
  )





pal2 <- brewer.pal(8,"Dark2")
df_netflix5 <- df_netflix %>% 
  mutate(title = strsplit(as.character(title), " ")) %>%
  unnest(title) %>%
  group_by(title)

corp_title <- Corpus(VectorSource(df_netflix5$title))
corp_title <- tm_map(corp_title, tolower)
title_cleaned <- tm_map(corp_title,removeWords, stopwords("english"))


tdm <- TermDocumentMatrix(title_cleaned)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.35, colors=pal2, vfont=c("sans serif","plain"))










