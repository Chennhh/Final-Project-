# R version 3.4.4 (2018-03-15)
library(tidyverse)
library(httr)
library(ggplot2)
library(stringr)
library("tm")
library("SnowballC")
library("RColorBrewer")
library(wordcloud)
library(leaflet)



yelp_result <- function(keyword, num_of_business){
api_key <- "0cFq1_x0M3Th7jRukCHXdPlxXXdIg0OSP5YCN5sAARZMFEmgYKQdja8A8HpI8xRe51nC6-JCZjeGWUaZh92xrOFGKvKFITn5ENbiNkPkMqw_0wupzQTiQrIhsEvhWnYx"
client_id <- "mkwzEaUHww2bepzeXx2JaqA"
client_secret <- api_key

response <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

#token <- content(response)$access_token


# extrat the data from YELP
yelp <- "https://api.yelp.com"
term <- keyword
location <- "Boston, MA"
categories <- NULL
limit <- num_of_business
radius <- 8000
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers(Authorization = paste("Bearer", api_key)))
return(content(res))
}



yelp_summary_in_dataframe <- function(results) {
rows = length(results$business)
result_df <- data.frame(matrix(nrow = rows, ncol = 7))
colnames(result_df) <- c("name", "price", "rating","review", "c1", "c2", "c3")
for (i in 1:rows) {
  result_df[i, 1] <- results$business[[i]]$name
  result_df[i, 2] <- results$business[[i]]$price
  result_df[i, 3] <- results$business[[i]]$rating
  result_df[i, 4] <- results$business[[i]]$review_count
  cat_column_index <- 5
  for(num in 1:length(results$business[[i]]$categories)) {
    result_df[i, cat_column_index] <- results$business[[i]]$categories[[num]]$title
    cat_column_index <- cat_column_index + 1
  }
}
return(result_df)
}

get_average <- function(df_col){
  return(mean(df_col))
}


category_word_colud <- function(dataframe){
  dataframe$c1 <- paste(dataframe$c1, dataframe$c2, dataframe$c3)
  ##str(dataframe$c1)
  wordcloud(words = dataframe$c1, 
            min.freq=1, 
            random.order = FALSE,
            rot.per=.5,colors=brewer.pal(8, "Dark2"))
}

# the maximum num of querie is 50
yelp50<- yelp_result("Asian", 50) 
df<- yelp_summary_in_dataframe(yelp50)
df
rating_avg <- get_average(df$rating)
review_avg <- get_average(df$review)
is.numeric(df$review_count)
category_word_colud(df)

###transform the characters to numeric vectors
rating <- df$rating
price <- df$price
pricen<- str_length(price)
is.numeric(pricen)
### the regression between the price and the rating 


model <- lm(pricen~rating)
summary(model)
anova(model)
cor(pricen,rating)
### according to the outputs from anova table & linear regression, the P-value of the rating and price is statistically significantly 
### and the beta for the rating is -0.4850, which means in this sample, when the price goes up for 1 unit, the rating averagely goes 
### down for 0.4850. and the P-value is less than .05. 

### bar plot of the cuisine types 
category_df <- df[c("c1", "c2", "c3")]
View(category_df)
as.data.frame(category_df)
df_c1 <- category_df[c("c1")]
colnames(df_c1) <- c("cat")
df_c2 <- category_df[c("c2")]
colnames(df_c2) <- c("cat")
df_c3 <- category_df[c("c3")]
colnames(df_c3) <- c("cat")
category150 <- rbind(df_c1, df_c2, df_c3)
View(category150)


cat_freq_bar_chart <- function(cat_df){
freq_list <- as.data.frame(table(unlist(cat_df)))
colnames(freq_list) <- c("category", "frequency")
data <- data.frame(
  category=freq_list$category,
  frequency=freq_list$frequency
)

data %>%
  mutate(name = fct_reorder(category, frequency)) %>%
  ggplot( aes(x=category, y=frequency)) +
  geom_bar(stat="identity") +
  coord_flip()
}

cat_freq_bar_chart(category150)
### the above is the barplot of the cuisines categories. we can apparently notice that Asian Fusion 
### is the most frequently served, to be more specific, the sushi bars, chinese cuisines, thai 
###japeanses and ramen are most ofter served.




###
top <- df %>% filter(rating > rating_avg) %>% filter(review > review_avg)
View(top)
### I use the average points to filter out the restaurants above the
### the graph of the top 10 restaurants with more reviews and higher ratings in ordinal sequention


### the below is a leaflet map.
graph_of_top10 <- leaflet() %>%
  addTiles() %>% addMarkers(lng=-71.06625, lat=42.34382, popup=" 1.Myers' Chang Taiwanese CocktailBar") %>% 
  addMarkers(lng= -71.06077,lat= 42.35113, popup="2.Shojo Japanese Tapas Bars") %>% 
  addMarkers(lng = -71.05627, lat=42.36059, popup= "3.Koy Korean") %>% 
  addMarkers(lng = -71.08737, lat =42.34577, popup = "4.PhoBsil Vietnamese Thai Seafood") %>% 
  addMarkers(lng = -71.06256, lat =42.35171, popup = "5.Q Restaurant Chinese Bubble Tea") %>% 
  addMarkers(lng = -71.1288, lat =42.35246, popup = "6.Dolphin Bay Taiwanese Bubble Tea") %>% 
  addMarkers(lng = -71.08543, lat = 42.34848, popup = "7.Santouku Back Bay Ramen Soup Noodles") %>% 
  addMarkers(lng = -71.06144, lat = 42.35098, popup = "8.New Dong Khanh Vietnamese Chinese Bubble tea") %>% 
  addMarkers(lng = -71.07789, lat =42.36625, popup = "9.Shabu&Mien Hotpot Ramen") %>% 
  addMarkers(lng = -71.07557, lat =42.3469, popup = "10.Douzo Japanese Sushi Bars")
  
graph_of_top10


