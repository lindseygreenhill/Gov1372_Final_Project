library(readxl)
library(janitor)
library(skimr)
library(rtweet)
library(tidyverse)

# reading in the senate twitter sheet. I need to subset the link column so it is
# just the handle and not the full link

twitter_handles_senate <- read_excel("data/congress_twitter_092721.xlsx",
                                  sheet = 1,
                                  skip = 1) %>%
  clean_names() %>%
  mutate(member = "senate")

# reading in the house twitter sheet

twitter_handles_house <- read_excel("data/congress_twitter_092721.xlsx",
                                     sheet = 2,
                                     skip = 1) %>%
  clean_names() %>%
  mutate(member = "house")

# joining the data frames and cleaning the link col

twitter_handles <- twitter_handles_house %>%
  rbind(twitter_handles_senate) %>%
  mutate(handle = str_sub(link, start = 21)) %>%
  select(-link)

# checking for missing values. Looks like 3 members do not have twitter
# accounts. Those people are Jake Ellzey, Chris Smith, and Jefferson Van Drew so
# I have to add them manually. Chris Smith deactivated his twitter account in
# January of 2021. Jeff Van Drew also deactivated his twitter account when Chris
# Smith did it. They said it was in repsonse to Trump's behavior. I think as of
# now I will drop them from the data. 

skim(twitter_handles)

twitter_handles <- twitter_handles %>%
  mutate(handle = case_when(name == "Ellzey, Jake" ~ "RepEllzey",
                            T ~ handle)) %>%
  drop_na()

# Now I am using the rtweets package to create the tweets data fram. Code
# adapted from Blake Robert Mills article. I keep on getting errors for hitting
# rate limits. I think to solve this I need to do it in smaller chunks. 

# chunk 1

handles_1 <- twitter_handles %>%
  arrange(name) %>%
  slice(1:50)

#Tweets_1 <- vector()
#for(i in handles_1$handle){
#  df <- get_timeline(i, n = 3200)
#  Tweets_1 <- rbind(Tweets_1, df)
# }

# save(Tweets_1, file = "tweets_1.RData")


# chunk 2

#handles_2 <- twitter_handles %>%
#  arrange(name) %>%
#  slice(51:100)

#Tweets_2 <- vector()
# for(i in handles_2$handle){
#  df <- get_timeline(i, n = 3200)
#  Tweets_2 <- rbind(Tweets_2, df)
# }

# save(Tweets_2, file = "tweets_2.RData")

# chunk 3

handles_3<- twitter_handles %>%
  arrange(name) %>%
  slice(101:150)

 # Tweets_3 <- vector()
 #for(i in handles_3$handle){
  #df <- get_timeline(i, n = 3200)
  #Tweets_3 <- rbind(Tweets_3, df)
 #}

 # save(Tweets_3, file = "tweets_3.RData")

# chunk 4

handles_4 <- twitter_handles %>%
  arrange(name) %>%
  slice(151:200)

# Tweets_4 <- vector()

#for(i in handles_4$handle){
#  df <- get_timeline(i, n = 3200)
#  Tweets_4 <- rbind(Tweets_4, df)
# }

# save(Tweets_4, file = "tweets_4.RData")


# chunk 5

handles_5 <- twitter_handles %>%
  arrange(name) %>%
  slice(201:250)

# Tweets_5 <- vector()

#for(i in handles_5$handle){
 # df <- get_timeline(i, n = 3200)
#  Tweets_5 <- rbind(Tweets_5, df)
# }

# save(Tweets_5, file = "tweets_5.RData")


# I still don't have complete data frame but I am going to clean it anyways

tweets_tidy <- Tweets %>%
  select(created_at, screen_name, text, is_retweet, hashtags)

# Do we want to keep retweets? I am going to clean for emojies. Not sure if I
# have to deal with hashtags but I think quanteda will do this for me. 

# this code is adapted from Blake Robert Mills replication code

Emojis <- emojis 
Emojis$description <- str_replace_all(Emojis$description, " ", "-") %>% 
  paste(" ", .,"emoji ", sep="-")

tweets_tidy_2 <- tweets_tidy

for(i in 2284:2623){
  tweets_tidy_2$text <- str_replace_all(tweets_tidy_2$text, #Take each Tweet
                                   as.character(Emojis[i,1]), #Identify Emoji
                                   as.character(Emojis[i, 2])) #Replace Emoji with w
}

rm(Emojis)

# so now I have a new data frame called tweets_tidy_2 that has the emojis
# replaced. I am going to save the temporary data frame now. 

tt_3 <- tweets_tidy_2 %>%
  select(-hashtags)


write_csv(tt_3, "data/temp_tweet_df.csv")
