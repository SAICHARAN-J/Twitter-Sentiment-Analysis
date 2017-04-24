# Loading Libraries in R
library(ROAuth)
library(twitteR)
library(dplyr)
library(stringr)
library(tidyr)
library(xlsx)
# Key Specifiers to the API
consumer_key <- "yD46frFvHCCN33mZ3pELuG0VV";
consumer_secret <- "yUDjXxRIOHLzEOQE30cne88i3rMMD0flUJvM2pwXTU76IYO3KY";
access_token <- "2568995612-Sma29szAlVC1anzC24s3bhOsgPjwYY2i0nh5ERA";
access_secret <- "vdAwcyOaeIUhtQ9qdY6lJC2wdclIDx4DeZRbXaq4FXFAS";

# Setting up with the twitter Appliction
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
# Accessing the Twitter API
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

Cred <- OAuthFactory$new(consumerKey = consumer_key,consumerSecret = consumer_secret,requestURL = reqURL,accessURL = accessURL,authURL = authURL)
Cred$handshake(cainfo="cacert.pem")


# Accessing Tweets
data <- searchTwitter("#baahubali2",n = 17000)

# Retrieving Positive and Negative Words from Local Directory.
positive.words <- scan("positive.txt",comment.char = ";", what = "character")
negative.words <- scan("negative.txt",comment.char = ";", what = "character")

# Getting the Text part of the tweets

random_sample <- NULL;
for(tweet in data)
{
        random_sample <-  paste(random_sample,tweet$getText());
}

# Convertion of List to a Data-Frame 
data_df <- twListToDF(data)

#Selecting Required Columns from the DataFrame
data_df_selected <- select(data_df,c(1,3,5,8,10,11,12))

# Removing Emoticon from the tweets !! :)
data_df_selected$text <- lapply(data_df_selected$text,
                                function(text)
                                {
                                        iconv(text,"latin1","ASCII",sub = "")
                                }
)
# Viewing the DataFrame
View(data_df_selected)
word_list <- NULL;
# Getting the Text Part of the Tweets !
output_list <- lapply(data_df_selected$text,function(tweet_text)
{
        #Cleaning Data:
        tweet_text <- gsub("[[:punct:]]","",tweet_text);
        tweet_text <- gsub("[[:cntrl:]]","",tweet_text);
        tweet_text <- gsub("\n","",tweet_text);
        tweet_text <- gsub("\\d+","",tweet_text);
        tweet_text <- tolower(tweet_text);
        #Splitting Sentences into Words:
        tweet_text <- str_split(tweet_text,"\\s+");
        words <- unlist(tweet_text);
        word_list <<- paste(word_list,words);
        # Matching Words with Postive and Negative Words:
        positive.matches <- match(words,positive.words)
        negative.matches <- match(words,negative.words)
        #Removing all the NA's
        positive.matches <- !is.na(positive.matches)
        negative.matches <- !is.na(negative.matches)
        # Postive and Negative Count
        positive_count = sum(positive.matches)
        negative_count = sum(negative.matches);
        # Overall Score of Individual Tweet
        score = positive_count - negative_count;
        
        paste(positive_count,negative_count,score,sep=",");
})

# Adding the above Ouptut to the Origianl DataFrame
data_df_selected$New <- output_list;
# Separating them into three different Categories
data_df_selected <- separate(data_df_selected,New,c("Positive","Negative","Score"),sep =",")
# Adding a Sentiment Factor
data_df_selected <-  mutate(data_df_selected,Sentiment = ifelse(data_df_selected$Score > 0,
                                                                'Positive', ifelse(data_df_selected$Score < 0,'Negative','Neutral')))
#Saving it in the Local Directory !!
write.xlsx(data_df_selected,"Twitter_Baahubali2.xlsx")

#Converting Created Column into Data Format
data_df_selected$created <- strftime(data_df_selected,"%Y-%m-%d")
data_df_selected$created <- as.Date(data_df_selected$created)

#Grouping them for visualization
data_group <- group_by(data_df_selected,Sentiment,created)
grouped_frame <- summarize(data_group,number=n())

# Visualization
ggplot(grouped_frame,aes(created,number)) + 
        geom_line(aes(group=Sentiment,color=Sentiment),size=1.1) +
        geom_point(aes(group=Sentiment,color=Sentiment),size=2.5) +
        xlab("Tweeted On") + ylab("Number of Tweets")

#Device Usage
#Cleaning Status_Source
Users <- data_df_selected$statusSource
Users <- gsub("<[^>]*>","",Users)
Users <- gsub("Twitter for ","",Users)
Users <- gsub("\\s+","",Users)

#Importing it back to the Original DataFrame
data_df_selected$statusSource <- Users
#Selecting statusSource's which had been Widely Used
Main_Sample <- filter(data_df_selected,statusSource == "Android" | statusSource == "iPhone"| statusSource == "TwitterWebClient" | statusSource == "TwitterLite" | statusSource == "TweetDeck" | statusSource == "Windows" | statusSource == "WindowsPhone" |statusSource == "iPad")
#Grouping and Summarising them!
grouping_sample <- group_by(Main_Sample,statusSource)
sum_up <- summarize(grouping_sample,Number = n())

#Exporting sum_up frame to Local Directory!!
sum_up <- write.xlsx(sum_up,"Group_Users_Twitter.xlsx")


