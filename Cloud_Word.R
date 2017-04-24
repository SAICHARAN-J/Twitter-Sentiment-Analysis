library(tm)
library(wordcloud)

dirPath <- "Twitter_Words"
doc <- Corpus(DirSource(dirPath));

doc <- tm_map(doc,content_transformer(tolower));
doc <- tm_map(doc,removeNumbers);
doc <- tm_map(doc,removeWords,stopwords("english") )
doc <- tm_map(doc,removePunctuation);
doc <- tm_map(doc,stripWhitespace);

tdm <- TermDocumentMatrix(doc);
matrix <- as.matrix(tdm);
v_data <- sort(rowSums(matrix),decreasing = TRUE) 

word_cloud <- data.frame(words = names(v_data), frequency = v_data)

wordcloud(word_cloud$words,word_cloud$frequency,random.order = FALSE,
          color=c("red","orange","black","blue","#007bb5"))


