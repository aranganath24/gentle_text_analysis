files<-list.files(pattern=".txt")
diario<-Corpus(URISource(files), readerControl = list(reader=readPlain))


tdm<-as.matrix(TermDocumentMatrix(diario, 
                                  control=
                                    list(removePunctuation = TRUE,
                                         stopwords = TRUE,
                                         tolower = TRUE,
                                         stemming = FALSE,
                                         removeNumbers = TRUE,
                                         bounds = list(global = c(3, Inf)))))


# Word frequencies
word_frequencies<-sort(rowSums(tdm), decreasing=TRUE)

# Make data frame
df <- data.frame(word = names(word_frequencies),freq=word_frequencies)


wordcloud(words = df$word, freq = df$freq, min.freq=5, random.order=TRUE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


wordcloud2(data = df, minRotation = 0, maxRotation = 0, ellipticity = 0.2)
wordcloud_blm


