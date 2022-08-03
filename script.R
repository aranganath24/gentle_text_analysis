# Install packages

install.packages("tm")
install.packages("tidyverse")
install.packages("wordcloud2")
install.packages("quanteda")
install.packages("tidytext")

# Load libraries
library(tm)
library(tidyverse)
library(wordcloud2)
library(quanteda)
library(tidytext)

# Set working directory
setwd("2019.11.14-ElDiarioCorpus/creatives")


# 3. Read in text corpus -----------------------------------------------------

# reads in the filenames for diario advertisements as a character vector, and assigns it 
# to an object named "diario_files"
diario_files<-list.files(pattern=".txt")

# prints diario_files
diario_files

# Uses the "Corpus" function from the "tm" package to create a new text corpus based 
# on the diario creatives text files; this corpus is assigned to a new object named 
# "diario_adverts_corpus"
diario_creatives_corpus<-Corpus(URISource(diario_files), readerControl = list(reader=readPlain))

# prints metadata about the corpus assigned to the "diario_adverts_corpus" object
diario_creatives_corpus

# 4. Create a tidy dataframe from a corpus -----------------------------------

# Uses the "tidy" function from the "tidytext" package to transform the "diario_adverts_corpus" corpus into a tidy data frame, where each file's textual information is stored as a row in the data frame; this data frame is assigned to a new object named "diario_adverts_tidy"
diario_creatives_tidy<-tidy(diario_creatives_corpus)

# prints contents of "diario_creatives_tidy" to console
diario_creatives_tidy

# Opens "diario_creatives_tidy" in R Studio data viewer
View(diario_creatives_tidy)

# 5. Creating a draft word frequency table -----------------------------------------

## 5.1 Tokenize diario_creatives_tidy ------------------------------------------

# Tokenizes "diario_creatives_tidy" by word and assigns the resulting dataset to "diario_word_tokenized"
diario_word_tokenized<-
  diario_creatives_tidy %>% # declares dataset with relevant text
  unnest_tokens(input=text, # specifies name of input column containing text data
                token="words", # specifies how to tokenize input column
                output=word)  # specifies name of output column containing tokens


# opens "diario_word_tokenized" in R Studio data viewer
View(diario_word_tokenized)


## 5.2 Extract the first draft of a word frequency table  --------------------------

# Uses "count" function to generate a dataset that contains information on the frequency 
# of each word in "diario_word_tokenized"; assigns this newly created dataset 
# (organized in descending order) 
diario_frequency_table<-diario_word_tokenized %>% 
                          count(word, sort=TRUE)

# opens "diario_frequency_table" in R Studio data viewer
View(diario_frequency_table)


## 5.3 Cleaning the word frequency table ---------------------------------------

### 5.3.1 Removing stopwords ------------------------------------------------------

# prints contents of "stop_words"
stop_words

View(stop_words)

# extract Spanish stopwords as a data frame, and assign it to an object named "spanish_stopwords"
spanish_stopwords<-as.data.frame(stopwords("spanish"))

# Views spanish_stopwords in data viewer
View(spanish_stopwords)

# Takes the existing "diario_frequency_table" dataset, and removes English and 
# Spanish stopwords from it
diario_frequency_table<-
  diario_frequency_table %>% 
  filter(!word %in% stop_words$word) %>% # removes English stop words
  filter(!word %in% spanish_stopwords$`stopwords("spanish")`) # removes Spanish stopwords


### 5.3.2 Removing numbers --------------------------------------------------------

# defines a vector that extracts numbers from the "word" column in "diario_frequency_table" 
# and assigns it to a new object named "diario_frequency_table_numbers"
diario_frequency_table_numbers<-parse_number(diario_frequency_table$word)

# updates the existing "diario_frequency_table" data frame by binding 
# "diario_frequency_table_numbers" vector to it
diario_frequency_table<-cbind(diario_frequency_table, diario_frequency_table_numbers)

## views "diario_frequency_table" in data viewer
View(diario_frequency_table)

# extracts all rows in which the "diario_frequency_table_numbers" column 
# of the "diario_frequency_table" data frame is an NA value; this effectively 
#removes all rows in "diario_frequency_table" in which the "word" column has a number

diario_frequency_table<-diario_frequency_table %>% 
                        filter(is.na(diario_frequency_table_numbers))


# deletes the "diario_frequency_table_numbers" from the "diario_frequency_table" dataframe 
diario_frequency_table<-diario_frequency_table %>% 
                        select(-diario_frequency_table_numbers)


## 5.4 Views updated/cleaned "diario_frequency_table" in R Studio data  --------

# Views updated/cleaned "diario_frequency_table" in R Studio data viewer
View(diario_frequency_table)


# 6. Visualizing word frequency data -----------------------------------------

## 6.1 Charting word frequencies -----------------------------------------------

### 6.1.1  Extracting dataframe of ten most frequent words -------------------------

# extracts ten most frequently appearing words from "diario_frequency_table" from 
# "diario_frequency_table" and assigns this data frame to a new object named "diario_top_ten"
diario_top_ten<-diario_frequency_table %>% 
                    slice_max(n, n=10)

# Views "diario_top_ten" in data viewer
View(diario_top_ten)


### 6.1.2 Using ggplot2 to make chart of ten most frequent words ------------------

# Creates bar chart of word frequency of ten most frequently occurring words in "diario_top_ten"
diario_frequency_graph<-ggplot(data=diario_top_ten)+
                          geom_col(aes(x=word, y=n))+
                          labs(title="Ten Most Frequent Words in Diario Creatives",
                               caption = "Source: El Diario Project", 
                                x="", 
                                y="Frequency")


# prints "diario_frequency_graph"
diario_frequency_graph

# Creates a bar chart of information in diario_top_ten with "word" on x-axis and ordered 
# with respect to their frequency ("n")
diario_frequency_graph<-ggplot(data=diario_top_ten)+
                          geom_col(aes(x=reorder(word, n), y=n))+
                          labs(title="Ten Most Frequent Words in Diario Creatives",
                               caption = "Source: El Diario Project", 
                                x="", 
                                y="Frequency")



# prints updated "diario_frequency_graph"
diario_frequency_graph

# Uses "diario_top_ten" data frame to make horizontal bar chart of ten most 
# frequently used words in corpus, in ascending order; the chart is assigned 
# to a new object named "diario_frequency_graph_inverted"

diario_frequency_graph_inverted<-ggplot(data=diario_top_ten)+
                                  geom_col(aes(x=reorder(word, n), y=n))+
                                  coord_flip()+
                                  labs(title="Ten Most Frequent Words in Diario Creatives",
                                        caption = "Source: El Diario Project", 
                                         x="", 
                                         y="Frequency")
# Prints diario_frequency_graph_inverted
diario_frequency_graph_inverted

## 6.2 Creating a wordcloud ------------------------------------------------

# make word cloud based on word frequency information from "df_word_frequencies"
wordcloud2(data = diario_frequency_table, minRotation = 0, maxRotation = 0, ellipticity = 0.6)


# 7 Bigrams ---------------------------------------------------------------


## 7.1 Extracting bigrams from a text corpus and creating a bigram frequency table --------

# Views "diario_creatives_tidy" in data viewer
View(diario_creatives_tidy)

# Extracts table of bigram frequencies
diario_bigram<-diario_creatives_tidy %>% 
                   unnest_tokens(input=text,
                                 token="ngrams",
                                 n=2,
                                 output=bigram) %>% 
                      count(bigram, sort=TRUE)

# Views diario_bigram
View(diario_bigram)


# 7.2 Cleaning a bigram frequency table -----------------------------------

# Separates bigrams in the "bigram" column of "diario_bigram" into two 
# separate columns, named "word1" and "word2"
diario_bigram_separated<-diario_bigram %>% 
                          separate(bigram, c("word1", "word2"), sep=" ")

# Views "diario_bigram_separated"
View(diario_bigram_separated)


### 7.2.1 Removing stopwords from bigrams -----------------------------------

# Removes English and Spanish stopwords from the "word1" and "word2" columns
diario_bigram_filtered<-
  diario_bigram_separated %>% 
    filter(!word1 %in% stop_words$word) %>% # removes English stopwords from "word1" column
    filter(!word2 %in% stop_words$word) %>% # removes English stopwords from "word2" column
    filter(!word1 %in% spanish_stopwords$`stopwords("spanish")`) %>%  # Removes Spanish stopwords from "word1" column
    filter(!word2 %in% spanish_stopwords$`stopwords("spanish")`) # Removes spanish stopwords from "word2" column
  

# Views diario_bigram_filtered in data viewer
View(diario_bigram_filtered)


# 7.2.2 Removing numbers from bigrams -------------------------------------

# Extracts a vector of numbers contained in "word1" of "diario_bigram_filtered" 
# and assigns it to a new object named "diario_bigram_word1numbers"
diario_bigram_word1numbers<-parse_number(diario_bigram_filtered$word1)

# Extracts a vector of numbers contained in "word2" of "diario_bigram_filtered", 
# and assigns it to a new object named "diario_bigram_word2numbers"
diario_bigram_word2numbers<-parse_number(diario_bigram_filtered$word2)

# Adds the "diario_bigram_word1numbers" and "diario_bigram_word2numbers" vectors 
# as columns in "diario_bigram_filtered"
diario_bigram_filtered<-cbind(diario_bigram_filtered, diario_bigram_word1numbers, diario_bigram_word2numbers)

# Views "diario_bigram_filtered" in viewer
View(diario_bigram_filtered)

# Extracts rows in "diario_bigram_filtered" where the "diario_bigram_word1numbers" or 
# "diario_bigram_word2numbers" columns contain an NA value; this effectively deletes 
# rows where the bigram contains a number
diario_bigram_filtered<-diario_bigram_filtered %>% 
                          filter(is.na(diario_bigram_word1numbers)) %>% 
                          filter(is.na(diario_bigram_word2numbers))


# Views "diario_bigram_filtered"
View(diario_bigram_filtered)

## 7.3 Reconstituting the bigram frequency table after cleaning ------------


# Takes separate words, in the "word1" and "word2" columns of "diario_bigram_filtered", 
# and unites them back into one column, named "bigram"; this updated data frame 
# is assigned to a new object named "diario_bigram_filtered_unite"
diario_bigram_filtered_unite<-diario_bigram_filtered %>% 
                               unite(bigram, word1, word2, sep=" ")

# Views "diario_bigram_filtered_unite" in viewer
View(diario_bigram_filtered_unite)

# removes the "diario_bigram_word1numbers" and "diario_bigram_word2numbers" columns 
# from "diario_bigram_filtered_unite"
diario_bigram_frequency_final<-diario_bigram_filtered_unite %>% 
                                select(-c(diario_bigram_word1numbers, diario_bigram_word2numbers))


# Views "diario_bigram_frequency_final" in data viewer
View(diario_bigram_frequency_final)



# 7.4 Visualizing bigrams -------------------------------------------------

# Extracts a data frame of the ten most frequently occurring bigrams in "diario_bigram_filtered_unite" and assigns this data frame to a new object named "diario_bigram_top_ten"
diario_bigram_top_ten<-diario_bigram_frequency_final %>% 
                          slice_max(n, n=10)

# Views "diario_bigram_top_ten" in data viewer
View(diario_bigram_top_ten)

# Creates sideways bar chart of 10 most frequently ocurring bigrams
bigram_graph<-
  ggplot(diario_bigram_top_ten)+
  geom_col(aes(x=reorder(bigram, n), y=n))+
  coord_flip()+
  labs(title="Ten Most Frequent Bigrams in Diario Creatives",
       caption = "Source: El Diario Project", 
       x="", 
       y="Frequency")+
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10))

# prints "bigram_graph"
bigram_graph


# 8 Extracting keywords in context ------------------------------------------

# creates a tokens object based on the "text" column of the "tidy_diario" data 
# frame and assigns it to "kwic_token"
kwic_token<-tokens(diario_creatives_tidy$text, remove_punct = TRUE)

# Extracts contextual text data for the keyword "business", based on a 
# window of 3 words; the resulting data frame containing the contextual 
#information associated with each appearance of the keyword is assigned to a new object 
# named "business_keyword_context"
earth_keyword_context<-kwic(kwic_token, pattern="earth", window=3)


# Views "business_keyword_context" in data viewer
View(earth_keyword_context)












