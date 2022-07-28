setwd("/Users/adra7980/Documents/diario_text/2019.11.14-ElDiarioCorpus/adverts")

files<-list.files(pattern=".txt")
diario<-Corpus(URISource(files), readerControl = list(reader=readPlain))

diario_tidy<-tidy(diario)

spanish_stopwords<-as.data.frame(stopwords("spanish"))

diario_frequency_table<-tidy_diario %>% 
                        unnest_tokens(word, text) %>% 
                        count(word, sort=TRUE) %>% 
                        filter(!word %in% stop_words$word) %>% 
                        filter(!word %in% spanish_stopwords$`stopwords("spanish")`)


diario_frequency_table_numbers<-parse_number(diario_frequency_table$word)

diario_frequency_table<-cbind(diario_frequency_table, diario_frequency_table_numbers)

diario_frequency_table<-diario_frequency_table %>% 
                        filter(is.na(diario_frequency_table_numbers))

View(diario_frequency_table)
  
  
  