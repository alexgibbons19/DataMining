# Assignment #5 - Cosine Similarity
# Alex Gibbons and Lauren Kawecki
library("lsa")
library("tm")


preprocess_text <- function(text) {
  # remove extra whitespace
  text <- stripWhitespace(text)
  # remove punctuation
  text <- gsub("[[:punct:]]", "", text)
  # change all words to lower to ignore case
  text <- tolower(text)
  return(text)
}

find_frequency <- function(text) {
  # split each word into separate tokens
  tokens <- unlist(strsplit(text, " "))
  # get frequency of tokens
  term_freq <- table(tokens)
  return(term_freq)
}


cosine_similarity <- function(tf1, tf2) {
  # get list of all unique words
  all_terms<-unique(c(names(tf1),names(tf2)))
  # concatenate missing words with value 0 to tables
  tf1[setdiff(all_terms, names(tf1))] <- 0
  tf2[setdiff(all_terms, names(tf2))] <- 0
  # sort by name to ensure proper cosine calculation
  tf1 <- tf1[order(names(tf1))]
  tf2 <- tf2[order(names(tf2))]
  # compute cosine
  tf1_vec<-as.vector(tf1)
  tf2_vec<-as.vector(tf2)
  cosine_sim<-cosine(tf1_vec,tf2_vec)
  return(cosine_sim)
}

top_10_frequent_words <- function(text) {
  # process text again
  # concatenate all lines into single string
  text<-paste(text,collapse=" ")
  # remove numbers
  text<-removeNumbers(text)
  # remove stopwords
  text<-removeWords(text, stopwords("en"))
  
  # Convert text to a corpus
  corpus <- Corpus(VectorSource(text))
  
  # Create a term-document matrix
  tdm <- TermDocumentMatrix(corpus)
  
  # Find the top 10 most frequent terms
  top_terms <- findMostFreqTerms(tdm, 10)
  
  return(top_terms)
}

assignment_5 <- function(doc1,doc2){
  doc1<-preprocess_text(doc1)
  doc2<-preprocess_text(doc2)
  
  doc1_freq<-find_frequency(doc1)
  doc2_freq<-find_frequency(doc2)
  tf1_mat<-as.matrix(doc1_freq)
  tf2_mat<-as.matrix(doc2_freq)
  
  
  cosine<-cosine_similarity(doc1_freq,doc2_freq)
  print('Cosine Similarity:')
  print(cosine[1])
  
  top_10a <- top_10_frequent_words(doc1)
  top_10b <- top_10_frequent_words(doc2)
  
  print('Top 10 most freq words in doc a: ')
  print(top_10a[])
  print('Top 10 most freq words in doc b: ')
  print(top_10b[])
  return(0)
}

# First set of documents:
doc1a<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document1a.txt")
doc2a<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document2a.txt")

print("First set of documents:")
assignment_5(doc1a,doc2a)

# Second set of documents:
doc1b<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document1b.txt")
doc2b<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document2b.txt")

print("Second set of documents:")
assignment_5(doc1b,doc2b)

# Third set of documents:
doc1c<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document1c.txt")
doc2c<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//document2c.txt")

print("Third set of documents:")
assignment_5(doc1c,doc2c)

# Fourth set of documents: 
doc1<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//doc3.txt")
doc2<-readLines("C://Users//Kelly//Desktop//MCSP24//CMPT363 - Data Mining//doc4.txt")

print("Fourth set of documents:")
assignment_5(doc1,doc2)
