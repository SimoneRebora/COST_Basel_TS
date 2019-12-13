library(XML)
if (!require("stylo")) install.packages("stylo") # different way to call a package: it installs it if absent

### read all files in the corpus
my_corpus <- list.files("corpus_ELTeC_it", pattern = ".xml", full.names = T)
my_titles <- list.files("corpus_ELTeC_it", pattern = ".xml", full.names = F)

### prepare a new folder for stylometric analyses
unlink("corpus", recursive = T)
dir.create("corpus")

### the names of the files have to be changed, as stylo requires this format: Author_title_anything.else 
newNames <- strsplit(my_titles, "_|\\.")
newNames <- unlist(lapply(newNames, function(x) paste(x[2], "_", x[1], ".xml", sep = "")))
file.copy(from = my_corpus, to = "corpus")
file.rename(from = file.path("corpus", my_titles), to = file.path("corpus", newNames))

### to call stylo, use this very simple function:
stylo()
# Stylo works by default on the files in the "corpus" folder inside the working directory 
# it has a user interface, so (for simple experiments) it does not require coding
# Try a simple experiment:
# Input & Language --> Input: xml; Language: Italian; native encoding
# Features --> MFW settings 2000, both min and max
# Statisticsc --> Cluster Analysis and Cosine Delta

### how does it work the "xml" upload in Stylo?
### the following two lines of code are copy-pasted from Stylo "internal" code:
loaded_file <- scan("corpus/Svevo_ITBI0087.xml", what = "char", encoding = "utf-8", sep = "\n", quiet = TRUE)
stylo::delete.markup(loaded_file, markup.type = "xml")
# Note: the Preface is kept
stylo::delete.markup(loaded_file, markup.type = "xml")
substr(stylo::delete.markup(loaded_file, markup.type = "xml"), 1, 200)

### But we might want to run the analysis only on the body of the texts
### ...we can use the ELTeC encoding:

### first, select all files in the "corpus" folder
my_corpus <- list.files("corpus", pattern = ".xml", full.names = T)
my_corpus
### define the namespace
namespace <- c(TEI="http://www.tei-c.org/ns/1.0")
### then prepare an empty list for the texts 
my_texts <- list()
### and run a loop on all the files
for(i in 1:length(my_corpus)){
  # read xml (through the XML package)
  data <- xmlParse(my_corpus[i])
  
  # define the tag "p" under "body"
  my_tag <- "//TEI:body//TEI:p"
  # find all nodes in the document based on that tag
  my_result <- xpathSApply(doc = data, path = my_tag, fun = xmlValue, namespaces = namespace)
  
  # put them in a single text
  my_result <- paste(my_result, collapse = "\n")
  # save the texts (only the p's under body) inside the list
  my_texts[[i]] <- my_result
  # print progress
  cat(i, "done:", my_corpus[i], "\n")
}

### If we want to run stylo on the texts now saved in the list, we need to "tokenize" them
### i.e. split them into single words
### There is a function in the "stylo" package that does it,
### so we call it in a loop on the list that contains the texts

stylo_texts <- list()
for(i in 1:length(my_texts)){
  # prepare each text for stylo analysis
  stylo_texts[[i]] <- stylo::txt.to.words.ext(my_texts[[i]], corpus.lang =  "Italian")
  print(i)
}

### explore
head(stylo_texts[[1]])
head(stylo_texts[[2]])
length(stylo_texts[[1]])
length(stylo_texts[[2]])

### we can also see the length (in words) of each text in the list
lapply(stylo_texts, length)

### Finally, we give to each text in the list the name that it had in the "corpus" folder
names(stylo_texts) <- list.files("corpus")
# check the names:
names(stylo_texts)

### Now everything is ready to run stylo on the texts that we have saved in the R list
### We can even call stylo by deactivating the GUI, and setting all the features via R code
results_stylo <- stylo(gui = FALSE, 
      corpus.lang="Italian", 
      analysis.type="CA", 
      mfw.min=2000, 
      mfw.max=2000,
      distance.measure="dist.wurzburg",
      parsed.corpus = stylo_texts)
# Note: the results of the analysis have been saved in a variable called "stylo_results"
# also, we have generated a .csv file that we will open later with Gephi (for the network analysis)

### Explore
results_stylo$distance.table
# Note: the "$" simbol is used to see the sub-section in a structured variable

### see the name of the texts in the distance table
rownames(results_stylo$distance.table)

### see a portion of the distance table
### for example the one of the first text in our selection
results_stylo$distance.table[1,]

### which one is the "closest" text?
sort(results_stylo$distance.table[1,])

### see a table with the frequency of all words
results_stylo$frequencies.0.culling
# rows are the texts, columns the words

### produce a list of the most frequent words
colnames(results_stylo$frequencies.0.culling)

### which is the position in the table of the word "pane"
my_word <- "pane"
my_word_position <- which(colnames(results_stylo$frequencies.0.culling) == my_word)

### which author uses "pane" more frequently?
sort(results_stylo$frequencies.0.culling[,my_word_position], decreasing = T)

#########
### Now let's run a different analysis in Stylo, one that focuses on words
#########

### first, find the texts written by one author
my_author <- "Svevo"
Chosen_texts <- which(grepl(my_author, names(stylo_texts)))
# This is a typical example of an "embedded" R function
# the "grepl" function checks if the string "Svevo" is present in the names of the "stylo_texts" list
# the "which" function check where the "grep" function found a correspondence
Chosen_texts

### We use the "oppose" function, still in the "stylo" package,
### that looks for the most distinctive words.
### The method it uses is known as "Zeta Analysis"
### The corpus should be divided in two parts:
### A "primary set" where we have the texts of interest;
### A "secondary set" to be compared with

### Our primary set are the texts by the author of interest
primary_set <- stylo_texts[Chosen_texts]
### Our secondary set are the texts by all the others
secondary_set <- stylo_texts[-Chosen_texts]

oppose(primary.corpus = primary_set, secondary.corpus = secondary_set)



##############
### Sentiment analysis
###########

if (!require("syuzhet")) install.packages("syuzhet")

### Upload the Sentix dictionary (for Italian language)
load("Sentix.RData")
# Setix was downloaded from: http://valeriobasile.github.io/twita/sentix.html
# Sentix was readapted for the task, reducing it to two fields:
# word (or better, the lemma of the word)
# and value (simple sentiment value, obtained by multiplying polarity and intensity)

# explore Sentix
View(Sentix)

# as Italian is a quite inflected language, the text should be lemmatized
# using the "udpipe" package
if (!require("udpipe")) install.packages("udpipe")

# upload the Italian udpipe model
udmodel_italian <- udpipe_load_model(file = "italian-isdt-ud-2.3-181115.udpipe")

# annotate the text (might require a bit of time)
x <- udpipe_annotate(udmodel_italian, x = my_texts[[1]])
x <- as.data.frame(x)
# visualize the annotated text
View(x)

# save the lemmatized text
italian_text <- x$lemma[1]
n <- 1
for(i in 2:length(x$token_id)){
  if(x$sentence_id[i] != x$sentence_id[i-1]){
    n <- n+1
    italian_text[n] <- ""
  }
  if(grepl(pattern = "|", x$lemma[i], fixed = T)){
    italian_text[n] <- paste(italian_text[n], unlist(strsplit(x$lemma[i], "|", fixed = T))[2])
    next
  }
  if(is.na(x$lemma[i]))
    next
  italian_text[n] <- paste(italian_text[n], x$lemma[i])
}

# explore
head(italian_text)

### Calculate sentiment values using Sentix
syuzhet_vector_Italian <- get_sentiment(italian_text, method = "custom", lexicon = Sentix)
summary(syuzhet_vector_Italian)

### Visualize the plot
plot(
  syuzhet_vector_Italian, 
  type="l", 
  main="My Italian Text", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

### ...the result is quite noisy!
### To have a "plot arc", you need to apply a series of filters
### (the simplest one is "rolling mean")
simple_plot(syuzhet_vector_Italian, title = "My Italian text")

### Save plot in png
png("Sentix_simple_plot.png", height = 900, width = 1600, res = 100)
simple_plot(syuzhet_vector_Italian, title = "My Italian text")
dev.off()


### Repeat the analysis on all the texts of the corpus
### ...but careful!!!!!
### it might take a few minutes (or hours...)
for(iter in 1:length(my_texts)){
  
  x <- udpipe_annotate(udmodel_italian, x = my_texts[[iter]])
  x <- as.data.frame(x)

  italian_text <- x$lemma[1]
  n <- 1
  for(i in 2:length(x$token_id)){
    if(x$sentence_id[i] != x$sentence_id[i-1]){
      n <- n+1
      italian_text[n] <- ""
    }
    if(grepl(pattern = "|", x$lemma[i], fixed = T)){
      italian_text[n] <- paste(italian_text[n], unlist(strsplit(x$lemma[i], "|", fixed = T))[2])
      next
    }
    if(is.na(x$lemma[i]))
      next
    italian_text[n] <- paste(italian_text[n], x$lemma[i])
  }

  syuzhet_vector_Italian <- get_sentiment(italian_text, method = "custom", lexicon = Sentix)

  plot_title <- unlist(strsplit(my_corpus[iter], "/|_|\\."))
  plot_title <- paste(plot_title[2], "_", plot_title[3], ".png", sep = "")
  png(plot_title, height = 900, width = 1600, res = 100)
  simple_plot(syuzhet_vector_Italian, title = gsub(pattern = ".png", replacement = "", plot_title))
  dev.off()
  
  print(iter)
}




#############
#### Topic Modeling
##########

if (!require("mallet")) install.packages("mallet")
if (!require("wordcloud")) install.packages("wordcloud")

# prepare the topic model
# text array is the corpus to analyze
# stoplist.file is the file with the stopwords
testo.instances <- 
  mallet.import(text.array = unlist(my_texts), 
                stoplist.file = "stopwords-it.stopwords", 
                id.array = as.character(1:length(my_texts)))

# prepare passage 2
# num_topics is the number of topics we are looking for
# the rest... better ignore it for the moment
num_topics <- 5
topic.model <- MalletLDA(num.topics=num_topics, alpha.sum = 1, beta = 0.1)
topic.model$loadDocuments(testo.instances)

# explore the topic model: vocabulary
vocabulary <- topic.model$getVocabulary()
head(vocabulary)

# word frequency
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

# train the topic models
topic.model$setAlphaOptimization(20, 50)
topic.model$train(200)

# get the words of the topics
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
# get topics per text
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
rownames(doc.topics) <- gsub(pattern = "corpus/", replacement = "", my_corpus)
# visualize as a heatmap
heatmap(doc.topics)

# wordclouds of topics
for(i in 1:num_topics){
  n_topic <- i
  print(i)
  words.per.topic <- mallet.top.words(topic.model, word.weights = topic.words[n_topic,], num.top.words = 100)
  print(head(words.per.topic))
  png(filename = paste(i, ".png", sep = ""), width = 1000, height = 1000, res = 200)
  wordcloud(words = words.per.topic$words, freq = words.per.topic$weights)
  dev.off()
}
