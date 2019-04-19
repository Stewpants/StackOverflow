#http://tidytextmining.com/topicmodeling.html
getwd()
require(RCurl)
require(tm)
require(XML)
require(RWeka)
#file.edit('~/.Renviron') 
setwd('C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project')
members <- read.csv('membership_info.csv')
summary(members)

####Download the Files####
#setwd('C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/Corpus')

#shell.exec("https://insight.noblis.org/print/resumePdf/30840")
#readLines("https://insight.noblis.org/print/resumePdf/30840")
#getURL("https://sts.noblis.org/adfs/ls/?wa=wsignin1.0&amp;wtrealm=https%3a%2f%2finsight.noblis.org%2f&amp;wctx=rm%3d0%26id%3dpassive%26ru%3d%252fprint%252fresumePdf%252f30840&amp;wct=2017-02-16T20%3a34%3a20Z")

#download_pdf = function(member){
#  base_url  <- "https://insight.noblis.org/print/resumePdf/"
#  dest_file <- paste(member, '.pdf', sep = "")    
#  member_url <- paste(base_url, member, sep = "")
#  download.file(member_url, destfile = dest_file, mode = 'wb')
#}

#download_pdf(30840)

#for (employee in members$Employee.ID){
#  try(download_pdf(employee))
#}
#cert <- system.file("CurlSSL/cacert.pem", package = "RCurl")
#getURL('https://insight.noblis.org/print/resumePdf/30840',  .opts = list(ssl.verifypeer = FALSE))

####Covert to PDF####
wd <- 'C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/Corpus'
setwd('C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/Corpus')
resume_files <- list.files(pattern = '*.pdf', full.names= TRUE)

lapply(resume_files, function(i) system(paste('"C:/Users/M30840/OneDrive - Noblis, Inc/Documents/xpdfbin-win-3.04/bin32/pdftotext.exe"', 
                                              paste0('"', i, '"')), wait = FALSE))

resume_txt <- list.files(pattern = '*.txt', full.names= TRUE)

######PARTS OF SPEECH TAGGING#####
#http://martinschweinberger.de/docs/articles/PosTagR.pdf
detach("package:qdap", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
library(openNLP)

textCorpus <-  Corpus(DirSource(wd, pattern = "*.txt"))
summary(textCorpus)

# for (j in seq(textCorpus)) {
#   textCorpus[[j]] <- gsub("<.*>", " ", textCorpus[[j]])
#   textCorpus[[j]] <- gsub("[\r\n]", " ", textCorpus[[j]])
#   textCorpus[[j]] <- gsub("[[:punct:]]", " ", textCorpus[[j]])
#   textCorpus[[j]] <- gsub("[^[:alnum:]]", " ", textCorpus[[j]])
#   textCorpus[[j]] <- gsub("[^a-zA-Z0-9 ]"," ", textCorpus[[j]])
# }

for (j in seq(textCorpus)) {
  textCorpus[[j]] <- gsub("-", " ", textCorpus[[j]])
}

chunk <- 30
n <- length(textCorpus)
r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(textCorpus,r)

tagged.Corpus <- data.frame()

for (i in 1:length(d)) {
  
  x <- as.String(d[[i]])
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
  gc()
  y2 <- annotate(x, pos_tag_annotator, y1)
  gc()
  y2w <- subset(y2, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  gc()
  
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  r3 <- data.frame(word = x[y2w],tags)
  tagged.Corpus <- rbind(tagged.Corpus, r3)
  gc()
  
  r1 <- NULL
  r2 <- NULL
  r3 <- NULL
  y1 <- NULL
  y2 <- NULL
  y2w <- NULL
}

# inspect results
tagged.Corpus
summary(tagged.Corpus)
summary(tagged.Corpus$tags)

word.list <- data.frame()
word.list <- NULL

for (i in 1:length(tagged.Corpus)) {
  word.list <- rbind(word.list, tagged.Corpus[[i]])
}

write.csv(tagged.Corpus,"C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/word_list.csv")

r1 <- NULL
r2 <- NULL
r3 <- NULL

######CREATE STOP WORD LISTS######
library(dplyr)
library(stringr)
tagged.clean <- tagged.Corpus
tagged.clean$word <- str_replace_all(tagged.clean$word, "[[:punct:]]", " ")
#tagged.clean$word <- str_replace_all(tagged.clean$word, " ", "")
tagged.clean$word <- tolower(tagged.clean$word)

nouns <- subset(tagged.clean, tags == "NN" | tags == "NNS" 
                | tags == "NNP" | tags == "NNPS")
verbs <- subset(tagged.clean, tags == "VB" | tags == "VBD" | tags == "VBG" | tags == "VBN"  
                | tags == "VBP"  | tags == "VBZ")
adjectives <- subset(tagged.clean, tags == "JJ" | tags == "JJR" | tags == "JJS")
adverbs <- subset(tagged.clean, tags == "RB" | tags == "RBR" | tags == "RBS")

the.rest <- anti_join(tagged.clean, nouns, by=c('word', 'tags'))
the.rest <- anti_join(the.rest, verbs, by=c('word', 'tags'))
the.rest <- anti_join(the.rest, adjectives, by=c('word', 'tags'))
the.rest <- anti_join(the.rest, adverbs, by=c('word', 'tags'))

summary(nouns)
summary(verbs)
summary(adjectives)
summary(adverbs)
summary(the.rest)

noun.stop <- anti_join(tagged.clean, nouns, by=c('word'))
verb.stop <- anti_join(tagged.clean, verbs, by=c('word'))
adj.stop <- anti_join(tagged.clean, adjectives, by=c('word'))
adv.stop <- anti_join(tagged.clean, adverbs, by=c('word'))

noun.stop <- paste(noun.stop$word)
noun.stop <- na.omit(noun.stop)
noun.stop <- tolower(unique(noun.stop))
noun.stop <- as.character(noun.stop[!noun.stop == " "])

verb.stop <- paste(verb.stop$word)
verb.stop <- na.omit(verb.stop)
verb.stop <- tolower(unique(verb.stop))
verb.stop <- as.character(verb.stop[!verb.stop == " "])

adj.stop <- paste(adj.stop$word)
adj.stop <- na.omit(adj.stop)
adj.stop <- tolower(unique(adj.stop))
adj.stop <- as.character(adj.stop[!adj.stop == " "])

adv.stop <- paste(adv.stop$word)
adv.stop <- na.omit(adv.stop)
adv.stop <- tolower(unique(adv.stop))
adv.stop <- as.character(adv.stop[!adv.stop == " "])

write.csv(nouns,"C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/nouns.csv")
write.csv(noun.stop,"C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/noun_stop.csv") 

#####Cleaning Corpus####
setwd('C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project')
myCorpus <- Corpus(DirSource(wd, pattern = "*.txt"))
summary(myCorpus)

removeSpecialChars <- function(x) {
  gsub("<.*>", " ", x)
  gsub("[\r\n]", " ", x)
  gsub("[[:punct:]]", " ", x)
  gsub("[^[:alnum:]]", " ", x)
  gsub("[^a-zA-Z0-9 ]"," ",x)
}

noun.stop <- removeSpecialChars(noun.stop)
noun.stop <- gsub(" ", "", noun.stop)
noun.stop <- noun.stop[!noun.stop == " " | !(is.na(noun.stop) | !noun.stop == "")]

verb.stop <- removeSpecialChars(verb.stop)
verb.stop <- gsub(" ", "", verb.stop)
verb.stop <- verb.stop[!verb.stop == " " | !(is.na(verb.stop) | !verb.stop == "")]

adj.stop <- removeSpecialChars(adj.stop)
adj.stop <- gsub(" ", "", adj.stop)
adj.stop <- adj.stop[!adj.stop == " " | !(is.na(adj.stop) | !adj.stop == "")]

adv.stop <- removeSpecialChars(adv.stop)
adv.stop <- gsub(" ", "", adv.stop)
adv.stop <- adv.stop[!adv.stop == " " | !(is.na(adv.stop) | !adv.stop == "")]

manualStopwords <- read.csv("stopwords.csv", header = FALSE)
myStopwords <- c(stopwords("english"), paste(manualStopwords[, 1]))
myStopwords <- stripWhitespace(myStopwords)

#"[\r\n]", "<.*?>", "[[:punct:]]", "[^[:alnum:]]", ".", "[^a-zA-Z0-9 ]"))

myCorpus <- tm_map(myCorpus, content_transformer(removeSpecialChars))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, stripWhitespace)

corpusCopy <- myCorpus

chunk <- 500
n <- length(myStopwords)
r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(myStopwords,r)

for (i in 1:length(d)) {
  myCorpus <- tm_map(myCorpus, removeWords, c(paste(d[[i]])))
}

myCorpus <- tm_map(myCorpus, stemDocument)

#stemCompletion_mod <- function(x,dict=dictCorpus) {
# PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type=c('prevalent')),sep="", collapse=" ")))
#}

#myCorpus <- stemCompletion_mod(myCorpus, dict = corpusCopy)

for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  writeLines(myCorpus[[i]]$content)
}

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
tdm.matrix <- as.matrix(tdm)
#tdm$dimnames <- resume_txt

tdm.mc <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf),tokenize =MC_tokenizer))
tdm.mc

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bi <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf),tokenize =BigramTokenizer))
tdm.bi

library(Matrix)
#dn <- tdm$dimnames
#tdm.matrix <-
#  sparseMatrix(
#    i = tdm$i,
#    j = tdm$j,
#    x = tdm$v,
#    dims = c(tdm$nrow, tdm$ncol),
#    dimnames = dn
#  )
summary(tdm.matrix)

#######Latent Semantic Analysis#######
#######Distance closest to 1 is best
detach("package:dplyr", unload = TRUE)
library(lsa)

rowTotals <- apply(tdm.matrix , 1, sum) #Find the sum of words in each Document
tdm.matrix.new   <- tdm.matrix[rowTotals > 0, ]

myMatrix <- lw_tf(tdm.matrix.new) * gw_idf((as.matrix(tdm.matrix.new)))
myLSAspace <- lsa(myMatrix, dims = dimcalc_share())
as.textmatrix(myLSAspace)

myLSA <- myLSAspace$dk
distance <- cosine(t(myLSA))

desc.func <-  function(LSA, item) {
  output <- data.frame(x = 1:nrow(LSA))
  for (i in 1:nrow(output)) {
    output[i,] <- cosine(as.vector(LSA[item,]), as.vector(LSA[i,]))
  }
  return(output)
}

ind.distance <- desc.func(myLSA, 13)
writeLines(myCorpus[[13]]$content)
writeLines(myCorpus[[54]]$content)
summary(myCorpus)

output <- cosine(as.vector((myLSA[12,])), as.vector((myLSA[2,])))

match1 <- tdm.matrix.new[,12]
match2 <- tdm.matrix.new[,2]
similar <- rbind(match1, match2)

write.csv(
  t(distance),
  "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/lsa_distance.csv"
)

#######FREQUENT WORDS AND ASSOCATIONS########
idx <- which(dimnames(tdm)$Terms == "model")
inspect(tdm[idx + (0:5), 5:10])

(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

term.freq <- rowSums(tdm.matrix)
term.freq <- subset(term.freq, term.freq >= 100)

df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

findAssocs(tdm, 'model', 0.5)
findAssocs(tdm, 'statist', 0.5)
findAssocs(tdm, 'aerospac', 0.5)
#######WORDCLOUD########
library(wordcloud)
library(RColorBrewer)
uni.dn <- tdm$dimnames
uni.sparse <-
  sparseMatrix(
    i = tdm$i,
    j = tdm$j,
    x = tdm$v,
    dims = c(tdm$nrow, tdm$ncol),
    dimnames = uni.dn
  )
word.freq <- sort(rowSums(uni.sparse), decreasing = T)
wordcloud(
  words = names(word.freq),
  freq = word.freq,
  min.freq = 200,
  random.order = F,
  colors= brewer.pal(8,"Dark2")
)

bi.dn <- tdm.bi$dimnames
bi.sparse <-
  sparseMatrix(
    i = tdm.bi$i,
    j = tdm.bi$j,
    x = tdm.bi$v,
    dims = c(tdm.bi$nrow, tdm.bi$ncol),
    dimnames = bi.dn
  )
word.freq <- sort(rowSums(bi.sparse), decreasing = T)
wordcloud(
  words = names(word.freq),
  freq = word.freq,
  min.freq = 50,
  random.order = F,
  colors= brewer.pal(8,"Dark2")
)

#######CLUSTERING########
tdm2 <- removeSparseTerms(tdm, sparse = .2)
tdm2
dn <- tdm2$dimnames
m2 <-
  sparseMatrix(
    i = tdm2$i,
    j = tdm2$j,
    x = tdm2$v,
    dims = c(tdm2$nrow, tdm2$ncol),
    dimnames = dn
  )

distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 4)

m3 <- t(m2)
set.seed(12)
k <- 4
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3)

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i,], decreasing = T)
  cat(names(s)[1:5], "nn")
}

# partitioning around medoids with estimation of number of clusters
library(fpc)
pamResult <- pamk(m3)
k <- pamResult$nc
pamResult <- pamResult$pamobject

for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i, ] == 1)], "nn")
}

pamResult

par(mfrow = c(1, 2))
plot(pamResult, col.p = pamResult$clustering)
par(mfrow = c(1, 1))

#######LDA TOPIC MODELLING ########
library(topicmodels)
dtm <- as.DocumentTermMatrix(tdm)
lda.out <- LDA(dtm, k = 25) # find n topics
term <- terms(lda.out, 5) # first 5 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

topicProbabilities <- as.data.frame(lda.out@gamma)
rownames(topicProbabilities) <- dtm$dimnames$Docs
colnames(topicProbabilities) <- term
summary(topicProbabilities)
topicProbabilities

write.csv(topicProbabilities, 'C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/topic_probs.csv')

topic <- topics(lda.out, 1)
#topics <- data.frame(date=as.Date(comp_df$Date.sent.to.company), topic)
#qplot(date, ..count.., data=topics, geom="density", fill=term[topic], position="stack")

library(tidytext)

ap_lda_td <- tidy(lda.out)
ap_lda_td

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

library(tidyr)

beta_spread <- ap_lda_td %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread


###Optimized Topic Model ####
###Do Not Run, Takes a very long time!####
# best.model <- lapply(seq(2,50, by=1), function(k){LDA(dtm, k)})
# best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
# 
# best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
# 
# ggplot(best.model.logLik.df, aes(x=topics, y=LL)) +
#   xlab("Number of topics") + ylab("Log likelihood of the model") +
#   geom_line() +
#   theme_bw()
# 
# best.model.logLik.df[which.max(best.model.logLik.df$LL),]
# lda_AP <- LDA(dtm, 26)   # generate the model with 26 topics
# term <- terms(lda_AP, 5)                         # gets 5 keywords for each topic, just for a quick look
# topics(lda_AP, 5)                        # gets 5 topic numbers per document
# 
# term <- apply(term, MARGIN = 2, paste, collapse = ", ")
# 
# topicProbabilities <- as.data.frame(lda_AP@gamma)
# rownames(topicProbabilities) <- dtm$dimnames$Docs
# colnames(topicProbabilities) <- term
# summary(topicProbabilities)
# topicProbabilities

#######Eculidean DISTANCE #######
####Lower Distance is better
vs <- VectorSource(myCorpus)
vd <- dist(t(uni.sparse))
resume_dist <- as.data.frame(as.matrix(vd))
write.csv(resume_dist, "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/rank.csv")


wt_tdm <- weightTfIdf(tdm, normalize = TRUE)
w <- as.matrix(wt_tdm)
vw <- dist(t(w))
weighted_dist <- as.data.frame(as.matrix(vw))
write.csv(weighted_dist, "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/weighted_rank.csv")

#######Bigram Euclidian DISTANCE ########
wt_tdm.bi <- weightTfIdf(tdm.bi, normalize = TRUE)
w.bi <- as.matrix(wt_tdm.bi)
vw.bi <- dist(t(w.bi))
weighted_dist.bi <- as.data.frame(as.matrix(vw.bi))
write.csv(weighted_dist.bi,
          "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/bigram_weighted_rank.csv")

#######Gale Shapely Matching########
require(matchingR)

prep.rank <- replace(distance, distance == 1, -1)

jobs <- prep.rank[(ncol(prep.rank)-5):ncol(prep.rank), 1:(ncol(prep.rank)-6)]
applicants <- prep.rank[1:(ncol(prep.rank)-6),1:(ncol(prep.rank)-6)]

nrow(applicants)
ncol(t(jobs))

rank.app <- apply(t(jobs), 1, rank, ties.method='max')
rank.jobs <- apply(jobs, 1, rank, ties.method='max')

nrow(rank.app)
ncol(rank.jobs)

ranking <- galeShapley.collegeAdmissions(prep.rank, t(prep.rank), slots = 10)
ranking.jobs <- galeShapley.collegeAdmissions(rank.app, rank.jobs, 
                                              slots = 10, studentOptimal = TRUE)

matches <- data.frame(cbind(rownames(prep.rank), rownames(prep.rank)[ranking$proposals[,]]))

matches.app <- data.frame(ranking.jobs$matched.colleges[,])
rownames(matches.app) <- colnames(rank.jobs)
rownames(rank.jobs)[ranking.jobs$matched.colleges[1,2]]

for (i in 1:ncol(matches.app)) {
  for (j in 1:nrow(matches.app)){
    print(matches.app[j,i])
    matches.app[j,i] <- rownames(rank.jobs)[ranking.jobs$matched.colleges[j,i]]
  }
}

matches.app

write.csv(matches.app,
          "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/Gale_Shapely_match.csv")

##### Page Ranks Methodology ####
# The PageRank algorithm outputs a probability distribution used to represent the 
# likelihood that a person randomly clicking on links will arrive at any particular page. 
# PageRank can be calculated for collections of documents of any size. 
# It is assumed in several research papers that the distribution is evenly divided among 
# all documents in the collection at the beginning of the computational process. 
# The PageRank computations require several passes, called "iterations", through the 
# collection to adjust approximate PageRank values to more closely reflect the theoretical 
# true value.
library(igraph)

match.graph <- graph_from_adjacency_matrix(distance, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(match.graph,  edge.label=round(E(match.graph)$weight, 3))
match.page <- page_rank(match.graph, vids = V(match.graph))

write.csv(match.page$vector,
          "C:/Users/M30840/OneDrive - Noblis, Inc/Membership Project/match_page.csv")