# Load required libraries
library(tm) #text mining

# Data from: https://www.kaggle.com/deepak711/4-subject-data-text-classification, uploaded by Deepak T
# The different documents are splitted into seven categories: 
# computer science, history, maths, accounts, biology, geography and physics. 

# Source directories
source.comp = DirSource("./files/Computer_Science/Computer_Science", encoding = "UTF-8")
source.hist = DirSource("./files/History", encoding = "UTF-8")
source.math = DirSource("./files/Maths", encoding = "UTF-8")
source.acc = DirSource("./files/Physics_Biology_Geography_Accounts subject training data for text classification/train_data_final/accounts", encoding = "UTF-8")
source.bio = DirSource("./files/Physics_Biology_Geography_Accounts subject training data for text classification/train_data_final/biology", encoding = "UTF-8")
source.geo = DirSource("./files/Physics_Biology_Geography_Accounts subject training data for text classification/train_data_final/geography", encoding = "UTF-8")
source.phy = DirSource("./files/Physics_Biology_Geography_Accounts subject training data for text classification/train_data_final/physics", encoding = "UTF-8")

# corpora from the source directories
# Since we want to obtain a frequency term matrix for each category, we need a corpus per category
corpusComp = Corpus(source.comp)
corpusHist = Corpus(source.hist)
corpusMath = Corpus(source.math)
corpusAcc = Corpus(source.acc)
corpusBio = Corpus(source.bio)
corpusGeo = Corpus(source.geo)
corpusPhy = Corpus(source.phy)

##############################
#EXPLORATION OF THE corpora

# Number of files per category:
sprintf("Computer Science: %i", length(corpusComp))
sprintf("History: %i", length(corpusHist))
sprintf("Maths: %i", length(corpusMath))
sprintf("Accounts: %i", length(corpusAcc))
sprintf("Biology: %i", length(corpusBio))
sprintf("Geography: %i", length(corpusGeo))
sprintf("Physics: %i", length(corpusPhy))

# Take a look at the first file of each corpus
# We can see that maybe it could be relevant to keep numbers in the corpora of computer science and math.
# Also there are some punctuation symbols like -, <, > that could be relevant also
inspect(corpusComp[1])
inspect(corpusHist[1])
inspect(corpusMath[1])
inspect(corpusAcc[1])
inspect(corpusBio[1])
inspect(corpusGeo[1])
inspect(corpusPhy[1])

# Take a look at the content of the first file of each corpus
inspect(corpusComp[[1]])
inspect(corpusHist[[1]])
inspect(corpusMath[[1]])
inspect(corpusAcc[[1]])
inspect(corpusBio[[1]])
inspect(corpusGeo[[1]])
inspect(corpusPhy[[1]])

# To inspect the metadata: meta(corpusComp[[1]])


##############################
#EXPLORATION OF THE CORPUS Computer Science

# Term document matrix from the corpus
# We can see that there have been identified 23060 terms in the corpus
tdm = TermDocumentMatrix(corpusComp)
inspect(tdm)

# Terms identified in the corpus 
# We can see that there are some words like "\\-vhat" , "i•", "\"!\"" that should not be considered words
# That is why we need to apply to the corpus some transformations like remove punctuation, stop words, etc.
# BUT as we have seen before, we have to look carefully at each corpus in order to 
# identify which transformations are relevant or not
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# As it is said before, maybe we are not interested in removing numbers or some symbols from this corpus.
# Looking at the hundred most frequent terms we can see that no numbers appear 
# and also the only term that is not a word is •
# So we can conclude that all the possible transformations (removeNumbers, removePunctuation, ...) should be applied
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)


##############################
#EXPLORATION OF THE CORPUS History

# Term document matrix from the corpus
# We can see that there have been identified 40200 terms in the corpus
tdm = TermDocumentMatrix(corpusHist)
inspect(tdm)

# Terms identified in the corpus 
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# Looking at the hundred most frequent terms we can see that all the terms are words 
# So we can conclude that all the possible transformations (removeNumbers, removePunctuation, ...) should be applied
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)



##############################
#EXPLORATION OF THE CORPUS Maths

# Term document matrix from the corpus
# We can see that there have been identified 4914 terms in the corpus
# We have a lower number of terms than in the two previous corpora because the number of documents is also lower.
tdm = TermDocumentMatrix(corpusMath)
inspect(tdm)

# Terms identified in the corpus 
# We can see that there are some words like "10x=" , "3(x" that should not be considered words
# That is why we need to apply to the corpus some transformations like remove punctuation, stop words, etc.
# BUT as we have seen before, we have to look carefully at each corpus in order to 
# identify which transformations are relevant or not, because maybe in this corpus 
# of math documents, numbers are  relevant
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# As it is said before, maybe we are not interested in removing numbers or some symbols from this corpus.
# Looking at the hundred most frequent terms we can see some terms that are meaningless:
# "www.ebook3000.com" at position 35
# "−" at position 63
# Since "www.ebook3000.com" appears at the end of some documents and it looks like the source of the files, 
# it will be deleted, because is not a term specific for mathematics
# Since no numbers appears we can apply removeNumbers
# BUT since "-" is a frequent term we need to select which punctuations characters should be deleted
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)

##############################
#EXPLORATION OF THE CORPUS Accounts

# Term document matrix from the corpus
# We can see that there have been identified 6621 terms in the corpus
# Again, we have a lower number of terms than in the two first corpora because the number of documents is also lower.
tdm = TermDocumentMatrix(corpusAcc)
inspect(tdm)

# Terms identified in the corpus 
# We can see that there are some words like "?700" , "........." that should not be considered words
# That is why we need to apply to the corpus some transformations like remove punctuation, stop words, etc.
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# As it is said before, maybe we are not interested in removing numbers or some symbols from this corpus.
# Looking at the hundred most frequent terms we can see some numbers appear at the latest positions
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)

# Now we need to know if numbers should be deleted or not. 
# We have to look now where are numbers when we remove the stopwords
# We can see that at position 39 and 40 we find 100 and 000, at 52 10000, etc.
# We can conclude that maybe we are not interested in removing numbers
tdm = TermDocumentMatrix(corpusAcc, control=list(removePunctuation = TRUE, stopwords = TRUE))
freq = rowSums(as.matrix(tdm))
tail(sort(freq),n=100)



##############################
#EXPLORATION OF THE CORPUS Biology

# Term document matrix from the corpus
# We can see that there have been identified 24798 terms in the corpus
tdm = TermDocumentMatrix(corpusBio)
inspect(tdm)

# Terms identified in the corpus 
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# Looking at the hundred most frequent terms we can see:
# "−" at position 36
# Since no numbers appears, we can apply removeNumbers
# BUT since "-" is a frequent term we need to select which punctuations characters should be deleted
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)


##############################
#EXPLORATION OF THE CORPUS Geography

# Term document matrix from the corpus
# We can see that there have been identified 6967 terms in the corpus
# Again, we have a lower number of terms than in the two first corpora because the number of documents is also lower.
tdm = TermDocumentMatrix(corpusGeo)
inspect(tdm)

# Terms identified in the corpus 
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# As it is said before, maybe we are not interested in removing numbers or some symbols from this corpus.
# Looking at the hundred most frequent terms we can see: 
# "-" at 26 position
# Since no numbers appears, we can apply removeNumbers
# BUT since "-" is a frequent term we need to select which punctuations characters should be deleted
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)


##############################
#EXPLORATION OF THE CORPUS Physics

# Term document matrix from the corpus
# We can see that there have been identified 28793 terms in the corpus
tdm = TermDocumentMatrix(corpusPhy)
inspect(tdm)

# Terms identified in the corpus 
# We can see that there are some words like "‘f—jr—and", "=(rq)ri;+<r2)", ... that should not be considered words
# That is why we need to apply to the corpus some transformations like remove punctuation, stop words, etc.
dimnames(tdm)$Terms

# Frequency of each term (sum of the frequency of a term in each document)
freq = rowSums(as.matrix(tdm))

# As it is said before, maybe we are not interested in removing numbers or some symbols from this corpus.
# Looking at the hundred most frequent terms we can see: 
# "-" at position 28
# Since no numbers appears, we can apply removeNumbers
tail(sort(freq),n=100)

# Number of terms only appearing once
sum(freq == 1)

##############################
# TRANSFORMATIONS
# We have seen that the characer "-" could be a relevant term when predicting the category
# but since it appears in math, biology, geography and physics it is not so important when classifying,
# so we can apply removePunctuation in every corpus.

# Stop words to be removed:
stopwords()

# Create another term document matrix after applying the corresponding transformations
tdmComp = TermDocumentMatrix(corpusComp, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmHist = TermDocumentMatrix(corpusHist, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

myStopwords = c(stopwords(),"www.ebook3000.com","wwwebookcom")
tdmMath = TermDocumentMatrix(corpusMath, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = myStopwords,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmAcc = TermDocumentMatrix(corpusAcc, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmBio = TermDocumentMatrix(corpusBio, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmGeo = TermDocumentMatrix(corpusGeo, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmPhy = TermDocumentMatrix(corpusPhy, control=list(removePunctuation = TRUE,
                                                      tolower=TRUE,
                                                      stopwords = TRUE,
                                                      removeNumbers = TRUE,
                                                      stemming = TRUE))




#####################################
# SLITTING INTO TRAINING AND TEST
l = length(corpusComp)
trainingComp = corpusComp[1:(round(l*0.7))]
testComp = corpusComp[(1+(round(l*0.7))):l]

l = length(corpusHist)
trainingHist = corpusHist[1:(round(l*0.7))]
testHist = corpusHist[(1+(round(l*0.7))):l]

l = length(corpusMath)
trainingMath = corpusMath[1:(round(l*0.7))]
testMath = corpusMath[(1+(round(l*0.7))):l]

l = length(corpusAcc)
trainingAcc = corpusAcc[1:(round(l*0.7))]
testAcc = corpusAcc[(1+(round(l*0.7))):l]

l = length(corpusBio)
trainingBio = corpusBio[1:(round(l*0.7))]
testBio = corpusBio[(1+(round(l*0.7))):l]

l = length(corpusGeo)
trainingGeo = corpusGeo[1:(round(l*0.7))]
testGeo = corpusGeo[(1+(round(l*0.7))):l]

l = length(corpusPhy)
trainingPhy = corpusPhy[1:(round(l*0.7))]
testPhy = corpusPhy[(1+(round(l*0.7))):l]



#####################################
# TRAINING

# Goal: obtain a TF-IDF weighting
# But since we are working with seven corpora, we will have different formulas
# w_ij = tf_ij * idf_j
# tf_ij = number of occurencies of the term i in the corpus j 
# (notice that it will not matter if term i appears 1 or 10 times in a given document)
# idf_i = log(number of corpora(7)/number of corpora where appears the term i)
# For the tf_ij we need to take into account the size of the corpus

#####################################
# First we obtain the term document matrix of each corpus
tdmComp = TermDocumentMatrix(trainingComp, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                      stopwords = TRUE, removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmHist = TermDocumentMatrix(trainingHist, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                      stopwords = TRUE, removeNumbers = TRUE,
                                                      stemming = TRUE))

myStopwords = c(stopwords(),"www.ebook3000.com","wwwebookcom")
tdmMath = TermDocumentMatrix(trainingMath, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                      stopwords = myStopwords, removeNumbers = TRUE,
                                                      stemming = TRUE))

tdmAcc = TermDocumentMatrix(trainingAcc, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                    stopwords = TRUE, removeNumbers = TRUE,
                                                    stemming = TRUE))

tdmBio = TermDocumentMatrix(trainingBio, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                    stopwords = TRUE, removeNumbers = TRUE,
                                                    stemming = TRUE))

tdmGeo = TermDocumentMatrix(trainingGeo, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                    stopwords = TRUE, removeNumbers = TRUE,
                                                    stemming = TRUE))

tdmPhy = TermDocumentMatrix(trainingPhy, control=list(removePunctuation = TRUE, tolower=TRUE,
                                                    stopwords = TRUE, removeNumbers = TRUE,
                                                    stemming = TRUE))
#####################################
# We want to create now tf_ij for each corpus
# Remember we do not care the times a term appears in a document
freqRow <- function(row){
  s = 0
  for (r in row){
    if (r != 0){
      s = s + 1
    }
  }
  return(s)
}

# Also, since each corpus has a different number of documents, we need to scale the matrices
freq = apply(tdmComp, 1, freqRow)
freq = as.matrix(freq)
freqComp = freq/length(trainingComp)

freq = apply(tdmHist, 1, freqRow)
freq = as.matrix(freq)
freqHist = freq/length(trainingHist)

freq = apply(tdmMath, 1, freqRow)
freq = as.matrix(freq)
freqMath = freq/length(trainingMath)

freq = apply(tdmAcc, 1, freqRow)
freq = as.matrix(freq)
freqAcc = freq/length(trainingAcc)

freq = apply(tdmBio, 1, freqRow)
freq = as.matrix(freq)
freqBio = freq/length(trainingBio)

freq = apply(tdmGeo, 1, freqRow)
freq = as.matrix(freq)
freqGeo = freq/length(trainingGeo)

freq = apply(tdmPhy, 1, freqRow)
freq = as.matrix(freq)
freqPhy = freq/length(trainingPhy)

# Now we have to reduce the matrices
# Delete terms that has a frequency lower than 0.25 --> they would be deleted since they are not relevant to classify
tj_comp = subset(freqComp, freqComp[,1] > 0.25)
tj_hist = subset(freqHist, freqHist[,1] > 0.25)
tj_math = subset(freqMath, freqMath[,1] > 0.25)
tj_acc = subset(freqAcc, freqAcc[,1] > 0.25)
tj_bio = subset(freqBio, freqBio[,1] > 0.25)
tj_geo = subset(freqGeo, freqGeo[,1] > 0.25)
tj_phy = subset(freqPhy, freqPhy[,1] > 0.25)


#####################################
# Once we have tf_ij, we need to compute idf_i = log(number of corpora(7)/number of corpora where appears the term i)
# First, we obtain all the terms, and then a matrix with the references of each term to the categories is computed
# Terms
l_comp = as.list(rownames(tj_comp))
l_hist = as.list(rownames(tj_hist))
l_math = as.list(rownames(tj_math))
l_acc = as.list(rownames(tj_acc))
l_bio = as.list(rownames(tj_bio))
l_geo = as.list(rownames(tj_geo))
l_phy = as.list(rownames(tj_phy))

# The elements of l1 will be added to l2 if they are not already there
fun <- function(l1, l2){
  for (l in l2){
    if (is.element(l, l1)){
      TRUE
    }
    else{
      l1 <- append(l1, l)
    }
  }
  return(l1)
}
names <- fun(l_comp,l_hist)
names <- fun(names,l_math)
names <- fun(names,l_acc)
names <- fun(names,l_bio)
names <- fun(names,l_geo)
names <- fun(names,l_phy)

# Matrix with references
dt <- c()
for (term in names){
  if(is.element(term,l_comp)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_hist)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_math)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_acc)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_bio)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_geo)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_phy)){
    dt <- c(dt,1)
  }
  else{
    dt <- c(dt,0)
  }
}

references <- t(matrix(data=dt, dimnames= list(c("Comp","Hist","Math","Acc","Bio","Geo","Phy"),names), nrow=7))

idf <- as.matrix(rowSums(references))
idf <- 7/idf
idf <- log10(idf)


# tf_ij
dt <- c()
for (term in names){
  if(is.element(term,l_comp)){
    dt <- c(dt,tj_comp[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_hist)){
    dt <- c(dt,tj_hist[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_math)){
    dt <- c(dt,tj_math[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_acc)){
    dt <- c(dt,tj_acc[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_bio)){
    dt <- c(dt,tj_bio[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_geo)){
    dt <- c(dt,tj_geo[term,])
  }
  else{
    dt <- c(dt,0)
  }
  
  if(is.element(term,l_phy)){
    dt <- c(dt,tj_phy[term,])
  }
  else{
    dt <- c(dt,0)
  }
}

tf_ij <- t(matrix(data=dt, dimnames= list(c("Comp","Hist","Math","Acc","Bio","Geo","Phy"),names), nrow=7))



## w_ij = tf_ij * idf_j
w_ij = tf_ij * idf[,1]


#####################################
# SYSTEM

# The logic behind the prediction consist on:
# 1. Calculate the term document matrix of the given document
# 2. Which category fits better that document?
# 2.1. Category = column of w_ij
# 3. Calculate the sum of the category weighted column times the term document column 
# 3.1. M = term document matrix of the document that we want to predict the category
# 3.2. For each category j in w_ij: cs_j = sum(i*w_ij) where i is a term of M
# 4. Predicted category = max(j) where cs_j
cs_j <- function(index, tdm_doc){
  # Time the column of a category with the corresponding element
  m <- as.matrix(tdm_doc)
  res <- 0
  for (row in rownames(m)){
    if (is.element(row, rownames(w_ij))){
      res <- res + (m[row,]*w_ij[row,index])
    }
  }
  return(res)
}

predict_cat <- function(doc){
  # Since all the matrices from all the categories have the same transformations,
  # except math that it includes some stopwords, we apply those transformations
  # to the new document when computing the term document matrix
  myStopwords = c(stopwords(),"www.ebook3000.com","wwwebookcom")
  tdm <- TermDocumentMatrix(doc, control=list(removePunctuation = TRUE, tolower = TRUE,
                                             stopwords = myStopwords, removeNumbers = TRUE,
                                             stemming = TRUE))
  # Compute the cs_j for each category
  comp <- cs_j("Comp",tdm)
  hist <- cs_j("Hist",tdm)
  math <- cs_j("Math",tdm)
  acc <- cs_j("Acc",tdm)
  bio <- cs_j("Bio",tdm)
  geo <- cs_j("Geo",tdm)
  phy <- cs_j("Phy",tdm)
  
  return(which.max(c(comp,hist,math,acc,bio,geo,phy)))
}

#####################################
# TESTING
# Accuracy = number of corrected predictions/total predictions
# Calculate the accuracy for each category
numberCorrect <- function(corpus, indexCorpus){
  correct <- 0
  for (i in 1:length(corpus)){
    p <- predict_cat(corpus[i])
    aux <- 0
    if (indexCorpus == p){
      aux <- 1
    }
    correct <- correct + aux
  }
  return(correct)
}

accuracy <- c()

# Accuracy of Computer Science
numCorrect <- numberCorrect(testComp, 1)
allAnswers <- length(testComp)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of History
numCorrect <- numberCorrect(testHist, 2)
allAnswers <- length(testHist)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of Math
numCorrect <- numberCorrect(testMath, 3)
allAnswers <- length(testMath)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of Accounts
numCorrect <- numberCorrect(testAcc, 4)
allAnswers <- length(testAcc)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of Biology
numCorrect <- numberCorrect(testBio, 5)
allAnswers <- length(testBio)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of Geography
numCorrect <- numberCorrect(testGeo, 6)
allAnswers <- length(testGeo)
accuracy <- c(accuracy,numCorrect/allAnswers)

# Accuracy of Physics
numCorrect <- numberCorrect(testPhy, 7)
allAnswers <- length(testPhy)
accuracy <- c(accuracy,numCorrect/allAnswers)

matrix(accuracy, dimnames=list(c("Computer Science", "History", "Maths",
                                 "Accounts", "Biology", "Geography", "Physics"),c("accuracy")))

#####################################
# ANALYSIS OF THE RESULTS
# Why biology and geography has does low values?
# Geography: We see that it is always predicted as history (2) except once that is predicted as computer science (1)
for (i in 1:length(testGeo)){
  p <- predict_cat(testGeo[i])
  aux <- 0
  if (6 == p){
    aux <- 1
  }
  else{
    print(p)
  }
}

# Biology: We see that most of the times it is predicted as history (2). Also physics (7), geography (6), and computer science (1)
for (i in 1:length(testBio)){
  p <- predict_cat(testBio[i])
  aux <- 0
  if (5 == p){
    aux <- 1
  }
  else{
    print(p)
  }
}

