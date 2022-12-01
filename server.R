library(shiny)
library(stringr)
library(tm)
library(RWeka)
library(purrr)

rm(corpus)
#corpus <- readRDS('corpus.RData')
corpus <- readRDS('corpus.RDS')
#corpus <- readRDS(url('https://github.com/KP0114/Data_Science_Capstone/blob/671b3c1f4a48e8e1866af23ec70a9dd1da2a1913/corpus.RData'))

#corpus2 <- readRDS(gzcon(url('https://github.com/KP0114/Data_Science_Capstone/blob/main/corpus.RDS')))
#corpus2 <- readRDS(url('https://github.com/KP0114/Data_Science_Capstone/blob/fad4c652491de3b9bb249fe8f565162ff407959d/corpus.RDS'))

#x <- RCurl::getURL('https://github.com/KP0114/Data_Science_Capstone/blob/fad4c652491de3b9bb249fe8f565162ff407959d/corpus.RDS')


#u <- 'https://github.com/KP0114/Data_Science_Capstone/blob/main/corpus.RDS'
#data <- readRDS(url(u,method="libcurl"))

BackoffModels <- function(n){
  BackoffModel <<- list()
  for(i in 2:n){
    BackoffModel[[paste(i,"grams")]] <<- createNgrams(corpus,i)
  }
}

createNgrams <- function(text, n){ 
  ngram <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  ngrams <- TermDocumentMatrix(text, control = list(tokenize = ngram))
  ngrams_freq <- rowSums(as.matrix(ngrams))
  ngrams_freq <- sort(ngrams_freq, decreasing = TRUE)
  ngrams_freq_df <- data.frame(word = names(ngrams_freq), freq=ngrams_freq)
  
  ngrams_freq_df
  
}

extractLowerGram <- function(x,n){
  
  x <- strsplit(as.character(x), ' ' )
  x <- head(x[[1]],n-1)
  x <- paste(x,collapse = ' ' )
  x
}
predict <- function(x,n) {
  xs <- stripWhitespace(stemDocument(removePunctuation(tolower(removeNumbers(x)))))
  
  if(n > length(strsplit(xs,' ')[[1]])){
    n <- length(strsplit(xs,' ')[[1]])
    n <- n+1
  }
  
  if( n >= 2){
    xs <- strsplit(xs, ' ' )
    xs <- tail(xs[[1]],n-1)
    xs <- paste(xs,collapse = ' ' )
  }
  currentModel <- BackoffModel[[paste(n,"grams")]]
  
  currentModel$lowerGram <- lapply(currentModel[['word']],extractLowerGram,n)
  
  matchList <- currentModel[currentModel$lowerGram == xs,]
  
  if(dim(matchList)[1] != 0){
    candidateList <- head(as.character(matchList[['word']]),3)
    candidateList <- lapply(candidateList,function(x){tail(strsplit(x[[1]]," "),1)[[1]][[n]]})
    mesg <<- paste("Next word is predicted using ",n,"gram.")
    candidateList
  }
  else if(n == 2){
    mesg<<- "No Matches Found"
  }
  else{
    predict(xs,n-1)
  }
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$prediction <- renderText({
    BackoffModels(input$Ngram)
    result <- predict(input$inputString,input$Ngram)
    output$text2 <- renderText({mesg})
    paste(input$inputString,result,',')
  });
  output$text1 <- renderText({input$inputString});
})
