#' test
#'
#' methods: LDA wrapper, fit
#' @import SnowballC
#@export "test"

#install.packages("SnowballC")
#library("SnowballC")
devtools::use_package("SnowballC")
test <- function(){
  data <- rjson::fromJSON(file = "rlda/data.json")
  df<- plyr::ldply(data, data.frame)
  x = tm::SimpleCorpus(tm::VectorSource(df$speech), control=list(language ="en"))
  dtm = tm::DocumentTermMatrix(x, control=list(language="en",removePunctuation=TRUE, stopwords=TRUE, removeNumbers=TRUE,stemming=TRUE, tolower=TRUE))
  lda = topicmodels::LDA(dtm, 5)
  r = new("rlda", dtm=dtm, lda_u=lda)
  r=rlda::fit(r)
  r = rlda::compute_sim(r)
  r = rlda::getTopicInDoc(r)
  return(r)
  }
