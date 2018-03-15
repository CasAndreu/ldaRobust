
#install.packages("devtools")
#install.packages("SnowballC")
# load and install rlda

# please make sure to change the following directory to be in the rlda folder on ur machine (comment out if working directory already set to correct working directory)
setwd("./ldaRobust")
devtools::document()
setwd("..")
devtools::install("ldaRobust")

# load data (congress dataset)
print("loading data")
setwd("./ldaRobust")
data <- rjson::fromJSON(file = "data.json")
print("data has been loaded")

# transform to document term matrices
df<- plyr::ldply(data, data.frame)
x = tm::SimpleCorpus(tm::VectorSource(df$speech), control=list(language ="en"))
dtm = tm::DocumentTermMatrix(x, control=list(language="en",removePunctuation=TRUE, stopwords=TRUE, removeNumbers=TRUE,stemming=TRUE, tolower=TRUE))
print("document term matrix created")

# run original lda model with 5 topics
print("running first LDA")
lda = topicmodels::LDA(dtm, 5)
print("LDA has been created")

# create rlda object with default value(i.e. K = 5, no random seeding, similarity threshold for topicInDoc is 0.5)
print("creating rlda object")
r = new("rlda", dtm=dtm, lda_u=lda)
print("rlda object created")

# run lda fit, compute_sim and getTopicInDoc
print("running fit function")
r=rlda::fit(r)
print("running compute sim function")
r_sim = rlda::compute_sim(r)
print("running getTopicInDoc function")
r_top = rlda::getTopicInDoc(r)
