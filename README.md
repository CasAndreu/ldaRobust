# ldaRobust
This is a package to assess LDA topic instability and robustness


## How to install

Eventually the package will be on CRAN. However, for now download a copy of the package in your local machine and follow the instrunctions below in order to install it.

1. Make sure you have the following dependencies installed:
#install.packages("devtools")
#install.packages("SnowballC")

2. Make sure to change the following directory to be parent directory of the ldaRobust folder on your machine (comment out if working directory already set to correct working directory)
```
dir = "."
setwd(dir)
devtools::install("ldaRobust")
```

## Short Example

1. Load data (a sample of Congressional one-minute floor speeches)
```
print("loading data")
setwd("./ldaRobust")
data <- rjson::fromJSON(file = "data.json")
print("data has been loaded")
```

2. Select a random sample of only 1,000 floor speeches to speed up computation in this example
```
data <- data[sample(x = 1:length(data), size = 1000, replace = FALSE)]
```

3. Transform the documents in this sample dataset to a Document Term Matrix
```
df<- plyr::ldply(data, data.frame)
x = tm::SimpleCorpus(tm::VectorSource(df$speech), control=list(language ="en"))
dtm = tm::DocumentTermMatrix(x, control=list(language="en",removePunctuation=TRUE, stopwords=TRUE, removeNumbers=TRUE,stemming=TRUE, tolower=TRUE))
print("document term matrix created")
```

4. Run a first original LDA model with 5 topics.
```
print("running first LDA")
lda = topicmodels::LDA(dtm, 5)
print("LDA has been created")
```

5. Create `rlda` object with default values (i.e. K = 5, no random seeding, similarity threshold for topicInDoc is 0.5)
```
print("creating rlda object")
r = new("rlda", dtm=dtm, lda_u=lda)
print("rlda object created")
```

6. Run `ldaRobust::fit`, `compute_sim` and `getTopicInDoc`
```
print("running fit function")
r=rlda::fit(r)
print("running compute sim function")
r_sim = rlda::compute_sim(r)
print("running getTopicInDoc function")
r_top = rlda::getTopicInDoc(r)
```
