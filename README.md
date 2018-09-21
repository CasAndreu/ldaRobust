# ldaRobust
This is a package to assess LDA topic instability and robustness

## Authors
[Andreu Casas](http://andreucasas.com/) (Moore-Sloan Fellow, Center for Data Science, New York University)

Tianyi Bi (Center for Data Science, New York University)

## Sponsors
We are greatful to the [Moore Sloan Data Science Enviornment](http://msdse.org/) for supporting the development of this research project and open-source software.


## How to install

Eventually the package will be on CRAN... but for now, use the following instructions to install it in your local machine.

1. Download or Clone a copy of this `ldaRobust` repository-directory.
Click on the green `Clone or download` button on the upper right corner of the page to get the cloning link or to download a zipped version of the repository.

2. Make sure you have `devtools` installed in order to download and install `ldaRobust` from GitHub.
If you don't have it, type:
```
install.packages("SnowballC")
```

2. Make sure to change the working directory to the parent directory of the ldaRobust folder in your machine.
For example, I have the `ldaRobust` directory in a `repos` directory in my Desktop. So in order to install the package I do:
```
dir <- "/Users/andreucasas/Desktop/repos/"
setwd(dir)
devtools::install("ldaRobust")
```

## Short Example

1. Load a document term matrix (DTM) that comes with the package: `grimmer_dtm.RData`. These are pre-processed press releases from U.S. Senators collected by Justin Grimmer, and they are part of the replication material for his article in the _American Journal of Political Science_: [Appropriators not Position Takers: The Distorting Effects of Electoral Incentives on Congressional Representation](https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12000).
```
setwd("./ldaRobust")
print(load("./data/grimmer_dtm.RData)) # - this will load a dtm object named 'grimmer_dtm'
```

2. Select a random sample of only 1,000 press releases for this short example. To run the code for the entire corpus takes much longer.
```
set.seed(123) # - we set the seed so we can get reproducible results
data <- grimmer_dtm[sample(x = 1:nrow(grimmer_dtm), size = 1000, replace = FALSE), ]
```

3. Run a first original LDA model with 44 topics.
```
lda = topicmodels::LDA(data, 44)
```

You can run the following code to take a look at the 10 most predictive features of each of the 44 topics.
```
library(dplyr) # - install dplyr if you don't have it
original_lda_predictive_features <- sapply(1:nrow(lda@beta), function(i)
  as.character((data.frame(pr = lda@beta[i,], feature = lda@terms) %>% 
  dplyr::arrange(desc(pr)) %>%
  head(n = 10))$feature))
print(original_lda_predictive_features)
```
You should see the following output with the most predictive keywords of each topic.
![alt text](https://github.com/CasAndreu/ldaRobust/blob/master/images/grimmer_example_top_features_01.png)
![alt text](https://github.com/CasAndreu/ldaRobust/blob/master/images/grimmer_example_top_features_02.png)

4. Initialize a _Robust Latent Dirichlet Allocation_ object (`rlda`) that will contain all the information we will generate. You need to specify the following parameters:
  - `dtm` = your Document Term Document matrix.
  - `lda_u`= your original LDA model. We will asses how robust this model is to different model specifications.
  - `K` = the number of models (numeric) or a list of topic numbers (list) of other models to which you want to compare your original LDA. If you provide a numeric value, e.g. 2, we will compare your original model with 10 topics to a 9 and a 11 topic model. If you provide a list, e.g. [5,15,25], we will compare your original model with 10 topics to a 5, 15, and 25 topic model.
  - `compute_parallel` = a bool value to indicate whether you want the models to be trained in parallel.
  
In this example we specify `K = 6`, which means that we'll estimate 6 alternative models with {41,42,43,45,46,47} topics. Please make sure that the ldaRobust library is loaded; otherwise, cannot create new objects.
```
library(ldaRobust)
r <- new("rlda", 
         dtm=data, 
         lda_u=lda, 
         K = 6,
         
         compute_parallel = TRUE)
```

5. Fit the alternative LDA models.
```
r <- ldaRobust::fit(r)
```

6. Compute pairwise topic similarities and build topic clusters based on a chosen similarity threshold.
```
r <- get_cluster_matrix(r, sim_threshold = 0.93)
```

7. Explore whether the resulting topic clusters are present in all models.
```
r <- plot_cluster_proportion(r)
```
![alt text](https://github.com/CasAndreu/ldaRobust/blob/master/images/topic_presence_41_47-STRICT.png)
