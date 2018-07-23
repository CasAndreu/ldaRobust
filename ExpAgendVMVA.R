##this file implements the expressed agenda model from
##Grimmer (2008).

##Suppose that there are D total documents and w words.
##the program requires a D \times w matrix (term-document matrix)



##Further, suppose that there are N total actors whose attention to issues
##you would like to measure.  You should be sure that the documents
##are sorted by their author.  Then you also should include an N \times 2 matrix
##specifying the first document in the term-document matrix that was authored
##by a given author, where the second column specifies the last document
##authored.  place this in the "author" argument

##n.cats sets the number of components (topics) in the mixture model

##this requires MCMCpack.  to install execute
##install.packages('MCMCpack')

exp.agenda.vonmon<- function(term.doc, authors, n.cats, verbose=T, kappa=400){
len<- length
dir.lik<- function(par, pis, prior){
alphas = par[1:ncol(pis)]
alphas<- exp(alphas)
ester<- log(ddirichlet(pis,alpha=alphas))
## ester[which(ester==-Inf)]<- log(1e-323)
priors<- dgamma(alphas, rate=prior, shape=1, log=T)
out<- sum(ester) +sum(priors)
return(out)
}

n.cats<- n.cats
require(MCMCpack)
N<- nrow(authors)

##component labels
taus<- matrix(NA, nrow=nrow(term.doc), ncol=n.cats)

mus<- matrix(NA, nrow=ncol(term.doc), ncol=n.cats)
for(j in 1:ncol(mus)){
mus[,j]<- rgamma(ncol(term.doc), shape=10)
}
for(j in 1:ncol(mus)){
mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
}

##actually alpha in the paper
##will fix in output
thetas<-matrix(NA, nrow=1, ncol=n.cats)
for(j in 1:nrow(thetas)){
thetas[j,]<- 20 + rnorm(n.cats, 0,0.025)
}

pis<- matrix(NA, nrow=N, ncol=n.cats)
for(j in 1:N){
pis[j,]<- thetas + (authors[j,2] - authors[j, 1])/n.cats
}


kappa<- kappa
k<-0
prior<- 1
prior<- c(rep(prior, n.cats))
prior<- prior*-1
Bayes<- 1
v<- 0
z<- 0

while(z==0){
thetas.old<- thetas
pi.gam<- digamma(pis) - digamma(apply(pis, 1, sum))
exp.pi<- exp(pi.gam)

##step one computes the variational
##posterior, which is going to be very close
##the original posterior from the ECM algorithm
##once again, differences in the 'pi.gam' stage

part1<- kappa*term.doc%*%mus


for(j in 1:N){
	for(k in authors[j,1]:authors[j,2]){
		temp<- part1[k,] - max(part1[k,])
		num<- exp.pi[j,]*exp(temp)
		taus[k,]<- num/sum(num)
		}
	}



## M step for the mus
##
mus<- matrix(1/sqrt(ncol(term.doc)), nrow=nrow(mus), ncol=n.cats)
m<- 0


for(j in 1:N){
	for(k in authors[j,1]:authors[j,2]){
		fills<- term.doc[k,]%o%taus[k,]
		mus<- mus + fills
			}
}

for(j in 1:ncol(mus)){
mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
}


##the M part for top level Dirichlet
##is the maximization of alpha parameters

##this is the Newton-Raphson algorithm from
##Blei, Ng, and Jordan (2003)
alpha<- thetas[1,]
##and we know that the log
N1<- N
te<- log(exp.pi)
suff.stats<- apply(te, 2, sum)/N1
k<-0
a<- 0
while(k==0){
sum.alpha<- digamma(sum(alpha))
di.alph<- digamma(alpha)
grads<- Bayes*prior + N1*(sum.alpha - di.alph + suff.stats)
qs<- - N1*trigamma(alpha)
c <- N1*trigamma(sum(alpha))
b1<- sum(grads/qs)
b2<- 1/c + sum(1/qs)
b<- b1/b2
g<- (grads-b)/qs
alpha2<- alpha - g
ester<- which(alpha2<0)
if(len(ester)>0){
alpha2[ester]<- pi + rpois(len(ester), rep(3, len(ester)))
}
alpha<- alpha2
#print(alpha)
g<- max(g)
if(abs(g)<1e-8){
k<-1}
a<- a + 1
if(a>15000){
print('Switching to Optim')
temp.pi<- exp.pi/apply(exp.pi,1, sum)
temp<- optim(log(thetas[1,]), dir.lik, control=list(trace=100, fnscale=-1), method='BFGS',
				 pis=temp.pi, prior=abs(prior))
alpha<- exp(temp$par)
k<- 1
}
}

thetas[1,]<- alpha


sen.list<- list()
for(j in 1:N){
ints<- authors[j,1]:authors[j,2]
sen.list[[j]]<- taus[ints,]
}




for(i in 1:N){
if(is.null(dim(sen.list[[i]]))==T){
	pre.mle<- sen.list[[i]]}
if(is.null(dim(sen.list[[i]]))==F){
pre.mle<- apply(sen.list[[i]], 2, sum)}
theta.ch<- thetas
#for(j in 1:n.coals){
#theta.ch[j,]<- theta.ch[j,] + pre.mle
#theta.ch[j,]<- theta.ch[j,]*coals[i,j]
#}
theta.ch<- theta.ch + pre.mle
pis[i,]<- theta.ch
}


afr<- abs(thetas.old-thetas)
if(max(afr)<1e-5){
z<-1}
cat('\n')
#plot(mus[,1]~mus[,2])
#print(thetas[1,])
#print(max(thetas))
if(verbose==T){
cat(max(afr), '\n')
cat('next it', '\n')}
}

out<- list(pis, mus, taus, thetas)
names(out)<- c('thetas', 'mus', 'rs', 'alpha')
return(out)

}

##Read in Features
#data <- read.delim('EAMbyName2.csv', sep=',', header=T)
#labels <- data[,c(1,2,3)]
#data <- data[,-c(1,2,3)]
#data <- as.matrix(data)
#words <- c(4:ncol(data))

##Read in Authors
#author <- read.delim('EAMnamematrix.csv', sep=',')
#author <- as.matrix(author)

##Run Model

#i <- 40
#number of topics

load("./ldaRobust/grimmer_approx/approx_grimmer.RData")

list_of_matrix = c()
list_of_results = c()

for (i in 39:49){
x <- c()
	#results <- exp.agenda.vonmon(data, author, i, T, 400)
  results <- exp.agenda.vonmon(grimmer, as.matrix(author_l), i, T, 400)
	for (j in 1:i) {
		#temp <- words[order(results$mus[,j] - apply(results$mus[,-j], 1, mean), decreasing=T)[1:20]]
	  temp <- words2[order(results$mus[,j] - apply(results$mus[,-j], 1, mean), decreasing=T)[1:20]]
		x <- rbind(x, temp)
	}
cat6 <- x

cats<- apply(results$rs, 1, which.max)

topics <- c()
	#for (j in 1:nrow(data)) {
	#	topics[j] <- cats[[j]]
#}
for (j in 1:nrow(grimmer)) {
  topics[j] <- cats[[j]]
}

tmatrix <- cbind(labels, topics)
list_of_matrix <- rbind(list_of_matrix, tmatrix)
list_of_results <- rbind(list_of_results, results)
}
save(list_of_matrix, list_of_results, file = "TopicModels.RData")
