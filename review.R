setwd("C:/Users/ksiro/Documents/GitHub/Text classification/Text-analysis")
df <- read.csv("review.csv",sep=",",header=T)
colnames(df)<- c("review","condition")
condition <- df$condition
review <- df$review
corpus <- VCorpus(VectorSource(review))
var <- as.matrix(TermDocumentMatrix(corpus))
var <- t(var)
var <- as.data.frame(var)
var <- cbind(var,condition)
var <- aggregate(var[,1:ncol(var)-1],by=list(var$condition),sum)
colnames(var)[1] <- "condition"

d <- "was so happy that I fell in love with the movie"
d <- strsplit(d," ")
d <- unlist(d)

negative <- var[which(var$condition=="N"),]
positive <- var[which(var$condition=="P"),]

n_sum <-  rowSums(negative[,2:ncol(negative)])
p_sum <-  rowSums(positive[,2:ncol(positive)])

prob_n <-  n_sum/(n_sum + p_sum)
prob_p <- (p_sum)/(p_sum + n_sum)

 naive_neg <- function(x){ (x+1)/(prob_n + ncol(negative)-1 )
  }
negative_prob<- apply(negative[,2:ncol(negative)], 2, naive_neg)
negative_prob<- data.frame(negative_prob)
negative_prob <- t(negative_prob)

naive_pos <- function(x){ (x+1)/(prob_n + ncol(positive)-1)
}
positive_prob<- apply(positive[,2:ncol(positive)], 2, naive_pos)
positive_prob <- as.data.frame(positive_prob)
positive_prob <- t(positive_prob)

values <- match(d,colnames(negative_prob))
values <- values[!is.na(values)]

 probs_neg <- negative_prob[,values]
 probs_pos <- positive_prob[,values]

##probs <- rbind(positive_prob,negativ_prob)
##probs <- round(bar,2)

##values <- match(d,colnames(var[,2:ncol(var)]))
##values <- values[!is.na(values)]
  
#probs <- probs[,values]


positive_rev <- prob_p * prod(probs_pos) *100
negative_rev <- prob_n * prod(probs_neg) * 100

positive_rev
negative_rev
a <- positive_rev > negative_rev
ifelse(a, print("positive"),print("negative"))
