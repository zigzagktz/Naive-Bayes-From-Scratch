
condition <- df$Condition
corpus <- VCorpus(VectorSource(df$Document[1:8]))
var <- as.matrix(TermDocumentMatrix(corpus))
var <- t(var)
var <- as.data.frame(var)
var <- cbind(var,condition)
var <- aggregate(var[,1:25],by=list(var$condition),sum)
colnames(var)[1] <- "condition"


counts <- var[,2:25]
s <- rowSums (counts, na.rm = FALSE, dims = 1)

p_n <-  s[1]/(s[1]+s[2])
p_p <- s[2]/(s[1]+s[2])

 naive_neg <- function(x){ x/s[1]
  }
negativ_prob<- apply(counts[1,], 2, naive_neg)

naive_pos <- function(x){ x/s[2]
}
positive_prob<- apply(counts[2,], 2, naive_pos)

probs <- rbind(positive_prob,negativ_prob)
probs <- round(bar,2)

values <- match(d,colnames(var))
values <- values[!is.na(values)]

d <- "was great better"
d <- strsplit(d," ")
d <- unlist(d)
values <- match(d,colnames(counts))
values <- values[!is.na(values)]

probs <- probs[,values]


positive_rev <- p_p * prod(probs[1,]) *100
negative_rev <- p_n * prod(probs[2,]) * 100
positive_rev
negative_rev

p(a/b) = p(b) * p(b1/a) * p(b3/a) * p(b3/a)