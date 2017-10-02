## Paula P Sobrino
## June 11, 2017
## Chisquare-Test

## Load in data:
setwd("~/Desktop/madness")
corpus=read.csv("corpus_adverts.csv")

## Check
head(corpus)
tail(corpus)
nrow(corpus)

## 1st test. Tabulate mode source and mode target:

corpus.table <- table(corpus$mode.source, corpus$mode.target)

## Display margins:

addmargins(corpus.table)

## Proportion of total:

corpus.table / sum(corpus.table)

## Save this:

myprops <- corpus.table / sum(corpus.table)

## Round numbers:

round(myprops, 2)

## Get row and column proportions:

prop.table(dist.table, margin = 1)
prop.table(dist.table, margin = 2)

## Two other useful functions:

rowSums(corpus.table)
colSums(corpus.table)

#Perform a Fisher Exact test:
chisq.test(corpus.table, simulate.p.value=TRUE)
fisher.test(corpus.table, simulate.p.value=TRUE)

## Save the result:

corpus.fisher <- fisher.test(corpus.table, simulate.p.value=TRUE)

## Inspect the object:
corpus.fisher
str(corpus.fisher)

## Get expected counts:

corpus.fisher$expected

##Get standardized residuals:

corpus.fisher$stdres



