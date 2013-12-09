dir.create("data")
download.file( "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", "./data/samsungData.rda", method="curl")
#All of the columns of the data set (except the last two) represents one measurement from the Samsung phone. The variable subject indicates which subject was performing the tasks when the measurements were taken. The variable activity tells what activity they were performing.  

# Your task is to build a function that predicts what activity a subject is performing based on the quantitative measurements from the Samsung phone. For this analysis your training set must include the data from subjects 1, 3, 5, and 6.  But you may use more subjects data to train if you wish. Your test set is the data from subjects 27, 28, 29, and 30, but you may use more data to test. Be careful that your training/test sets do not overlap.  
load("./data/samsungData.rda")

samsungData <- data.frame(samsungData)
samsungData$activity <- as.factor(samsungData$activity)
# number of duplicated columns
table(duplicated(lapply(samsungData, summary)))
# drop duplicated columns
noDupCol <- samsungData[!duplicated(lapply(samsungData, summary))]
# test
table(duplicated(lapply(noDupCol, summary)))

# check missing values
sum(sapply(noDupCol[,names(noDupCol)!='activity'], is.nan))
sum(sapply(noDupCol[,names(noDupCol)!='activity'], is.na))

# check ranges
min(sapply(noDupCol[,names(noDupCol)!='activity'], min))
max(sapply(noDupCol[,names(noDupCol)!='activity'], max))

trainSet <- noDupCol[ noDupCol$subject %in% c(1,3,5,6), 
                      !(names(noDupCol) %in% c('subject'))]
evalSet  <- noDupCol[ noDupCol$subject %in% c(27,28,29,30),
                      !(names(noDupCol) %in% c('subject')) ]
validationSet  <- noDupCol[ noDupCol$subject %in% c(27,28,29,30, 1, 3, 5, 6),
                           !(names(noDupCol) %in% c('subject')) ]

miss.rate <- function(decisionTree, dataSet) {
  class.pred <- table(predict(decisionTree, dataSet, type="class"), dataSet$activity)
  miss <- 1-sum(diag(class.pred))/sum(class.pred)
  r <- list(t=class.pred, miss=miss)
  return (r)
}

library(ggplot2)
library(ggdendro)
library(tree)
fit <- tree(formula=activity ~ . , data= trainSet)
summary(fit)

prunedr <- dendro_data(pruned, type="proportional", comp)
p <- ggplot() +
  geom_segment(data=prunedr$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_text(data=prunedr$labels,aes(x=x, y=y, label=label), size=4, vjust=-0.5) +
  geom_text(data=prunedr$leaf_labels, aes(x=x, y=y, label=label, colour=label), size=3, vjust=2) +
  theme_dendro()

miss.rate(fit, trainSet)
miss.rate(pruned, trainSet)

prunning <- function(decisionTree, dataSet, n) {
  miss=vector()
  for ( i in 2:n ) {
    pruned <- prune.tree(decisionTree, best=i)
    miss = c(miss, miss.rate(pruned, dataSet)$miss)
  }
  plot = ggplot(data=data.frame(a=as.factor(2:n), m=miss),aes(a, m)) + geom_bar(stat='identity')
  return (list(miss=miss, plot=plot))
}

trainPrun <- prunning(fit, trainSet, 12)
evalPrun <- prunning(fit, evalSet, 12)

trainPrun$plot
evalPrun$plot

choosenDecisionTree = prune.tree(fit, best=7)

#visualize
fitChoosen <- dendro_data(choosenDecisionTree, type="proportional")
p <- ggplot() +
  geom_segment(data=fitChoosen$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_text(data=fitChoosen$labels,aes(x=x, y=y, label=label), size=4, vjust=-0.5) +
  geom_text(data=fitChoosen$leaf_labels, aes(x=x, y=y, label=label, colour=label), size=3, vjust=2) +
  theme_dendro()
p
choosenDecisionTree$frame$splits

valRes <- miss.rate(decisionTree==choosenDecisionTree, dataSet=evalSet)
valRes
