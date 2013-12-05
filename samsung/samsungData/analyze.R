dir.create("data")
download.file( "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", "./data/samsungData.rda", method="curl")
#All of the columns of the data set (except the last two) represents one measurement from the Samsung phone. The variable subject indicates which subject was performing the tasks when the measurements were taken. The variable activity tells what activity they were performing.  

# Your task is to build a function that predicts what activity a subject is performing based on the quantitative measurements from the Samsung phone. For this analysis your training set must include the data from subjects 1, 3, 5, and 6.  But you may use more subjects data to train if you wish. Your test set is the data from subjects 27, 28, 29, and 30, but you may use more data to test. Be careful that your training/test sets do not overlap.  
load("./data/samsungData.rda")

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

library(ggplot2)
library(ggdendro)
library(rpart)
fit <- rpart( activity ~ ., method="class", data=trainSet)
fitr <- dendro_data(fit)

p <- ggplot() +
   geom_segment(data=fitr$segments, aes(x=x, y=y, xend=xend, yend=yend)) +
   geom_text(data=fitr$labels,aes(x=x, y=y, label=label), size=3, vjust=0) +
   geom_text(data=fitr$leaf_labels, aes(x=x, y=y, label=label), size=3, vjust=1) +
   theme_dendro()
