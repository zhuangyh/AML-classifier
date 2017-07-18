################################################################################

### Automated diagnosis of Acute Myeloid Leukemia using flow cytometry data
### Author: Yonghua Zhuang
### Insight Health Data Science Fellow
### Boston, 2017 Summer

################################################################################

library(flowCore)
library(flowDensity)
library(dbscan)
library(flowViz)
library(spade)
library(caret)
library(randomForest)
library(e1071)
library(ggcyto)

# set working directory
setwd("~/Downloads/FlowCAP-II/Data/AML")

################################################################################

### Data exploration

################################################################################
## Density plots
file.location <- c("~/Downloads/FlowCAP-II/Data/AML/FCS/0006.FCS",
                   "~/Downloads/FlowCAP-II/Data/AML/FCS/0014.FCS",
                   "~/Downloads/FlowCAP-II/Data/AML/FCS/0022.FCS",
                   "~/Downloads/FlowCAP-II/Data/AML/FCS/0038.FCS",
                   "~/Downloads/FlowCAP-II/Data/AML/FCS/0054.FCS",
                   "~/Downloads/FlowCAP-II/Data/AML/FCS/0070.FCS")
samp <- read.flowSet(file.location)
samp
# log data transformation 
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)
trans <- transformList(c("FL1 Log", "FL2 Log", "FL3 Log", "FL4 Log", "FL5 Log"), lgcl)
samp_log <- transform(samp, trans)

## Stacked density plots
densityplot(~ `FL4 Log`, data=samp_log, smooth=TRUE)

healthy1<-read.FCS("~/Downloads/FlowCAP-II/Data/AML/FCS/0004.FCS",  alter.names=TRUE)
#logTransi <- truncateTransform("truncate at 1", a=1)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)
trans_individual <- transformList(c("FL1.Log", "FL2.Log", "FL3.Log", "FL4.Log", "FL5.Log"), lgcl)
healthy1 <- transform(healthy1, trans_individual)
densityplot(~ `FL4.Log`, data=healthy1 , smooth=TRUE)

## Scatted plots
xyplot(`FL3 Log` ~ `FL4 Log`, data=samp[[1]], smooth=FALSE)
xyplot(`FL3 Log` ~ `FL4 Log`, data=samp[[4]], smooth=FALSE)

#############################################################################

### Feature enginerring

############################################################################

# read general data information
AML <- read.csv("AML.csv")
indir<-"~/Downloads/FlowCAP-II/Data/AML/FCS" 
# Prepare file paths (2872 individual files)
files<-sprintf("%04d", AML$FCSFileName)
info <-AML
info[,1]<-file.path(indir,paste(files,".FCS",sep=""))

# Using HdBscan for unsupervised clustering to extract No. of cell populations
# Due to computation issues, only first 10000 cells were used for hdBscan 
info$cluster <- 0
for (i in (1:length(info[, 1]))){
  # read FCS files and convert to data frame 
  tempdata <- as.data.frame(exprs(read.FCS(info[, 1][i], transformation="scale", alter.names=TRUE)))
  info$cluster[i] <- max(hdbscan(tempdata[1:10000, ], minPts=30)$cluster) 
  }

# Transfer long table to wide table
# Paste Tube number (test) for clusters
library(reshape2)
data1 <- reshape(info_cluster, idvar = "SampleNumber", timevar = "TubeNumber", direction = "wide")
Label <- info[which(info$TubeNumber==1), ]
data1$Label <- Label$Label
write.table(data1, "alldataHdprep1.txt")

# Extract mean, median, standard deviation, skewness, Kurtosis for each markers
library(psych)
features <- NULL
for (i in (1:length(info[, 1]))){
  expr1 <- as.data.frame(exprs(read.FCS(info[, 1][i], transformation="scale", alter.names=TRUE)))
  expr2 <- describe(expr1)[, c(3:5, 11, 12)]
  rownames(expr2) <- NULL
  expr3 <- do.call(cbind, split(expr2, 1:nrow(expr2)))
  expr3$SampleNumber <- info$SampleNumber[i]
  expr3$TubeNumber <- info$TubeNumber[i]
  features <- rbind(features, expr3)
  }
# save features in text
write.table(features, "allfeaturesprep.txt")
# paste Tube number (i.e., test No.) for each markers
features$TubeNumber <- paste("Tube", features$TubeNumber, sep = "")
data2 <- reshape(features, idvar = "SampleNumber", timevar = "TubeNumber", direction = "wide")
Label <- info[which(info$TubeNumber==1), ]
data2$Label <- Label$Label
write.table(data2, "alldataprep2.txt")
# Prepared data include 359 samples and 280 features plus sampleNumber and Label
dim(data2)

# Load features and labels for model development
data2 <- read.table("alldataprep2.txt")
## Subsampling with Smote method 
# library(DMwR)
# set.seed(7890)
# set.seed(201)
# smote_data <- SMOTE(Label ~ ., data  = data2)                         
# table(smote_data$Label) 
# write.table(smote_data, "smote_data.txt")
# set.seed(201)
# Downsampling for healthy controls
set.seed(120)
down_data <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
table(down_data$Class)
write.table(down_data, "down_data.txt")

# Subsample for 5 times 
set.seed(120)
down_data1 <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
set.seed(121)
down_data2 <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
set.seed(122)
down_data3 <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
set.seed(123)
down_data4 <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
set.seed(124)
down_data5 <- downSample(x = data2[, -ncol(data2)],y = data2$Label)
# Write 5 subsampled datasets
write.table(down_data1, "down_data1.txt")
write.table(down_data2, "down_data2.txt")
write.table(down_data3, "down_data3.txt")
write.table(down_data4, "down_data4.txt")
write.table(down_data5, "down_data5.txt")

##################################################################################

### Classification method (LR, SVM, RF) were developed in python with sklearn
### Python code file: AML-classifier.ipynb
### RF model was chosen as final model 
### The following code only focuses on random forest and prepares web app development 

##################################################################################

# Classification method with random Forest
data1.1 <- data1
data1.1$SampleNumber <- NULL

# Split data set into training and testing dataset
trainIndex1 <- createDataPartition(data1.1$Label, p=.4, list=FALSE, times=1)
data1Train <- data1.1[trainIndex1, ]
data1Test  <- data1.1[-trainIndex1,]

set.seed(415)
fit1 <- randomForest(as.factor(Label) ~ .,
                    data=data1Train, 
                    importance=TRUE, 
                    ntree=1000)
fit1
data1Test.pred <- predict(fit1, data1Test)
# Confusion matrix
table(observed = data1Test$Label, predicted = data1Test.pred)

# Classification with sunsampled data (downsized and balanced)
data2.1 <- down_data
names(data2.1) <- make.names(names(data2.1))
data2.1$SampleNumber <- NULL

# Random split to training and testing
trainIndex2 <- createDataPartition(data2.1$Class, p=.6, list=FALSE, times=1)
data2Train <- data2.1[trainIndex2, ]
data2Test  <- data2.1[-trainIndex2,]

# Selected futures from Test D
colnames(data2Train)
subdata2Train <- data2Train[, c(106:140, 281)]
write.table(subdata2Train, "subdata2Train.txt")

# Random forest model development
set.seed(415)
fit2 <- randomForest(as.factor(Class) ~ .,
                     data=data2Train, 
                     importance=TRUE, 
                     ntree=1000)
fit2
data2Test.pred <- predict(fit2, data2Test)

# confuston matrix
table(observed = data2Test$Class, predicted = data2Test.pred)















