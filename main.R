
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)

install.packages('topicmodels')
library(topicmodels)

#h5ls("~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/data/A/A/A/TRAAABD128F429CF47.h5")
#sound<-h5read("~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/data/A/A/A/TRAAABD128F429CF47.h5", "/analysis")

common_id<-read.table("~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/common_id.txt",header=FALSE)
#x<-as.character(dir[1,1])
#substring(x,2,2)

d<-list()
d<-as.list(rep(NA,dim(common_id)[1]))
#names(d)<-as.vector(common_id[,1])

d.bars = vector()
d.beats = vector()
d.sec = vector()
d.seg = vector()
d.tat = vector()
for(i in 1:dim(common_id)[1]){
  x<-as.character(common_id[i,1])
  
  A<-substring(x,3,3)
  B<-substring(x,4,4)
  C<-substring(x,5,5)
  
  dir<-paste('~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/data/',A,'/',B,'/',C,'/',x,'.h5',sep = "")
  sound<-h5read(dir, "/analysis")
  d[[i]]<-sound
  
  
  d.bars[i] <- length(sound$bars_confidence)
  d.beats[i] <- length(sound$beats_confidence)
  d.sec[i] <- length(sound$sections_confidence)
  d.seg[i] <- length(sound$segments_confidence)
  d.tat[i] <- length(sound$tatums_confidence)
}

# to specify the terget dimensions of features
bars_dim <- floor(median(d.bars)) #120
beats_dim <- floor(median(d.beats)) #446
sec_dim <- floor(median(d.sec)) #9
seg_dim <- floor(median(d.seg)) #744
tat_dim <- floor(median(d.tat)) #983

############## Pre processing of lyric data ####
load('~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/lyr.RData')
c<- c(2,3,6:30)
lyr<-lyr[,-c]
lyr<-lyr[,colSums(lyr[,-1])>0] #cols which never appears



library("slam")
#summary(col_sums(lyr[,-1]))

## calculate term frequency-inverse document frequency (tf-idf)
# Although for final ranks all words are included,
# For topic modelling to better cluster the topics, 
# only words that are meaningful(not the too common word or not error) are considered
tf <- lyr[,-1]
idf <- log(nrow(lyr[,-1])/colSums(lyr[,-1]))
tfidf <- lyr[,-1]

for(word in names(idf)){
  tfidf[,word] <- tf[,word] * idf[word]
}
tfidf<-tfidf[ , colSums(is.na(tfidf)) == 0]
Coltfdif<-colMeans(tfidf)
#summary(Coltfdif)  
#We notice that the median is around 0.03226

Newlyr<-lyr[, Coltfdif>=0.03226]
error_row<-which(rowSums(Newlyr)==0)
Newlyr<-Newlyr[rowSums(Newlyr)>0,]  #the row 756 is empty

########### topic modelling ##########
VEM<-LDA(Newlyr, 10, method = "VEM")
Topic <- topics(VEM, 1)
#terms(VEM, 100)


########### calculate rank for the 10 topics #############
#based on the original set of lyrics calculate the word rank for each topic

load('~/Documents/Fall2016-proj4-LlilyJiang/data/Project4_data/lyr.RData')
c<- c(1:30)
Ranklyr<-lyr[,-c]
Ranklyr<-Ranklyr[-error_row,]


RankTopic<-cbind(Topic,Ranklyr)

WordCounts<-data.frame(matrix(ncol = 4791, nrow = 10))
WordRanks<-data.frame(matrix(ncol = 4791, nrow = 10))
for(j in 1:10){
  Temp<-RankTopic[Topic==j,]
  Temp<-Temp[,-1]
  WordCounts[j,]<-as.data.frame(t(colSums(Temp)))
  WordRanks[j,]<-4791-rank(WordCounts[j,],ties.method='first')
}




######### song features ##############
source("./lib/featuresprocessing.r")



#### feature processing to combine features for different songs in one matrix


train_data <- data.frame(matrix(ncol = 22151, nrow = 0))


for(i in 1:dim(common_id)[1]){
  data<-d[[i]]
  bars_s <- feature_truncate_1d(data$bars_start, 120)
  beats_s <- feature_truncate_1d(data$beats_start, 446)
  sections_s <- feature_truncate_1d(data$sections_start, 9)
  segments_s <- feature_truncate_1d(data$segments_start, 744)
  segments_l_m <- feature_truncate_1d(data$segments_loudness_max, 744)
  segments_l_m_t <- feature_truncate_1d(data$segments_loudness_max_time, 744)
  segments_l_s <- feature_truncate_1d(data$segments_loudness_start, 744)
  segments_p <- feature_truncate_2d(data$segments_pitches, 744)
  segments_t <- feature_truncate_2d(data$segments_timbre, 744)
  tatums_s <- feature_truncate_1d(data$tatums_start, 744)
  feature_row <- c(bars_s, beats_s, sections_s, segments_s, segments_l_m, segments_l_m_t,
                    segments_l_s, segments_p, segments_t, tatums_s)
  #train_id[i,1] <- common_id[i,1]
  train_data[i,] <- feature_row
}
train_id <- common_id

### PCA to increase processing speed

##Remove Invalid Rows
#to be consistent with the lyr data,delete rows with no lyr data first!!!
train_data<-train_data[-error_row,]
train_id<-as.data.frame(train_id[-error_row,])

#remove rows(songs) that have NA features
Invalid_Row<-which(is.na(rowSums(train_data)))
train_data<-train_data[-Invalid_Row,]
train_id<-as.data.frame(train_id[-Invalid_Row,]) #finally totally 2340 rows

##Remove Invalid Cols (to enable using PCA)
Invalid_Column<-cbind()
for(i in 1:ncol(train_data)){
  train_samecol<-sum(train_data[,i]==train_data[1,i])
  if(train_samecol==nrow(train_data)){
    #print(i)
    Invalid_Column<-cbind(Invalid_Column,i)
  }
}
#the output col is 567,576
train_data <- train_data[,-Invalid_Column]

train_data_SET <- cbind(train_id, train_data)
#write.csv(train_data_SET, file = paste(data_output_path, "/train_raw.csv", sep=""))


##PCA
pca <- prcomp(train_data, center=TRUE, scale=TRUE)
#cumdev <- cumsum(pca$sdev) / sum(pca$sdev)
#cumdev_keep <- cumdev[cumdev <= 0.9]
#n <- sum(cumdev_keep) + 1
#pca_matrix <- pca$rotation[,1:n]
pca_matrix <- pca$rotation
save(pca_matrix, file = "./output/pca_loading.rda")
train_data_pca <- as.matrix(train_data) %*% pca_matrix
train_data_pca_SET <- cbind(train_id, train_data_pca)
#write.csv(train_data_pca, file = paste(data_output_path, "/train.csv", sep=""))



########### multi class classification on train data after PCA ##########


NewTopic<-Topic[-Invalid_Row]
train.label<-NewTopic
train.label<-as.data.frame(factor(train.label))

set.seed(10)
t<-sample(1:2340,2000)
train.feature<-as.data.frame(train_data_pca)

trainX<-train.feature[t,]
trainY<-train.label[t,]
testX<-train.feature[-t,]
testY<-train.label[-t,]

###Random Forest
library(randomForest)
fit_rf<-randomForest(trainX,trainY,importance=T,proximity=T)
#pred<-predict(fit_rf,testX,type="vote",norm.votes=T)
pred_rf<-predict(fit_rf,testX)
mean(pred_rf != testY)

###KNN
library(caret)
fit_knn<-knn3(trainX,trainY,k=10)
pred_knn<-predict(fit_knn,testX,type="class")
mean(pred_knn != testY)

fit_knn2<-knn3(StrainX,trainY,k=10)
pred_knn2<-predict(fit_knn2,StestX,type="class")
mean(pred_knn2 != testY)

###multinomial logistic regression
scale.train<-scale(train.feature)
StrainX<-as.data.frame(scale.train[t,])
StestX<-as.data.frame(scale.train[-t,])

library(nnet)

#fit_logistic<-multinom(trainY~.,trainX)
fit_logistic<-multinom(trainY~.,trainX,MaxNWts=23500)

pred_logit<-predict(fit_logistic,testX,type="class")
mean(pred_logit != testY)

fit_logistic2<-multinom(trainY~.,StrainX,MaxNWts=23500)
pred_logit2<-predict(fit_logistic2,StestX,type="class") #for normalized data
mean(pred_logit2 != testY)



############## Process Test Data ###############
dTest<-list()
dTest<-as.list(rep(NA,100))
song_id<-seq(1,100)
dTest<-
for(i in 1:100){
  dir<-paste('~/Documents/Fall2016-proj4-LlilyJiang/data/TestSongFile100/testsong',i,'.h5',sep = "")
  sound<-h5read(dir, "/analysis")
  dTest[[i]]<-sound
}

###EXTRACT TEST SONG FEATURES
Test_data <- data.frame(matrix(ncol = 22151, nrow = 0))

for(i in 1:100){
  data<-dTest[[i]]
  bars_s <- feature_truncate_1d(data$bars_start, 120)
  beats_s <- feature_truncate_1d(data$beats_start, 446)
  sections_s <- feature_truncate_1d(data$sections_start, 9)
  segments_s <- feature_truncate_1d(data$segments_start, 744)
  segments_l_m <- feature_truncate_1d(data$segments_loudness_max, 744)
  segments_l_m_t <- feature_truncate_1d(data$segments_loudness_max_time, 744)
  segments_l_s <- feature_truncate_1d(data$segments_loudness_start, 744)
  segments_p <- feature_truncate_2d(data$segments_pitches, 744)
  segments_t <- feature_truncate_2d(data$segments_timbre, 744)
  tatums_s <- feature_truncate_1d(data$tatums_start, 744)
  feature_row <- c(bars_s, beats_s, sections_s, segments_s, segments_l_m, segments_l_m_t,
                   segments_l_s, segments_p, segments_t, tatums_s)
  #train_id[i,1] <- common_id[i,1]
  Test_data[i,] <- feature_row
}

Test_data <- Test_data[,-Invalid_Column] #keep the consistency with train data

####### PCA and predict based on trained models
TEST_Features<-as.matrix(Test_data) %*% pca_matrix
fit_logistic<-multinom(train.label~.,train.feature,MaxNWts=23500)
pred_TEST<-predict(fit_logistic,TEST_Features)


TEST_Rank<-data.frame(matrix(ncol = 5001, nrow = 100))
names(TEST_Rank)<-names(lyr)
for(i in 1:100){
  TEST_Rank[i,1]<-i
}
for(i in 1:100){
  for(j in 2:30){
    TEST_Rank[i,j]<-4971
  }
}

for(i in 1:100){
  r<-pred_TEST[i]
  TEST_Rank[i,31:5001]<-WordRanks[r,]
}

write.csv(TEST_Rank,'~/Documents/Fall2016-proj4-LlilyJiang/output/Rank.csv')
