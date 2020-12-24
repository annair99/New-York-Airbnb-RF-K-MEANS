#######CLEAN#####
library(data.table)
library(mltools)
library(standardize)
library("dplyr")
library(factoextra)
#############Q0#################
AB_NYC_2019 <- read.csv("C:/Users/User/OneDrive/Desktop/MSDS/DS/archive/AB_NYC_2019.csv")
NYC_DATA_CLEAN= AB_NYC_2019[,-c(1:4,6,13)]
NYC_DATA_CLEAN$neighbourhood_group<-as.factor(NYC_DATA_CLEAN$neighbourhood_group)
NYC_DATA_CLEAN= na.omit(NYC_DATA_CLEAN)##No NA values
View(NYC_DATA_CLEAN)
#adding the target variable
NYC_TARGET<-NYC_DATA_CLEAN
NYC_TARGET$target = NA

#Creating the target class
for (i in 1:nrow(NYC_TARGET)){
  if(NYC_TARGET$price[i]<80){
    NYC_TARGET$target[i]="Low"
  }else if(NYC_TARGET$price[i]<150){
    NYC_TARGET$target[i]="Medium"
  }else{
    NYC_TARGET$target[i]="High"
  }
}
NYC_TARGET$target=as.factor(NYC_TARGET$target)
summary(NYC_TARGET$target)
#one hot encoding categorical
S_NYC<-one_hot(as.data.table(NYC_TARGET), cols = c("room_type","neighbourhood_group"))
S_NYC = data.frame(S_NYC)
##Standardizing
S_NYC1<-S_NYC%>%mutate_if(is.numeric,scale)
S_NYC1<-S_NYC1[,-11]#get rid of original price var
S_NYC_features = S_NYC1[,-16]
S_NYC_label = data.frame(S_NYC1[,16])

#############Q1#############
#PCA
AIRBNB.pca <- prcomp(S_NYC_features,scale=TRUE) # pca of features
PC = data.frame(AIRBNB.pca$x)
fviz_eig(AIRBNB.pca)#plot eigenvalues

fviz_pca_var(AIRBNB.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
summary(AIRBNB.pca) #11 PCS explain 95% variation
########Q1.1############
percent_var = summary(AIRBNB.pca)
fviz_eig(AIRBNB.pca,geom = "line",choice = "eigenvalue",ncp = 15)
fviz_eig(AIRBNB.pca,geom = "line",choice = "variance",ncp = 15)

Prop_Var = data.frame(percent_var$importance[3,])
Prop_Var = round(100*Prop_Var,2)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
plot(seq(1,15),Prop_Var$percent_var.importance.3...)

ggplot(Prop_Var, aes(x=seq(1,15), y=percent_var.importance.3...)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Cumulative % Variance explained by PC")+
  xlab("Dimension")+
  ylab("% cumulative variance explained")
###########Q1.2############
PC = cbind(PC,S_NYC_label)
PC = PC[,c(1,2,3,16)]
#LOW,MEDIUM
PC_LM = subset(PC,PC$S_NYC1...16. == "Low" | PC$S_NYC1...16. == "Medium")
#MEDIUM HIGH
PC_MH = subset(PC,PC$S_NYC1...16. == "High" | PC$S_NYC1...16. == "Medium")
#LOW HIGH
PC_LH = subset(PC,PC$S_NYC1...16. == "Low" | PC$S_NYC1...16. == "High")
##plotting Low versus Medium
#displaying 3D graph of each vector
library(factoextra)
library(plot3D)
library(rgl)
library(scatterplot3d)
colors <- c("#1B9E77", "#D95F02", "yellow") # creating color function
colors <- colors[as.numeric(PC_LM$S_NYC1...16.)] # adding the colors numbers to each CL
#side plot
scat2<-scatterplot3d(PC_LM[,1:3], pch = 16, color=colors, main = "3")
legend(scat2$xyz.convert(-30,1,15),legend = levels(PC_LM$S_NYC1...16.),col =c("#1B9E77", "#D95F02", "yellow"),pch=16 )
#3D plot
plot3d(PC_LM[,1:3], col=colors, main="Low versus medium")

##Medium versus High

colors <- c("#1B9E77", "#D95F02", "yellow") # creating color function
colors <- colors[as.numeric(PC_MH$S_NYC1...16.)] # adding the colors numbers to each CL

#side plot
scat2<-scatterplot3d(PC_MH[,1:3], pch = 16, color=colors, main = "3")
legend(scat2$xyz.convert(-25,1,15),legend = levels(PC_LM$S_NYC1...16.),col =c("#1B9E77", "#D95F02", "yellow"),pch=16 )
#3D plot
plot3d(PC_MH[,1:3], col=colors, main="Medium versus High")
#Low versus high
colors <- c("#1B9E77", "#D95F02","yellow") # creating color function
colors <- colors[as.numeric(PC_LH$S_NYC1...16.)] # adding the colors numbers to each CL
#side plot
scat2<-scatterplot3d(PC_LH[,1:3], pch = 16, color=colors, main = "3")
legend(scat2$xyz.convert(-25,1,15),legend = levels(PC_LH$S_NYC1...16.),col =c("#1B9E77", "#D95F02", "#7570B3"),pch=16 )
#3D plot
plot3d(PC_LH[,1:3], col=colors, main="Low versus High")
###########Q2###########
#apply K means clustering
start = Sys.time()
k.set= c(1,5,10,15,20,25,30,40,50,60,70,80,90,100)
Qm <- numeric(length(k.set))
Perf.m <- numeric(length(k.set))
wss = numeric(length(k.set))
for (k in 1:length(k.set)){
  km.airbnb <- kmeans(S_NYC_features, k.set[k], nstart = 100,iter.max=50)
  #wss[k] = km.airbnb$tot.withinss
  Q <- km.airbnb$tot.withinss/km.airbnb$totss
  Perf <- 1-(km.airbnb$tot.withinss/km.airbnb$totss)
  Qm[k] = Q
  Perf.m[k] = Perf
}
end = Sys.time()
end-start
ggplot(data.frame(cbind(k.set,Perf.m)),aes(x=k.set, y=Perf.m)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 15)+
  xlab("k")+
  ylab("Perf(k)")+
  ggtitle("k vs Perf(k)")+
  theme_bw()
##################Q2.1 ##############
k.set= c(1,5,10,15,20,25,30,40,50,60,70,80,90,100)
nstart=30
impk = numeric(length(k.set))
gini_fin = numeric(length(k.set))
gin = numeric(length(k.set))
full_data= S_NYC1
##cluster #8 is purest
for(k in 1:length(k.set)){
  full_data = S_NYC1
  km_explore<- kmeans(full_data[,-16],k.set[k], iter.max = 50)
  full_data$cluster = NA
  full_data$cluster=as.factor(km_explore$cluster)
  clus_Size = km_explore$size #5 clusters when k=5
  gini_cl = numeric(k.set[k])
  impk = 0
  for(i in 1:k.set[k]){#go through each cluster
    clu<-subset(full_data,full_data$cluster == i)
    clu_1 = table(clu$target)[1]/clus_Size[i]
    clu_2=table(clu$target)[2]/clus_Size[i]
    clu_3=table(clu$target)[3]/clus_Size[i]
    gini_cl = clu_1*(1-clu_1)+clu_2*(1-clu_2)+clu_3*(1-clu_3)
    impk = impk+gini_cl
    
  }
  gin[k] = impk
  #print(gini_cl)
}
gini_final = numeric(length(k.set))
for(i in 1:length(k.set)){
  gini_final[i] = gin[i]/(.6667*k.set[i])
}
gini_plot=data.frame(gini_final)
ggplot(gini_plot , aes(x=k.set, y=gini_plot$gini_final)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab("k")+
  ylab("gini k")+
  ggtitle("k vs gini index")
###################Q3###########
##k=15
set.seed(1)
kmbest<- kmeans(S_NYC_features,15,nstart = 100,iter.max=100)
kmbest.centers = kmbest$centers
write.table(kmbest.centers, file = "K Means Centriod values.csv", sep = ",", row.names = TRUE)
kmbest$withinss
kmbest$tot.withinss
kmbest$size

full_data= S_NYC1
full_data$cluster = NA
full_data$cluster=as.factor(kmbest$cluster)
gini_cl = numeric(15)
clu_11 = numeric(15)
clu_22= numeric(15)
clu_33 = numeric(15)
clus_Siz = numeric(15)
wss = numeric(15)
clus_Size = kmbest$size
for(i in 1:15){
  clus_Siz[i]= kmbest$size#go through each cluster
  clu<-subset(full_data,full_data$cluster == i)
  clu_1 = table(clu$target)[1]/clus_Size[i]
  clu_11[i] =  clu_1
  clu_2=table(clu$target)[2]/clus_Size[i]
  clu_22[i] =  clu_2
  clu_3=table(clu$target)[3]/clus_Size[i]
  clu_33[i] =  clu_3
  gini_cl[i] = clu_1*(1-clu_1)+clu_2*(1-clu_2)+clu_3*(1-clu_3)
}


wss=kmbest$withinss
desc = data.frame(cbind(wss,clus_Size,gini_cl,clu_11,clu_22,clu_33))
###3D projection3######
kmbest_pc = PC
kmbest_pc$cluster =as.factor(kmbest$cluster)
kmbest_pc = data.frame(kmbest_pc)
colors <- c("red", "blue", "yellow","purple","green","orange","pink","black","azure","coral","gold","gray","burlywood","aquamarine","beige") # creating color function
colors <- colors[as.numeric(kmbest_pc$cluster)] # adding the colors numbers to each CL

#side plot
scat2<-scatterplot3d(kmbest_pc[,1:3], pch = 16, color=colors, main = "3")
legend(scat2$xyz.convert(-25,1,15),legend = levels(PC_LM$S_NYC1...16.),col =c("#1B9E77", "#D95F02", "#7570B3"),pch=16 )
#3D plot
plot3d(kmbest_pc[,1:3], col=colors, main="K=15")

kmbest_center = prcomp(kmbest.centers,scale=TRUE) # pca of features
kmbest_center=kmbest_center$x[,1:3]
kmbest_center = data.frame(kmbest_center) #15x3 cluster matrix

colors <- c("red", "blue", "yellow","purple","green","orange","pink","black","azure","coral","gold","gray","burlywood","aquamarine","beige") # creating color function
#colors <- colors[as.numeric(kmbest_pc$cluster)] 

x <- kmbest_center$PC1
y <- kmbest_center$PC2
z <- kmbest_center$PC3

plot3d(kmbest_center, col=colors,pch = 50, main="K=15 cluster centers")
scat<-scatterplot3d(kmbest_center, pch = 16,color = colors,  main = "k=15 cluster centers")
########ACCURACY OF K=15###########
S_NYC_15 = S_NYC1
S_NYC_15$cluster<-as.factor(kmbest$cluster)
#cluster 1
clu1<-subset(S_NYC_15,S_NYC_15$cluster == "1")#LOW
table(clu1$target) # High:40 , Medium: 95, Low:179 
#cluster 2
clu2<-subset(S_NYC_15,S_NYC_15$cluster == "2")#Low
table(clu2$target)#Low:574, Medium:236, High:65, 
#cluster 3
clu3<-subset(S_NYC_15,S_NYC_15$cluster == "3")# High
table(clu3$target)#Medium:1906, low:170, High:4162
#cl4
clu4<-subset(S_NYC_15,S_NYC_15$cluster == "4")#Medium
table(clu4$target)#Medium:892, low:249, High:565 , 
#cluster 5
clu5<-subset(S_NYC_15,S_NYC_15$cluster == "5")#High
table(clu5$target) # High:233 , Medium: 2, Low:0
#cluster 6
clu6<-subset(S_NYC_15,S_NYC_15$cluster == "6")#LOW
table(clu6$target) # High:33 , Medium: 101, Low:663
#cluster 7: 
clu7<-subset(S_NYC_15,S_NYC_15$cluster == "7")#Low
table(clu7$target) # High:169 , Medium: 1185, Low:3908
#cluster 8
clu8<-subset(S_NYC_15,S_NYC_15$cluster == "8")#Medium
table(clu8$target) # High:766 , Medium: 2747, Low:2271
#cluster 9
clu9<-subset(S_NYC_15,S_NYC_15$cluster == "9")#High
table(clu9$target) # High:2380 , Medium: 739, Low:27


#cluster 10
clu10<-subset(S_NYC_15,S_NYC_15$cluster == "10")#Medium
table(clu10$target) # High:379 , Medium: 651, Low:429

#cluster 11
clu11<-subset(S_NYC_15,S_NYC_15$cluster == "11")#High
table(clu11$target) # High:1491 , Medium: 1341, Low:158

#cluster 12
clu12<-subset(S_NYC_15,S_NYC_15$cluster == "12")#Medium
table(clu12$target) # High: 2257 , Medium: 2303 , Low:394

#cluster 13
clu13<-subset(S_NYC_15,S_NYC_15$cluster == "13")#Medium
table(clu13$target) # High: 21 , Medium: 29 ,Low:14

#cluster 14
clu14<-subset(S_NYC_15,S_NYC_15$cluster == "14")#Low
table(clu14$target) # High: 96 , Medium:636 ,Low:1694

#cluster 15
clu15<-subset(S_NYC_15,S_NYC_15$cluster == "15")#Low
table(clu15$target) # High: 63 , Medium:456 ,Low:2074

S_NYC_15$predclass = NA

table(S_NYC_15$cluster)

S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="1","Low",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="2","Low",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="3","High",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="4","Medium",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="5","High",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="6","Low",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="7","Low",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="8","Medium",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="9","High",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="10","Medium",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="11","High",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="12","Medium",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="13","Medium",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="14","Low",S_NYC_15$predclass)
S_NYC_15$predclass<-ifelse(S_NYC_15$cluster =="15","Low",S_NYC_15$predclass)

table(S_NYC_15$predclass,S_NYC_15$target) # rows are predicted , columns are true values
library("caret")
S_NYC_15$predclass = as.factor(S_NYC_15$predclass)
S_NYC_15$target =  as.factor(S_NYC_15$target)
confusionMatrix(S_NYC_15$predclass,S_NYC_15$target)# 75% accurare cl1
mean(S_NYC_15$predclass == S_NYC_15$target) #43%

#######gini index of best k#####
full_data= S_NYC1
full_data$cluster = NA
full_data$cluster=as.factor(kmbest$cluster)
gini_cl = numeric(15)
clu_11 = numeric(15)
clu_22= numeric(15)
clu_33 = numeric(15)
clus_Siz = numeric(15)
clus_Size = kmbest$size
for(i in 1:15){
  clu<-subset(full_data,full_data$cluster == i)
  clu_1 = table(clu$target)[1]/clus_Size[i]
  clu_11[i] =  clu_1
  clu_2=table(clu$target)[2]/clus_Size[i]
  clu_22[i] =  clu_2
  clu_3=table(clu$target)[3]/clus_Size[i]
  clu_33[i] =  clu_3
  gini_cl[i] = clu_1*(1-clu_1)+clu_2*(1-clu_2)+clu_3*(1-clu_3)
}


###########Q4###########
SDATA = S_NYC1
SDATA$cluster = as.factor(kmbest$cluster)
set.seed(1)
LOWPRICE =SDATA[which(S_NYC1$target =="Low"),]
set.seed(1)
MEDPRICE = SDATA[which(S_NYC1$target =="Medium"),]
set.seed(1)
HIGHPRICE = SDATA[which(S_NYC1$target =="High"),]


##TRAIN/TEST SET: LOW
n<-nrow(LOWPRICE)
train<-sample(1:n, 0.8*n)
trainset_LOW <- LOWPRICE[train,]
testset_LOW <- LOWPRICE[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(MEDPRICE)
train<-sample(1:n, 0.8*n)
trainset_MED <- MEDPRICE[train,]
testset_MED <- MEDPRICE[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(HIGHPRICE)
train<-sample(1:n, 0.8*n)
trainset_HIGH <- HIGHPRICE[train,]
testset_HIGH <- HIGHPRICE[-train,]
#combined train set
TRAIN_SET = rbind(trainset_LOW,trainset_MED,trainset_HIGH)#High:10176  Low: 10243  Medium:10655 
TEST_SET = rbind(testset_LOW,testset_MED,testset_HIGH)#High: 2544   Low:2561   Medium:2664

TRAIN_SET1 = TRAIN_SET[,-17]
TEST_SET1 = TEST_SET[,-17]

##########same test split for cluster data#######



########################### Question5################################
library(randomForest)
library(caret)
TRAIN_SET1$target<- factor(TRAIN_SET1$target) # changing label to factors
TEST_SET1$target<- factor(TEST_SET1$target)
set.seed(1)
start = Sys.time() ####1.706 mins
RF_100 <- randomForest(target ~., data=TRAIN_SET1,ntree = 100, ntry=4
                        ,importance=T)
RF_200 <- randomForest(target ~., data=TRAIN_SET1,ntree = 200, ntry=4
                       ,importance=T)
RF_300 <- randomForest(target ~., data=TRAIN_SET1,ntree = 300, ntry=4
                       ,importance=T)
RF_400 <- randomForest(target ~., data=TRAIN_SET1,ntree = 400, ntry=4
                       ,importance=T)
end=Sys.time()
Time=end-start 
predSET_100tr<-predict(RF_100,newdata = TRAIN_SET1)1
predSET_100test<-predict(RF_100,newdata = TEST_SET1)
results_100tr = confusionMatrix(predSET_100tr, TRAIN_SET1$target)
results_100test = confusionMatrix(predSET_100test, TEST_SET1$target)

predSET_200tr<-predict(RF_200,newdata = TRAIN_SET1)
results_200tr = confusionMatrix(predSET_200tr, TRAIN_SET1$target)
predSET_200test<-predict(RF_200,newdata = TEST_SET1)
results_200test = confusionMatrix(predSET_200test, TEST_SET1$target)

predSET_300tr<-predict(RF_300,newdata = TRAIN_SET1)
results_300tr = confusionMatrix(predSET_300tr, TRAIN_SET1$target)
predSET_300test<-predict(RF_300,newdata = TEST_SET1)
results_300test = confusionMatrix(predSET_300test, TEST_SET1$target)

predSET_400tr<-predict(RF_400,newdata = TRAIN_SET1)
results_400tr = confusionMatrix(predSET_400tr, TRAIN_SET1$target)
predSET_400test<-predict(RF_400,newdata = TEST_SET1)
results_400test = confusionMatrix(predSET_400test, TEST_SET1$target)

Train_Acc = c(results_100tr$overall[1],results_200tr$overall[1],results_300tr$overall[1],results_400tr$overall[1])
Test_Acc = c(results_100test$overall[1],results_200test$overall[1],results_300test$overall[1],results_400test$overall[1])
nfor = c(100,200,300,400)

ggplot(data.frame(cbind(ntree,Train_Acc,Test_Acc)), aes(x=ntree))+
  geom_point(aes(y = Train_Acc, color = "darkred"),shape=18,size=3)+
  geom_line(aes(y = Train_Acc,color = "darkred"),linetype="longdash") +
  geom_point(aes(y = Test_Acc,color = "steelblue"),shape=18,size=3)+
  geom_line(aes(y = Test_Acc,color="steelblue"), linetype="dotdash")+
  ylim(.63,.85)+
  scale_color_identity(name = "",
                       labels = c("Train-set", "Test-set"),
                       guide = "legend") +
  xlab("Number of forests")+
  ylab("Accuracy")+
  ggtitle("Train vs Test set Accuracy")

####Q6:
#high:cl1,low # cl2, medium cl3
CL1_coef =c(results_100test$table[1],results_200test$table[1],results_300test$table[1],results_400test$table[1])
CL2_coef =  c(results_100test$table[5],results_200test$table[5],results_300test$table[5],results_400test$table[5])
CL3_coef = c(results_100test$table[9],results_200test$table[9],results_300test$table[9],results_400test$table[9])

ggplot(data.frame(cbind(nfor,CL1_coef,CL2_coef,CL3_coef)), aes(x=nfor))+
  geom_point(aes(y = CL1_coef, color = "darkred"),shape=18,size=3)+
  geom_line(aes(y = CL1_coef,color = "darkred"),linetype="longdash") +
  geom_point(aes(y = CL2_coef,color = "steelblue"),shape=18,size=3)+
  geom_line(aes(y = CL2_coef,color="steelblue"), linetype="dotdash")+
  geom_point(aes(y = CL3_coef, color = "green"),shape=18,size=3)+
  geom_line(aes(y = CL3_coef,color = "green"),linetype="dashed") +
  scale_color_identity(name = "Class coefficient",
                       labels = c("High", "Low","Medium"),
                       guide = "legend") +
  xlab("Number of forests")+
  ylab("Class Coefficient")+
  ggtitle("Diagonal coefficient vs Number of trees")+
  ylim(1200,2300)

############################Q7###############################################
#Bntr = 400

RF_200 <- randomForest(target ~., data=TRAIN_SET,ntree = 400, ntry=4
                       ,importance=T)

predSET_400tr<-predict(RF_400,newdata = TRAIN_SET)
results_400tr = confusionMatrix(predSET_400tr, TRAIN_SET$target)
predSET_400test<-predict(RF_400,newdata = TEST_SET)
results_400test = confusionMatrix(predSET_400test, TEST_SET$target)


#####
varImpPlot((RF_400))
Importance = data.frame(importance(RF_400))
Importance_features = Importance[order(Importance$MeanDecreaseAccuracy,decreasing = TRUE),]
View(Importance_features)
write.csv(Importance_features,"Importance.csv", row.names = TRUE)

#Most important: latitude

set.seed(1)
LOWPRICE = S_NYC[which(S_NYC$target =="Low"),]
set.seed(1)
MEDPRICE = S_NYC[which(S_NYC$target =="Medium"),]
set.seed(1)
HIGHPRICE = S_NYC[which(S_NYC$target =="High"),]
###Compare histograms visually and conduct ks-test
par(mfrow = c(1,3))
hist(LOWPRICE$latitude, main = "Low Price" , col = "lightblue", xlab = "latitude")
hist(MEDPRICE$latitude, main = "Medium Price" , col = "lightblue", xlab = "latitude")
hist(HIGHPRICE$latitude, main = "High Price" , col = "lightblue", xlab = "latitude")

ks.test(LOWPRICE$latitude,MEDPRICE$latitude)
ks.test(LOWPRICE$latitude,HIGHPRICE$latitude)
ks.test(MEDPRICE$latitude,HIGHPRICE$latitude)
#diff population

par(mfrow = c(1,3))
hist(LOWPRICE$neighbourhood_group_Staten.Island, main = "Low Price" , col = "yellow", xlab = "staten")
hist(MEDPRICE$neighbourhood_group_Staten.Island, main = "Medium Price" , col = "yellow", xlab = "staten")
hist(HIGHPRICE$neighbourhood_group_Staten.Island, main = "High Price" , col = "yellow", xlab = "staten")

ks.test(LOWPRICE$neighbourhood_group_Staten.Island,MEDPRICE$neighbourhood_group_Staten.Island)
ks.test(LOWPRICE$neighbourhood_group_Staten.Island,HIGHPRICE$neighbourhood_group_Staten.Island)
ks.test(MEDPRICE$neighbourhood_group_Staten.Island,HIGHPRICE$neighbourhood_group_Staten.Island)

#Least Important:staten_island

#########Q9#######
####cluster 1####
clu1<-subset(full_data,full_data$cluster == 1)
table(clu1$target)
round(prop.table(table(clu1$target)),2) #2%,80%,18% Majority class Low
classifier1 = function(data){
  data$prediction = "Low"
  return(data)
}

#####Cluster 2#####
clu2<-subset(full_data,full_data$cluster == 2)
table(clu2$target)
round(prop.table(table(clu2$target)),2) ###76%,1%,18%, majority class high
classifier2 = function(data){
  data$prediction = "High"
}
#####Cluster 3####
#379    429    651 
clu3<-subset(full_data,full_data$cluster == 3)
table(clu3$target)
round(prop.table(table(clu3$target)),2) ###76%,1%,18%, majority class high
#clone
#TRAIN/TEST SET
cl_low = clu3[which(clu3$target == "Low"),]
cl_medium = clu3[which(clu3$target == "Medium"),]
cl_high = clu3[which(clu3$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
#after undersample: 26,265 and 80
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
#after undersample: 7,66 and 21
table(TRAIN_SET_CL$target)#high:572, low:251, medium:609
table(TEST_SET_CL$target)#144:high,#63 low,#153 medium

RF_400_CL3 <- randomForest(target ~., data=TRAIN_SET_CL[,-17],ntree = 400, ntry=4
                           ,importance=T)
pred_400_CL3<-predict(RF_400_CL3,newdata = TEST_SET_CL)
results= confusionMatrix(pred_400_CL3, TEST_SET_CL$target)

classifier3 = function(data){
  pred_400_CL3<-predict(RF_400_CL3,newdata = data)
  results= confusionMatrix(pred_400_CL3, data$target)
  data$prediction = pred_400_CL3
}

####cluster 4#####
clu4<-subset(full_data,full_data$cluster == 4)
table(clu4$target)
round(prop.table(table(clu4$target)),2) ###majority vote, low 83%

classifier4 = function(data){
  data$prediction = "Low"
}

####cluster 5#####
clu5<-subset(full_data,full_data$cluster == 5)
table(clu5$target)
round(prop.table(table(clu5$target)),2) ###majority vote, low 74%

classifier5 = function(data){
  data$prediction = "Low"
}

####cluster 6####
clu6<-subset(full_data,full_data$cluster == 6)
table(clu6$target)
round(prop.table(table(clu6$target)),2) ###766   2271   2747 , balance class 766 


#TRAIN/TEST SET
cl_low = clu6[which(clu6$target == "Low"),]
cl_medium = clu6[which(clu6$target == "Medium"),]
cl_high = clu6[which(clu6$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H:612   L1816  M 2197
table(TEST_SET_CL$target)#154    455    550 

cloned_tr = TRAIN_SET_CL[,-17]
train.CLONED = SMOTE(cloned_tr[,-c(16)], # feature values
                     as.numeric(cloned_tr[,16]), # class labels
                     K = 4, dup_size = 2)

TRAIN_CLONED<-train.CLONED$data
table(TRAIN_CLONED$class) #H1:402 L2:437 M3:595 

cloned_ts = TEST_SET_CL[,-17]
table(cloned_ts$target)#H:31,L:111,M:151
test.CLONED = SMOTE(cloned_ts[,-c(16)], # feature values
                    as.numeric(cloned_ts[,16]), # class labels
                    K = 4, dup_size = 2)

TEST_CLONED<-test.CLONED$data
table(TEST_CLONED$class) #high1:93,low2:111,medium3:151
TRAIN_CLONED$class = as.factor(TRAIN_CLONED$class)
TEST_CLONED$class = as.factor(TEST_CLONED$class)


RF_400_CL6 <- randomForest(class ~., data=TRAIN_CLONED,ntree = 400, ntry=4
                           ,importance=T)

pred_400_CL6<-predict(RF_400_CL6,newdata = TEST_CLONED)
results= confusionMatrix(pred_400_CL6, TEST_CLONED$class)

classifier6 = function(data){
  pred_400_CL6<-predict(RF_400_CL6,newdata = data)
  results= confusionMatrix(pred_400_CL6, data$target)
  data$prediction = pred_400_CL3
}

#######Cluster 7#####
clu7<-subset(full_data,full_data$cluster == 7)
table(clu7$target)
round(prop.table(table(clu7$target)),2)# H: 2257  L:394   M: 2303 
#
cl_low = clu7[which(clu7$target == "Low"),]
cl_medium = clu7[which(clu7$target == "Medium"),]
cl_high = clu7[which(clu7$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H: 1805    315   1842
table(TEST_SET_CL$target)#H:452     L:79    M:461

cloned_tr = TRAIN_SET_CL[,-17]
train.CLONED = SMOTE(cloned_tr[,-c(16)], # feature values
                     as.numeric(cloned_tr[,16]), # class labels
                     K = 4, dup_size = 4)

TRAIN_CLONED<-train.CLONED$data
table(TRAIN_CLONED$class) #H1:1805 L2:1575 M3:1842
round(prop.table(table(TRAIN_CLONED$class)),2)
cloned_ts = TEST_SET_CL[,-17]
table(cloned_ts$target)
test.CLONED = SMOTE(cloned_ts[,-c(16)], # feature values
                    as.numeric(cloned_ts[,16]), # class labels
                    K = 4, dup_size = 4)

TEST_CLONED<-test.CLONED$data
table(TEST_CLONED$class) #h452 l395 m461 
TRAIN_CLONED$class = as.factor(TRAIN_CLONED$class)
TEST_CLONED$class = as.factor(TEST_CLONED$class)


RF_400_CL7 <- randomForest(class ~., data=TRAIN_CLONED,ntree = 400, ntry=4
                           ,importance=T)

pred_400_CL7<-predict(RF_400_CL7,newdata = TEST_CLONED)
results= confusionMatrix(pred_400_CL7, TEST_CLONED$class)

classifier7 = function(data){
  pred_400_CL7<-predict(RF_400_CL7,newdata = data)
  data$prediction = pred_400_CL7
}






#######Cluster 8#####
clu8<-subset(full_data,full_data$cluster == 8)
table(clu8$target)#majority vote High
classifier8 = function(data){
  data$prediction = "High"
}

#
#####Cluster 9####
clu9<-subset(full_data,full_data$cluster == 9)
table(clu9$target)#clone class low
round(prop.table(table(clu9$target)),2)

#
cl_low = clu9[which(clu9$target == "Low"),]
cl_medium = clu9[which(clu9$target == "Medium"),]
cl_high = clu9[which(clu9$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H: 1192    126   1072
table(TEST_SET_CL$target)#H:299     32    269 
cloned_tr = TRAIN_SET_CL[,-17]
train.CLONED = SMOTE(cloned_tr[,-c(16)], # feature values
                     as.numeric(cloned_tr[,16]), # class labels
                     K = 4, dup_size = 6)

TRAIN_CLONED<-train.CLONED$data
table(TRAIN_CLONED$class) #1192  cloned:low:882 1072 
round(prop.table(table(TRAIN_CLONED$class)),2)
cloned_ts = TEST_SET_CL[,-17]
table(cloned_ts$target)
test.CLONED = SMOTE(cloned_ts[,-c(16)], # feature values
                    as.numeric(cloned_ts[,16]), # class labels
                    K = 4, dup_size = 6)

TEST_CLONED<-test.CLONED$data
table(TEST_CLONED$class) #299 224 269
TRAIN_CLONED$class = as.factor(TRAIN_CLONED$class)
TEST_CLONED$class = as.factor(TEST_CLONED$class)

RF_400_CL9 <- randomForest(class ~., data=TRAIN_CLONED,ntree = 400, ntry=4
                           ,importance=T)

pred_400_CL9<-predict(RF_400_CL9,newdata = TEST_CLONED)
results= confusionMatrix(pred_400_CL9, TEST_CLONED$class)

classifier9 = function(data){
  pred_400_CL9<-predict(RF_400_CL9,newdata = data)
  data$prediction = pred_400_CL9
}


#############3luster 10######

clu10<-subset(full_data,full_data$cluster == 10)
table(clu10$target)#majority high
round(prop.table(table(clu10$target)),2)#clone
classifier10 = function(data){
  data$prediction = "High"
}

###########cluster 11#######
clu11<-subset(full_data,full_data$cluster == 11)
table(clu11$target)#565    249    892 
round(prop.table(table(clu11$target)),2)#clone
#
cl_low = clu11[which(clu11$target == "Low"),]
cl_medium = clu11[which(clu11$target == "Medium"),]
cl_high = clu11[which(clu11$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H: 452    199    713
table(TEST_SET_CL$target)#H:113     50    179 #clone low
cloned_tr = TRAIN_SET_CL[,-17]
train.CLONED = SMOTE(cloned_tr[,-c(16)], # feature values
                     as.numeric(cloned_tr[,16]), # class labels
                     K = 4, dup_size = 1)

TRAIN_CLONED<-train.CLONED$data
table(TRAIN_CLONED$class) #1192  cloned:low:882 1072 
round(prop.table(table(TRAIN_CLONED$class)),2)
cloned_ts = TEST_SET_CL[,-17]
table(cloned_ts$target)
test.CLONED = SMOTE(cloned_ts[,-c(16)], # feature values
                    as.numeric(cloned_ts[,16]), # class labels
                    K = 4, dup_size = 1)

TEST_CLONED<-test.CLONED$data
table(TEST_CLONED$class) #113 100 179
TRAIN_CLONED$class = as.factor(TRAIN_CLONED$class)
TEST_CLONED$class = as.factor(TEST_CLONED$class)

RF_400_CL11 <- randomForest(class ~., data=TRAIN_CLONED,ntree = 400, ntry=4
                            ,importance=T)

pred_400_CL11<-predict(RF_400_CL11,newdata = TEST_CLONED)
results= confusionMatrix(pred_400_CL11, TEST_CLONED$class)

classifier11 = function(data){
  pred_400_CL11<-predict(RF_400_CL11,newdata = data)
  data$prediction = pred_400_CL11
}

##############cluster 12##########
clu12<-subset(full_data,full_data$cluster == 12)
table(clu12$target)#565    249    892 
round(prop.table(table(clu12$target)),2)#clone
cl_low = clu12[which(clu12$target == "Low"),]
cl_medium = clu12[which(clu12$target == "Medium"),]
cl_high = clu12[which(clu12$target == "High"),]
#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H: 16     11     23 
table(TEST_SET_CL$target)#5      3      6 
TRAIN_SET_CL$target = as.factor(TRAIN_SET_CL$target)
TEST_SET_CL$target = as.factor(TEST_SET_CL$target)

RF_400_CL12 <- randomForest(target ~., data=TRAIN_SET_CL,ntree = 400, ntry=4
                            ,importance=T)

pred_400_CL12<-predict(RF_400_CL12,newdata = TEST_SET_CL)
results= confusionMatrix(pred_400_CL12, TEST_SET_CL$target)

classifier12 = function(data){
  pred_400_CL12<-predict(RF_400_CL12,newdata = data)
  data$prediction = pred_400_CL12
}
########cluste3 13#####

clu13<-subset(full_data,full_data$cluster == 13)
table(clu13$target)#40    179     95

cl_low = clu13[which(clu13$target == "Low"),]
cl_medium = clu13[which(clu13$target == "Medium"),]
cl_high = clu13[which(clu13$target == "High"),]

#low
n<-nrow(cl_low)
train<-sample(1:n, 0.8*n)
trainset_low <- cl_low[train,]
testset_low <- cl_low[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(cl_medium)
train<-sample(1:n, 0.8*n)
trainset_medium <- cl_medium[train,]
testset_medium<- cl_medium[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(cl_high)
train<-sample(1:n, 0.8*n)
trainset_high <- cl_high[train,]
testset_high<- cl_high[-train,]


#combined train and test set
TRAIN_SET_CL = data.frame(rbind(trainset_low,trainset_medium,trainset_high)) 
TEST_SET_CL = data.frame(rbind(testset_low,testset_medium,testset_high))
table(TRAIN_SET_CL$target)#H: 32    143     76 
table(TEST_SET_CL$target)# 8     36     19 

cloned_tr = TRAIN_SET_CL[,-17]
table(cloned_tr$target)#H:128,LOW:1548,MEDIUM:926
train.CLONED = SMOTE(cloned_tr[,-c(16)], # feature values
                     as.numeric(cloned_tr[,16]), # class labels
                     K = 4, dup_size = 3)

TRAIN_CLONED<-train.CLONED$data
table(TRAIN_CLONED$class) #128 143  76 
trainCLONED = SMOTE(TRAIN_CLONED[,-c(16)], # feature values
                    as.numeric(TRAIN_CLONED[,16]), # class labels
                    K = 4, dup_size = 1)
TRAIN_CLONED<-trainCLONED$data
table(TRAIN_CLONED$class) #128 143 152

cloned_ts = TEST_SET_CL[,-17]
library("smotefamily")
table(cloned_ts$target)#8     36     19

test.CLONED = SMOTE(cloned_ts[,-c(16)], # feature values
                    as.numeric(cloned_ts[,16]), # class labels
                    K = 4, dup_size = 3)

TEST_CLONED<-test.CLONED$data
table(TEST_CLONED$class) #32 36 19 
testCLONED = SMOTE(TEST_CLONED[,-c(16)], # feature values
                   as.numeric(TEST_CLONED[,16]), # class labels
                   K = 4, dup_size = 1)
TEST_CLONED<-testCLONED$data
table(TEST_CLONED$class) #32 36 38
TRAIN_CLONED = data.frame(TRAIN_CLONED)
TRAIN_CLONED$class = as.factor(TRAIN_CLONED$class)
TEST_CLONED=data.frame(TEST_CLONED)
TEST_CLONED$class = as.factor(TEST_CLONED$class)


RF_400_CL13 <- randomForest(class ~., data=TRAIN_CLONED,ntree = 400, ntry=4
                            ,importance=T)

pred_400_CL13<-predict(RF_400_CL13,newdata = TEST_CLONED)
results= confusionMatrix(pred_400_CL13, TEST_CLONED$class)

classifier13 = function(data){
  pred_400_CL13<-predict(RF_400_CL13,newdata = data)
  data$prediction = pred_400_CL13
}

####cluster 14#####

clu14<-subset(full_data,full_data$cluster == 14)
table(clu14$target)#majority vote low
classifier14 = function(data){
  data$prediction = "Low"
}
######cluster 15####
clu15<-subset(full_data,full_data$cluster == 15)#96   1694    636 
table(clu15$target)#majority vote low
classifier15 = function(data){
  data$prediction = "Low"
}

TEST_SET1



#######Q10######
new = TEST_SET
new$prediction = NA

for(i in 1:nrow(TEST_SET)){
  if(new$cluster[i]=="1"){
    new$prediction[i]="Low"}
  else if(new$cluster[i]=="2"){
    new$prediction[i]="High"}
  else if(new$cluster[i]=="3"){
    pred_400_CL3<-predict(RF_400_CL3,newdata = new[i,])
    new$prediction[i]=pred_400_CL3}
  else if(new$cluster[i]=="4"){
    new$prediction[i]="Low"
  }else if(new$cluster[i]=="5"){
    new$prediction[i]="Low"}
  else if(new$cluster[i]=="6"){
    pred_400_CL6<-predict(RF_400_CL6,newdata = new[i,])
    new$prediction[i]=pred_400_CL6
  }else if(new$cluster[i]=="7"){
    pred_400_CL7<-predict(RF_400_CL7,newdata = new[i,])
    new$prediction[i]=pred_400_CL7
  }else if(new$cluster[i]=="8"){
    new$prediction[i]="High"
  }else if(new$cluster[i]=="9"){
    pred_400_CL9<-predict(RF_400_CL9,newdata = new[i,])
    new$prediction[i] = pred_400_CL9
  }else if(new$cluster[i]=="10"){
    new$prediction[i]="High"
  }else if(new$cluster[i]=="11"){
    pred_400_CL11<-predict(RF_400_CL11,newdata = new[i,])
    new$prediction[i] = pred_400_CL11
  }else if(new$cluster[i]=="12"){
    pred_400_CL12<-predict(RF_400_CL12,newdata = new[i,])
    new$prediction[i] = pred_400_CL12
  }else if(new$cluster[i]=="13"){
    pred_400_CL13<-predict(RF_400_CL13,newdata = new[i,])
    new$prediction[i] = pred_400_CL13
  }else if(new$cluster[i]=="14"){
    new$prediction[i]="Low"
  }else {
    new$prediction[i]="Low"}
}

######
TEST_SET_Q10 = new

for(i in 1:nrow(new)){
  if(new$prediction[i]==1){
    TEST_SET_Q10$prediction[i]="High"
  }else if(new$prediction[i]==2){
    TEST_SET_Q10$prediction[i]="Low"
  }else if(new$prediction[i]==3){
    TEST_SET_Q10$prediction[i]="Medium"
  }else{
    TEST_SET_Q10$prediction = TEST_SET_Q10$prediction
  }
}


TEST_SET_Q10$prediction = as.factor(TEST_SET_Q10$prediction)
results= confusionMatrix(TEST_SET_Q10$target, TEST_SET_Q10$prediction)

###############Q11#######################
set.seed(2)
LOWPRICE = S_NYC1[which(S_NYC1$target =="Low"),]
set.seed(2)
MEDPRICE = S_NYC1[which(S_NYC1$target =="Medium"),]
set.seed(2)
HIGHPRICE = S_NYC1[which(S_NYC1$target =="High"),]

##TRAIN/TEST SET: LOW
n<-nrow(LOWPRICE)
train<-sample(1:n, 0.8*n)
trainset_LOW <- LOWPRICE[train,]
testset_LOW <- LOWPRICE[-train,]

##TRAIN/TEST SET: MEDIUM
n<-nrow(MEDPRICE)
train<-sample(1:n, 0.8*n)
trainset_MED <- MEDPRICE[train,]
testset_MED <- MEDPRICE[-train,]

##TRAIN/TEST SET: HIGH
n<-nrow(HIGHPRICE)
train<-sample(1:n, 0.8*n)
trainset_HIGH <- HIGHPRICE[train,]
testset_HIGH <- HIGHPRICE[-train,]

#######Q11#########
##class low versus medium####
TRAIN_SET_LM = rbind(trainset_LOW,trainset_MED)#low:10243, medium:10655
TEST_SET_LM = rbind(testset_LOW,testset_MED)#low:2561, medium:2664

##class medium versus high###
TRAIN_SET_MH = rbind(trainset_MED,trainset_HIGH)#medium:10655, high:10176
TEST_SET_MH = rbind(testset_MED,testset_HIGH)#medium:2664, #HIGH:2544

###class low versus high###
TRAIN_SET_LH = rbind(trainset_LOW,trainset_HIGH)#LOW:10243, high:10176
TEST_SET_LH = rbind(testset_LOW,testset_HIGH)#low:2544, #HIGH:2544

TRAIN_SET_LH$target = droplevels.factor(TRAIN_SET_LH$target)
TEST_SET_LH$target = droplevels.factor(TEST_SET_LH$target)

#######TRAIN SVM######
library(e1071)
library("caret")
start = Sys.time()#12.466148 mins
set.seed(1)
#1.69 mins
###time: 
tune.out=tune(method="svm",
              target~., 
              data=TRAIN_SET_LH,
              kernel="linear",
              ranges = list(cost = c(0.001,0.01, 0.1, 1,5,10)))

bestmod = tune.out$best.model
summary(tune.out)
#pick lowest dispersion
plot(svm, TRAIN_SET_LH)
end=Sys.time()
Time = end-start
ypred <- predict(svm,TEST_SET_LH)
table(predict=ypred,
        truth=TEST_SET_LH$target)
confusionMatrix(ypred,TEST_SET_LH$target)


###final svm
start = Sys.time()
best = svm(formula = as.factor(target) ~ ., data = TRAIN_SET_LH, kernel = "linear", cost = 5)

plot(best,TRAIN_SET_LH,room_type_Entire.home.apt~reviews_per_month,fill=TRUE,col = c(gray(0.8),"pink"))

###medium low best cost 5

LM = svm(formula = as.factor(target) ~ ., data = TRAIN_SET_LM, kernel = "linear", cost = 5)
ypredLM <- predict(LM,TEST_SET_LM)
confusionMatrix(ypredLM,TEST_SET_LM$target)

#MEDIUM HIGH best cost 5
MH = svm(formula = as.factor(target) ~ ., data = TRAIN_SET_MH, kernel = "linear", cost = 5)
ypred <- predict(MH,TEST_SET_MH)
confusionMatrix(ypred,TEST_SET_MH$target)

#######
LH = svm(formula = as.factor(target) ~ ., data = TRAIN_SET_LH, kernel = "linear", cost = 5)
ypredLH <- predict(LH,TEST_SET_LH)
confusionMatrix(ypredLH,TEST_SET_LH$target)
