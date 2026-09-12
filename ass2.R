

# import dataset

data("penguins")

# check dataset

head(penguins)

# summary

summary(penguins)

# missing values

which(is.na(penguins))
sum(is.na(penguins))
colSums(is.na(penguins))

# handle missing values
# since 19 missing only we simply omit those rows

df <- na.omit(penguins)
head(df)

# check if done correctly
which(is.na(df))
sum(is.na(df))
colSums(is.na(df))

# remove duplicates

duplicated(df)
sum(duplicated(df))

# correct errors
# NONE

# outliers

# scatterplot -> no outlier

#plot(df$body_mass)
plot(df$year)
plot(df$bill_len)
plot(df$bill_dep)
plot(df$flipper_len)

# boxplot -> no outlier

boxplot(df$body_mass)
boxplot(df$year)
#boxplot(df$bill_len)
boxplot(df$bill_dep)
boxplot(df$flipper_len)

# histogram -> some have less freq but could be since they also recorded babies

hist(df$body_mass)
plot(penguins$island,penguins$species)
hist(df$bill_len)
#hist(df$bill_dep)
hist(df$flipper_len)

# density plot

plot(density(df$body_mass))
plot(density(df$year))
plot(density(df$bill_len))
plot(density(df$bill_dep))
plot(density(df$flipper_len))

pairs(df)

pie(table(df$species))

# via statistics

correlation <- cor(penguins$body_mass,penguins$flipper_len)
correlation
plot(penguins$body_mass,penguins$flipper_len, main = "Scatterplot of Body Mass vs. Flipper length")
abline(lm(penguins$body_mass ~ penguins$flipper_len), col = "red")
text(3, 90, paste("Correlation: ", round(correlation, 2)))

# std dev

print(sd(df$bill_dep))

# mean

print(mean(df$bill_dep))

# format numerical values

df$flipper_len<-as.numeric(as.character(df$flipper_len))
df$body_mass<-as.numeric(as.character(df$body_mass))
# convert data type
df$sex<-(as.character(df$sex))
df['sex'][df['sex']=="female"]<-0
df['sex'][df['sex']=="male"]<-1
df$sex<-as.numeric(df$sex)

df$island<-(as.character(df$island))
df['island'][df['island']=="Biscoe"]<-0
df['island'][df['island']=="Dream"]<-1
df['island'][df['island']=="Torgersen"]<-2
df$island<-(as.numeric(df$island))
summary(df)
head(df)
# format date

# create calculated column

# filter unwanted records

df <- df[,-which(names(df)=="year")]
head(df)
# sort and group data

# grp data via their species

library(dplyr)
grouped_species <- df %>% group_by(species)
head(grouped_species)
grouped_species <- grouped_species %>% summarise(
  bill_length = mean(bill_len),
  bill_depth = mean(bill_dep),
  flipper_length = mean(flipper_len),
  body_mass = mean(body_mass)
)
print(grouped_species)


summary(df)

find_mode<-function(x){
  uniq<-unique(x)
  tab<-tabulate(match(x,uniq))
  uniq[tab==max(tab)]
}
print("Statistics of bill length of penguins: ")
print(sprintf("Mean: %f",mean(df$bill_len)))
print(sprintf("Median: %f",median(df$bill_len)))
print(sprintf("Mode: %f",find_mode(df$bill_len)))
print(sprintf("Minimum: %f",min(df$bill_len)))
print(sprintf("Maximum: %f",max(df$bill_len)))
print(sprintf("Standard deviation: %f",sd(df$bill_len)))

print("Statistics of bill depth of penguins: ")
print(sprintf("Mean: %f",mean(df$bill_dep)))
print(sprintf("Median: %f",median(df$bill_dep)))
print(sprintf("Mode: %f",find_mode(df$bill_dep)))
print(sprintf("Minimum: %f",min(df$bill_dep)))
print(sprintf("Maximum: %f",max(df$bill_dep)))
print(sprintf("Standard deviation: %f",sd(df$bill_dep)))

print("Statistics of flipper length of penguins: ")
print(sprintf("Mean: %f",mean(df$flipper_len)))
print(sprintf("Median: %f",median(df$flipper_len)))
print(sprintf("Mode: %f",find_mode(df$flipper_len)))
print(sprintf("Minimum: %f",min(df$flipper_len)))
print(sprintf("Maximum: %f",max(df$flipper_len)))
print(sprintf("Standard deviation: %f",sd(df$flipper_len)))

print("Statistics of body mass of penguins: ")
print(sprintf("Mean: %f",mean(df$body_mass)))
print(sprintf("Median: %f",median(df$body_mass)))
print(sprintf("Mode: %f",find_mode(df$body_mass)))
print(sprintf("Minimum: %f",min(df$body_mass)))
print(sprintf("Maximum: %f",max(df$body_mass)))
print(sprintf("Standard deviation: %f",sd(df$body_mass)))

print("Species distribution: ")
print(table(df$species))
print("Sex distribution: ")
print(table(df$sex))
print("Island distribution: ")
print(table(df$island))


# NAIVE BAYES CLASSIFICATION

#e1071: Contains Naive Bayes classifier (naiveBayes()) and other useful machine learning functions.
library("e1071")
#caTools: Provides utilities for data splitting (for training and test sets).
library("caTools")
#caret: Simplifies machine learning tasks like training models, evaluating them and creating confusion matrices.
library("caret")

set.seed(123)
split <- sample.split(df,SplitRatio=0.7)
train_df <- subset(df,split==TRUE)
test_df <- subset(df,split==FALSE)

classifier <- naiveBayes(species ~ .,train_df)
print(classifier)

y_pred <- predict(classifier,newdata = test_df, interval="prediction",level=0.95)
print(y_pred)

cm <- table(test_df$species,y_pred)
print(confusionMatrix(cm))

result <- data.frame(y_pred,test_df$species)
names(result) <- c("Predicted","Actual")

nrow(test_df)

Correct <- rep(0,143)
Group1 <- Group2 <- Group3 <- 0
C1 <- C2 <- C3 <- 0

for(i in 1:143){
  if(result$Predicted[i]==result$Actual[i]){
    Correct[i]=1
  }
  if(result$Actual[i]=="Adelie"){
    C1 = C1 + 1
    if(Correct[i]==1){
      Group1 = Group1 + 1
    }
  }
  else if(result$Actual[i]=="Chinstrap"){
    C2 = C2 + 1
    if(Correct[i]==1){
      Group2 = Group2 + 1
    }
  }
  else if(result$Actual[i]=="Gentoo"){
    C3 = C3 + 1
    if(Correct[i]==1){
      Group3 = Group3 + 1
    }
  }
}

total_correct = round(sum(Correct)*100/143,2)
total_C1 = round(sum(Group1)*100/C1,2)
total_C2 = round(sum(Group2)*100/C2,2)
total_C3 = round(sum(Group3)*100/C3,2)

print(paste("Total Accuracy: ",total_correct,"%"))
print(paste("Adelie Accuracy: ",total_C1,"%"))
print(paste("Chinstrap Accuracy: ",total_C2,"%"))
print(paste("Gentoo Accuracy: ",total_C3,"%"))

barplot(c(total_correct,total_C1,total_C2,total_C3),names.arg=c("Total","Adelie","Chinstrap","Gentoo"),
        main=paste("Accuracy of Penguin Species"),xlab="Metrics",ylab="Accuracy %",col=c("skyblue","beige","pink","seagreen"))



#classmap
print("Classification via randomForest")
forest<-randomForest(species~.,data=train_df,keep.forest=TRUE)

pred_y <- predict(forest,newdata = test_df, interval="prediction",level=0.95)
result <- data.frame(pred_y,test_df$species)
names(result) <- c("Predicted","Actual")

#nrow(test_df)

Correct <- rep(0,143)
Group1 <- Group2 <- Group3 <- 0
C1 <- C2 <- C3 <- 0

for(i in 1:143){
  if(result$Predicted[i]==result$Actual[i]){
    Correct[i]=1
  }
  if(result$Actual[i]=="Adelie"){
    C1 = C1 + 1
    if(Correct[i]==1){
      Group1 = Group1 + 1
    }
  }
  else if(result$Actual[i]=="Chinstrap"){
    C2 = C2 + 1
    if(Correct[i]==1){
      Group2 = Group2 + 1
    }
  }
  else if(result$Actual[i]=="Gentoo"){
    C3 = C3 + 1
    if(Correct[i]==1){
      Group3 = Group3 + 1
    }
  }
}

total_correct = round(sum(Correct)*100/143,2)
total_C1 = round(sum(Group1)*100/C1,2)
total_C2 = round(sum(Group2)*100/C2,2)
total_C3 = round(sum(Group3)*100/C3,2)

barplot(c(total_correct,total_C1,total_C2,total_C3),names.arg=c("Total","Adelie","Chinstrap","Gentoo"),main=paste("Accuracy of Penguin Species"),xlab="Metrics",ylab="Accuracy %",col=c("skyblue","beige","pink","seagreen"))

print("Accuracy distribution: ")
print(paste("Total Accuracy: ",total_correct,"%"))
print(paste("Adelie Accuracy: ",total_C1,"%"))
print(paste("Chinstrap Accuracy: ",total_C2,"%"))
print(paste("Gentoo Accuracy: ",total_C3,"%"))

# ggplot
library(ggfortify)
library(plotly)
library(cluster)

set.seed(1)
g<-autoplot(clara(df[,2:7],3),frame=TRUE)
summary(g)
print(g)

vcrtrain <- vcr.forest.train(train_df[, 2:7],train_df[,1],trainfit=forest)
vcrout <- vcr.forest.newdata(test_df[, 2:7],test_df[,1],vcrtrain)
print("Confusion matrix")
print(confmat.vcr(vcrout))
print(stackedplot(vcrout, classCols= c(1,2,3)))
print(silplot(vcrout,classCols=c(1,2,3)))
classmap(vcrout, "Adelie", classCols = 2:7) # tight class
classmap(vcrout, "Chinstrap", classCols = 2:7) # less tight
# The cases misclassified as virginica are shown in blue.
classmap(vcrout, "Gentoo", classCols = 2:7)
