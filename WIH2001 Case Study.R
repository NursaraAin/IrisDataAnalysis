library(caret)
library(party)
library(e1071)
library(caTools)
library(dplyr)
library(car)

setwd("C:/Users/nursa/Desktop/sem4/WIH2001")
flower=read.csv("Iris.csv")

head(flower)
#Id SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm     Species
#1  1           5.1          3.5           1.4          0.2 Iris-setosa
#2  2           4.9          3.0           1.4          0.2 Iris-setosa
#3  3           4.7          3.2           1.3          0.2 Iris-setosa
#4  4           4.6          3.1           1.5          0.2 Iris-setosa
#5  5           5.0          3.6           1.4          0.2 Iris-setosa
#6  6           5.4          3.9           1.7          0.4 Iris-setosa

dim(flower)
#[1] 150   6

summary(flower)
#Id         SepalLengthCm    SepalWidthCm   PetalLengthCm    PetalWidthCm     Species         
#Min.   :  1.00   Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   Length:150        
#1st Qu.: 38.25   1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   Class :character  
#Median : 75.50   Median :5.800   Median :3.000   Median :4.350   Median :1.300   Mode  :character  
#Mean   : 75.50   Mean   :5.843   Mean   :3.054   Mean   :3.759   Mean   :1.199                     
#3rd Qu.:112.75   3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                     
#Max.   :150.00   Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500   

barchart(flower$spec, xlab="Frequency", ylab="Category", main="Distribution of Classes")

flower=rename(flower,slen=SepalLengthCm)
flower=rename(flower,swid=SepalWidthCm)
flower=rename(flower,plen=PetalLengthCm)
flower=rename(flower,pwid=PetalWidthCm)
flower=rename(flower,spec=Species)

classes=unique(flower$spec)

flower$spec=replace(flower$spec,flower$spec=='Iris-setosa',"1")
flower$spec=replace(flower$spec,flower$spec=='Iris-versicolor',"2")
flower$spec=replace(flower$spec,flower$spec=='Iris-virginica',"3")

ind <- sapply(flower, is.factor)
flower[ind] <- lapply(flower[ind], function(x) as.numeric(as.character(x)))
ind1 <- sapply(flower, is.character)
flower[ind1] <- lapply(flower[ind1], function(x) as.numeric(as.character(x)))

flower=subset(flower,select=-c(Id))

summarysummary(flower$spec)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     1       1       2       2       3       3 

sd(flower$slen)
sd(flower$swid)
sd(flower$plen)
sd(flower$pwid)

var(flower$slen)
var(flower$swid)
var(flower$plen)
var(flower$pwid)

scatterplot(slen ~ swid, data = flower, grid=FALSE, frame= FALSE)
scatterplot(plen ~ pwid, data = flower, grid=FALSE, frame= FALSE)
scatterplot(slen ~ swid, data = flower, grid=FALSE, frame= FALSE)
scatterplot(swid ~ pwid, data = flower, grid=FALSE, frame= FALSE)

tab <- table(flower$plen) # number of occurrences for each unique value
sort(tab, decreasing = TRUE) # sort highest to lowest

hist(flower$plen,
     main="Histogram",
     xlab="Petal Width",
     xlim=c(0,8),
     col="palevioletred")

corr<-subset(flower,select=-c(spec))
round(cor(corr),2)
library(psych)
corPlot(corr)

pairs(corr)
#      slen  swid  plen  pwid  spec
#slen  1.00 -0.11  0.87  0.82  0.78
#swid -0.11  1.00 -0.42 -0.36 -0.42
#plen  0.87 -0.42  1.00  0.96  0.95
#pwid  0.82 -0.36  0.96  1.00  0.96
#spec  0.78 -0.42  0.95  0.96  1.00

rg=subset(flower,select=-c(spec,plen,pwid))

model=lm(rg$slen~rg$swid)

set.seed(7267166)
trainIndex = createDataPartition(flower$spec, p = 0.7)$Resample1
train = flower[trainIndex, ]
test = flower[-trainIndex, ]

print(table(flower$spec))

print(table(train$spec))

train_scale <- scale(train[, 1:4])
test_scale <- scale(test[, 1:4])

# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(spec ~ ., data = train)
classifier_cl

#Y
#1         2         3 
#0.3333333 0.3333333 0.3333333 

#Conditional probabilities:
#  slen
#Y       [,1]      [,2]
#1 5.028571 0.3626467
#2 5.854286 0.5310051
#3 6.714286 0.6088286

#swid
#Y       [,1]      [,2]
#1 3.454286 0.3632718
#2 2.748571 0.3137735
#3 3.051429 0.3165730

#plen
#Y       [,1]      [,2]
#1 1.462857 0.1880014
#2 4.177143 0.4941337
#3 5.671429 0.5602370

#pwid
#Y       [,1]      [,2]
#1 0.240000 0.1142752
#2 1.314286 0.2157652
#3 2.088571 0.2794653


# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test)

# Confusion Matrix
cm <- table(test$spec, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)

#Confusion Matrix and Statistics

#y_pred
#1  2  3
#1 15  0  0
#2  0 15  0
#3  0  2 13

#Overall Statistics

#Accuracy : 0.9556          
#95% CI : (0.8485, 0.9946)
#No Information Rate : 0.3778          
#P-Value [Acc > NIR] : 2.61e-16        

#Kappa : 0.9333          

#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#                     Class: 1 Class: 2 Class: 3
#Sensitivity            1.0000   0.8824   1.0000
#Specificity            1.0000   1.0000   0.9375
#Pos Pred Value         1.0000   1.0000   0.8667
#Neg Pred Value         1.0000   0.9333   1.0000
#Prevalence             0.3333   0.3778   0.2889
#Detection Rate         0.3333   0.3333   0.2889
#Detection Prevalence   0.3333   0.3333   0.3333
#Balanced Accuracy      1.0000   0.9412   0.9688

