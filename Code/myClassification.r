#45155291 Ankit Sharma
#Apply binary classification using decision tree and K-NN techniques.

#--Task 3.1--#
#Load the preprocessed data into a data frame 
processed_data <- readRDS(file="./data/bcw_processed.Rda")

#For reproducible result
set.seed(5291)

#Divide the dataset into “training” and “test” subsets randomly (70% and 30% respectively).

#Set training and test ratio
m = nrow(processed_data)
training_percentage = 0.7
test_percentage = 0.3

#Sample random index
ind <- sample(2,m,replace = TRUE, prob = c(training_percentage,test_percentage))

#Select training and test data
training_data = processed_data[ind == 1,]
test_data = processed_data[ind == 2,]

#--Task 3.2--#
#Learn a classification tree from the training data using the default parameters 
#of the ctree function from the “party” library. 

#Data partitioning
training_features <- training_data[,1:9]
training_labels <- training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

#install and import "party" library
install.packages("party")
library(party)

#Generate classification tree
data_ctree <- ctree(Class ~ .,data = training_data)

#Plot of classification tree
png(filename=paste("./Plot/Task3.2 Classification Tree",".png"), width = 1080, height = 1080)
plot(data_ctree)
dev.off()

#predict test labels
ctree_pred <- predict(data_ctree, newdata = test_features)

#Comments on its structure
#The  most important variable is Bare.Nuclei because it is used in the first
#The other important variables in no specific order are Uniformity.of.Cell.Shape, Bland.Chromatin, Normal Nucleoli and Mitosis
#It is imporant to note that the variable chosen at low cuts are dependent of their combined performance with variables chosen in the above cuts
#As such a variable with better feature ranking may be present in lower cuts
#With a big certainty the variables not present at all have low imporantance
#Is there any knowledge we can infer from the tree representation that helps in differentiating between the classes?). 
#The classes can clearly be differentiated based on high importance varaibles such as Bare.Nuclei, Uniformity.of.Cell.Shape, Bland.Chromatin etc
#We can clearly identify benign class (bottom left) based on imporant variable values : Bare.Nuclei <= 3, Uniformity.of.Cell.Shape <= 3, Normal.Nucleoli <= 3 and Mitoses <= 1
#Similarly, malignant class has Bare.Nuclei > 3 and Bland.Chromatin > 2


#Using the learned tree, predict the class labels of the test data.
#Calculate the accuracy, precision, and recall.

#Create confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = ctree_pred))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm,1,sum) # number of instances per class
colsums = apply(cm,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy = sum(diag)/n
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/(precision+recall)

results <- data.frame(precision,recall,f1)
accuracy
results

#accuracy 0.9497717
#results
#precision    recall        f1
#2 0.9785714 0.9448276 0.9614035
#4 0.8987342 0.9594595 0.9281046

#--Task 3.3--#
#Try building you classification tree again via the ctree function but 
#using parameters that are different from the default settings. 
#Can you achieve better accuracy or more meaningful representation by tuning some parameters? 
#(Note that in the ctree function from “party” library, you can modifiy ctree_control parameters. 
#Execute ?ctree form RStudio Console for the detailed documentation.)

#Generate classification tree
myFormula <- Class ~ Uniformity.of.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses
data_ctree <- ctree(myFormula,data = training_data)

#Plot of classification tree
plot(data_ctree)
#plot(data_ctree, type='simple')

#predict test labels
ctree_pred <- predict(data_ctree, newdata = test_features)

#Create confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = ctree_pred))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm,1,sum) # number of instances per class
colsums = apply(cm,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy = sum(diag)/n
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/(precision+recall)

results <- data.frame(precision,recall,f1)
accuracy
results

#accuracy 0.9497717
#> results
#precision    recall        f1
#2 0.9785714 0.9448276 0.9614035
#4 0.8987342 0.9594595 0.9281046

#By using different  testtype {MonteCarlo, Bonferroni, Teststatistics} we may achieve better results

#--Task 3.4--#
#Apply K-NN classification to predict the labels in the test subset and 
#calculate the accuracy, precision and recall. 
#Particularly, try different values of K (e.g. K = 1, 2, 3, 4, 5), and 
#report your observations on the achieved classification.

#install and import "class" library
install.packages("class")
library(class)

#KNN k=1
knn_pred_1 <- knn(train = training_features, test = test_features, cl = training_labels, k =1)

cm_knn1 = as.matrix(table(Actual = test_labels, Predicted = knn_pred_1))

n_knn1 = sum(cm_knn1) # number of instances
nc_knn1 = nrow(cm_knn1) # number of classes
diag_knn1 = diag(cm_knn1) # number of correctly classified instances per class
rowsums_knn1 = apply(cm_knn1,1,sum) # number of instances per class
colsums_knn1 = apply(cm_knn1,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy_knn1 = sum(diag_knn1)/n_knn1
precision_knn1 = diag_knn1/colsums_knn1
recall_knn1 = diag_knn1/rowsums_knn1
f1_knn1 = 2*precision_knn1*recall_knn1/(precision_knn1+recall_knn1)

results_knn1 <- data.frame(precision_knn1,recall_knn1,f1_knn1)
accuracy_knn1
results_knn1

#accuracy_knn1
#[1] 0.9634703
#precision_knn1 recall_knn1   f1_knn1
#2      0.9790210   0.9655172 0.9722222
#4      0.9342105   0.9594595 0.9466667

#KNN k=2
knn_pred_2 <- knn(train = training_features, test = test_features, cl = training_labels, k =2)

cm_knn2 = as.matrix(table(Actual = test_labels, Predicted = knn_pred_2))

n_knn2 = sum(cm_knn2) # number of instances
nc_knn2 = nrow(cm_knn2) # number of classes
diag_knn2 = diag(cm_knn2) # number of correctly classified instances per class
rowsums_knn2 = apply(cm_knn2,1,sum) # number of instances per class
colsums_knn2 = apply(cm_knn2,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy_knn2 = sum(diag_knn2)/n_knn2
precision_knn2 = diag_knn2/colsums_knn2
recall_knn2 = diag_knn2/rowsums_knn2
f1_knn2 = 2*precision_knn2*recall_knn2/(precision_knn2+recall_knn2)

results_knn2 <- data.frame(precision_knn2,recall_knn2,f1_knn2)
accuracy_knn2
results_knn2

#> accuracy_knn2
#[1] 0.9634703
#precision_knn2 recall_knn2   f1_knn2
#2      0.9858156   0.9586207 0.9720280
#4      0.9230769   0.9729730 0.9473684

#KNN k=3
knn_pred_3 <- knn(train = training_features, test = test_features, cl = training_labels, k =3)

cm_knn3 = as.matrix(table(Actual = test_labels, Predicted = knn_pred_3))

n_knn3 = sum(cm_knn3) # number of instances
nc_knn3 = nrow(cm_knn3) # number of classes
diag_knn3 = diag(cm_knn3) # number of correctly classified instances per class
rowsums_knn3 = apply(cm_knn3,1,sum) # number of instances per class
colsums_knn3 = apply(cm_knn3,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy_knn3 = sum(diag_knn3)/n_knn3
precision_knn3 = diag_knn3/colsums_knn3
recall_knn3 = diag_knn3/rowsums_knn3
f1_knn3 = 2*precision_knn3*recall_knn3/(precision_knn3+recall_knn3)

results_knn3 <- data.frame(precision_knn3,recall_knn3,f1_knn3)
accuracy_knn3
results_knn3

#accuracy_knn3
#[1] 0.9726027
#precision_knn3 recall_knn3   f1_knn3
#2          1.000   0.9586207 0.9788732
#4          0.925   1.0000000 0.9610390

#KNN k=4
knn_pred_4 <- knn(train = training_features, test = test_features, cl = training_labels, k =4)

cm_knn4 = as.matrix(table(Actual = test_labels, Predicted = knn_pred_4))

n_knn4 = sum(cm_knn4) # number of instances
nc_knn4 = nrow(cm_knn4) # number of classes
diag_knn4 = diag(cm_knn4) # number of correctly classified instances per class
rowsums_knn4 = apply(cm_knn4,1,sum) # number of instances per class
colsums_knn4 = apply(cm_knn4,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy_knn4 = sum(diag_knn4)/n_knn4
precision_knn4 = diag_knn4/colsums_knn4
recall_knn4 = diag_knn4/rowsums_knn4
f1_knn4 = 2*precision_knn4*recall_knn4/(precision_knn4+recall_knn4)

results_knn4 <- data.frame(precision_knn4,recall_knn4,f1_knn4)
accuracy_knn4
results_knn4

#> accuracy_knn4
#[1] 0.9680365
#precision_knn4 recall_knn4   f1_knn4
#2      0.9859155   0.9655172 0.9756098
#4      0.9350649   0.9729730 0.9536424


#KNN k=5
knn_pred_5 <- knn(train = training_features, test = test_features, cl = training_labels, k =5)

cm_knn5 = as.matrix(table(Actual = test_labels, Predicted = knn_pred_5))

n_knn5 = sum(cm_knn5) # number of instances
nc_knn5 = nrow(cm_knn5) # number of classes
diag_knn5 = diag(cm_knn5) # number of correctly classified instances per class
rowsums_knn5 = apply(cm_knn5,1,sum) # number of instances per class
colsums_knn5 = apply(cm_knn5,2,sum) # number of predictions per class

#compute accuracy, precision, recall and f1
accuracy_knn5 = sum(diag_knn5)/n_knn5
precision_knn5 = diag_knn5/colsums_knn5
recall_knn5 = diag_knn5/rowsums_knn5
f1_knn5 = 2*precision_knn5*recall_knn5/(precision_knn5+recall_knn5)

results_knn5 <- data.frame(precision_knn5,recall_knn5,f1_knn5)
accuracy_knn5
results_knn5

#accuracy_knn5  0.9680365
#precision_knn5 recall_knn5   f1_knn5
#2      0.9928571   0.9586207 0.9754386
#4      0.9240506   0.9864865 0.9542484
