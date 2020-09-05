###############################
##Prinipal Component Analysis##
###############################

#############
##Packages###
#############

library(ggplot2) 
library(psych)
library(Rcpp)
library(nnet)

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)



########################
####Install Dataset#####
########################
data_iris <- read.csv(file.choose())
str(data_iris)

summary(data_iris)


###########################################################
####Spliting the dataset into testing and training data####
###########################################################

smp_size <- floor(0.80 * nrow(data_iris))

set.seed(100)

train_ind<- sample(seq_len(nrow(data_iris)), size = smp_size)
train<- data_iris[train_ind, ]
test <- data_iris[-train_ind, ]
head(train)
head(test)

#################################################
####Scatter plot and correlation coefficients####
#################################################

pairs.panels(train[ , 1:4], gap= 0.2, 
             bg= c("red" , "green", "blue")[train$Species], pch = 25)


###################
#######PCA#########
###################

p_c <- prcomp(train[ , -5],
              center =  T,
              scale. = T)

attributes(p_c)
p_c$center
p_c$sdev 

#################################
##Print Principlal Components####
#################################

print(p_c)

summary(p_c)

plot(p_c , type = 'l', main = " ")

biplot(p_c, scale= 0)
##########################
###Orthogonality of PCs###
##########################
pairs.panels(p_c$x, 
             gap= 0.2,
             bg = c("green" , "blue" , "violet")[train$Species],
             pch = 25)

##########################
######create bi-plot######
##########################

b_p <- ggbiplot(p_c, obs.scale = 1, var.scale = 1,
                groups = train$Species, ellipse = T, 
                circle = T, ellipse.prob = 0.68)# 0.68 can be replaced by 0.95

b_p <- b_p + theme(legend.direction = 'horizontal', legend.position = 'bottom') 
print(b_p)

#######################
######Prediction######
######################


training <- predict(p_c , train)
head(training)

training <- data.frame(training, train[5])
head(training)

testing<- predict(p_c,test)
testing<- data.frame(testing , test[5])
head(testing)

#########################################
####Multinomial logistic Regression#####
######################################## 

training$Species <- relevel(training$species, ref= "setosa")

model_1<- multinom(Species~PC1+PC2, data= training)
summary(model_1)


##################################################
#######confusion matrix for training data #######
#################################################

p <- predict(model_1, training)
t <- table(p, training$Species)
t

#################################
######## Misclassification ######
#################################

Misclassification_percentage <- (1-sum(diag(t))/sum(t))*100
Misclassification_percentage

# confusion matrix for testing data
p1 <- predict(model_1, testing)
t1 <- table(p1, testing$species)
t1

#Misclassification
Misclassification_percentage <- (1-sum(diag(t1))/sum(t1))*100
Misclassification_percentage
