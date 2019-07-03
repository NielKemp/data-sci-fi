
#LOTS OF EXAMPLES HERE: https://keras.rstudio.com/articles/examples/index.html

#install keras R package from CRAN
install.packages("keras")

#install core Keras lib and TensorFlow backend
#Anaconda manage way:
library(keras)
install_keras(method = "conda")
install_keras()

#above methods don't work, try directly from github
#install.packages("devtools")
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")

reticulate::py_config()



#things to do if nothing works:
#try first method (up to line 8) (gave location pythong something error)
#tried devtools approach -> had issues due to updating rland and processx
#update EVERYTHING (I'm sure this will create problems with tidyverse...)
#rerun devtolls stuff (this now works), run tf method, system can't find tf stuff.
#rerun first method (install_keras(method = "conda"))
#finally works, verify tf is installed using reticulate


library(tidyverse)
load("rawData/trump_tfidf.RData")
head(training_tweets)

library(keras)

#training data
x_train <- as.matrix(training_tweets[,1:201],ncol=201)
y_train <- as.matrix(1*(training_tweets[,202]),ncol=1)

#testing data
x_test <- as.matrix(test_tweets[,1:201],ncol=201)
y_test <- as.matrix(1*(test_tweets[,202]),ncol=1)
head(x_train)
head(y_train)
head(x_test)
head(y_test)

#create the model
model <- keras_model_sequential() #define an empty sequential model

#define a model by sequantially adding layers
model %>% layer_dense(units = 32, #specifies neurons in layer
            input_shape = c(201) #specifies number of input parameters
            )%>%
layer_activation('relu') %>%
layer_dense(units=1)%>% #specifies output layer
  layer_activation('sigmoid')#specifies activation function of output layers

summary(model)

#need to specify loss function and optimizer.
#going to use logarithmic loss - For the two-class problem is called: binary crossentropy (by Keras)
#Categorical crossentropy for multi-class classification problems
#cross entropy is a measure of how different two probability ditsributions are.
#In this case we have the distributions of the predicted vs the actual
#popular optimization algorithms: Stochastic Gradient Descent, Adam and RMSprop

model %>% compile(optimizer = 'rmsprop', loss = 'binary_crossentropy', metrics = c('accuracy'))
#Can do define and compile steps together as well.

#Need to train model now (It's still only defined)
#use batch training with sizes of 32
model %>% fit(x_train, y_train, epochs = 50, batch_size = 32) %>% plot()

#if we want to run for 5 more epochs, we just make another call
model %>% fit(x_train, y_train, epochs = 5, batch_size = 32)

#evaluate model on test data
model %>% evaluate(x_test,y_test, batch_size = 32, verbose = 1)

#make some predictions
model %>% predict_classes(x_test) %>%head()


#Adding more layers (Deep learning)

model2<-keras_model_sequential()
model2 %>%
  layer_dense(units=16,activation='relu',input_shape = c(201))%>%
  layer_dense(units=16,activation='relu')%>%
  layer_dense(units=1,activation = 'sigmoid')%>%
  compile(loss = 'binary_crossentropy',
          optimizer='rmsprop',
          metrics=c('accuracy')
          )
summary(model2)

model2 %>% fit(x_train,y_train,epochs=50,batch_size = 32)%>%plot()

model2 %>% evaluate(x_test,y_test,batch_size=32,verbose=1)


#adding dropout
model3 <- keras_model_sequential()

#define and compile
model3 %>%
  layer_dense(units=16,activation = 'relu',input_shape = c(201))%>%
  layer_dropout(rate=0.2)%>%
  layer_dense(units=16,activation='relu') %>%
  layer_dropout(rate=0.2)%>%
  layer_dense(units=1,activation='sigmoid')%>%
  compile(
    loss='binary_crossentropy',
    optimizer='rmsprop',
    metrics=c('accuracy')
  )

#train
model3 %>% fit(x_train,y_train,epochs=100,batch_size=32)%>%plot()

#evaluate
model3%>%evaluate(x_test,y_test,batch_size=32,verbose=1)


#More neural nets, this time on MNIST data
library(imager) #has many useful tools for image processing

#read Mnist data into object
mnist <- dataset_mnist() #comes built into the Keras package
head(mnist)

#xTrain/xTest are 3-d arrays(images,width, height) of greyscale values.
x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y

#plot a few of the images
par(mfrow =c(1,5))
for( i in 1:5){plot(as.cimg(t(x_test[i,,])))}

#treat each pixel independently, by unvravelling the 28x28 matrix into a vector of length 784 
#this means that each row will be a different pixel of the image
#the 0 to 255 greyscale encoding will be converted down to a range of 0 to 1

#reshape matrix into vector
dim(x_train) <- c(nrow(x_train),784)
dim(x_test) <- c(nrow(x_test),784)

head(x_train)

#rescale
x_train <- x_train/255
x_test <- x_test/255

#check current target variable
head(y_train)
head(y_test)

#change targets into:
# 0 -> [1,0,0,0,0,0,0,0,0]
# 1 -> [0,1,0,0,0,0,0,0,0]
# 2 -> [0,0,1,0,0,0,0,0,0] etc

#do this using 1-hot encoding
#one-hot encoding is implemented using to_categorical() in Keras
y_train <- to_categorical(y_train,10)
y_test <-to_categorical(y_test,10)

head(y_train)
head(y_test)

#create model
model <- keras_model_sequential()

#define model
#fit a 2 h-layer model with dropout
#1st layer -> 128 neurons, 2nd layer -> 64 neurons. Both using relu activation functions
#use softmax activation in output layer
#softmax has the properties that it forces the outputs to sum up to 1, so it's perfect for probabilities
#predicted class is the class with the highest predicted probability

model %>%
  layer_dense(units=128, activation ='relu', input_shape =c(784))%>%
  layer_dropout(rate=0.4)%>%
  layer_dense(units=64,activation='relu')%>%
  layer_dropout(rate=0.3)%>%
  layer_dense(units=10,activation='softmax')

#check model summary
summary(model)

#compile the model
model %>% compile(
  loss='categorical_crossentropy',
  optimizer='rmsprop',
  metrics=c('accuracy')
  )

#train the model 

history <- model %>% fit(x_train,y_train,
                         epochs=3, batch_size = 32,
                         validation_split = 0.2#add this only to training for hyperparam tuning
                         )

plot(history)

#evaluate the model
model %>% evaluate(x_test,y_test)

#look at some images the model predicted incorrectly
preds <- model %>% predict_classes(x_test)

wrong_preds <- tibble(id = 1:nrow(y_test),
                      obs = mnist$test$y,
                      preds=preds)%>%
  filter(obs!=preds)

par(mfrow = c(1:5))
for(i in 1:5){
            wrong_x <- mnist$test$x[wrong_preds$id[i],,]
            plot(as.cimg(t(wrong_x)))
            }