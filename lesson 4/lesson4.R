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