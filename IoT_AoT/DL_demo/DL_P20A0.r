#############################################################################
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("statmod","RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/2/R")
library(h2o)
localH2O = h2o.init()

#install.packages("caret")
library(caret)

#install.packages('e1071', dependencies=TRUE)
library(e1071)

#install.packages("bit64")
library(bit64)
#############################################################################
setwd("C:/Users/TK186040/Desktop/UseCase_DL")
org.df <- read.csv("./data_P20A0.csv", header=TRUE)
#org.df <- subset(org.df, dtc=="P20A0" & label_type=="SCR-B")
org.df <- subset(org.df, t < 30)


create.train.test.label <- function(org.df) {
  set.seed(56789)
  df <- subset(org.df, t==0)[c("car_id","train_test","cause_id")]
  cid.tbl <- table(df$cause_id)
  print(cid.tbl)
  if (min(cid.tbl) == 1) {
    stop("Not enough data")
  }
  
  causeids <- names(cid.tbl)
  labeled.df <- do.call(rbind, lapply(causeids, function(cid) {
    sub.df <- subset(df, cause_id==cid)
    sub.df$train_test <- as.character(sub.df$train_test)
    n <- nrow(sub.df)
    train.n <- ifelse(ceiling(0.97*n) == n, n-1, ceiling(0.97*n))
    train.idx <- sample(n, train.n)
    sub.df[train.idx, "train_test"] <- "train"
    sub.df[-train.idx, "train_test"] <- "test"
    sub.df
  }))
  
  new.df <- org.df
  new.df$train_test <- NULL
  new.df <- merge(new.df, labeled.df[c(1,2)], by.x="car_id")
  new.df
}

org.df <- create.train.test.label(org.df)
org.df$cause_id <- as.character(org.df$cause_id)
write.csv(org.df, "data_P20A0.csv")

# Data normalization
train.df <- subset(org.df, train_test=="train")
test.df <- subset(org.df, train_test=="test")

## Create an empty data frame for results 
res <- data.frame(training = NA, test = NA) 

target.vars <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10",
                 "x11","x12","x13","x14","x15","x16","x17","x18","x19","x20",
                 "x21","x22","x23","x24","x25","x26","x27","x28","x29","x30",
                 "x31","x32","x33","x34","x35","x36","x37","x38","x39","x40",
                 "x41","x42","x43","x44","x45","x46","x47","x48","x49","x50",
                 "x51","x52","x53","x54","x55","x56","x57","x58","x59","x60",
                 "x61","x62","x63","x64","x65","x66","x67","x68","x69","x70",
                 "x71","x72","x73","x74","x75","x76","x77","x78","x79","x80",
                 "x81","x82","x83","x84","x85","x86","x87","x88","x89","x90",
                 "x91","x92","x93","x94","x95","x96","x97","x98","x99","x100",
                 "x101","x102","x103","x104","x105","x106","x107","x108")

# Data normalization
train.df <- train.df[c("car_id","cause_id", "t", target.vars)]
write.csv(train.df, "./train_norm_P20A0_SCR-B.csv", row.names=F)

test.df <- test.df[c("car_id","cause_id", "t", target.vars)]
write.csv(test.df, "./test_norm_P20A0_SCR-B.csv", row.names=F)

train <-  h2o.importFile(path = normalizePath("./train_norm_P20A0_SCR-B.csv"))
test <-  h2o.importFile(path = normalizePath("./test_norm_P20A0_SCR-B.csv"))
valid <-  test

y_train <- as.factor(as.matrix(train[, 2])) 
y_test <- as.factor(as.matrix(test[, 2])) 

response <- "cause_id"
predictors <- target.vars

#################################################################
model <- h2o.deeplearning(
  model_id="model_P20A0_SCR-B", 
  training_frame=train, 
  validation_frame=test, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(100,100),          ## more hidden layers -> more complex interactions
  epochs=5,                      ## to keep it short enough
#  score_validation_samples=10000, ## downsample validation set for faster scoring
#  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=T,                ## manually tuned learning rate
#  rate=0.1, 
#  rate_annealing=2e-9,            
#  momentum_start=0.975,             ## manually tuned momentum
#  momentum_stable=0.98, 
#  momentum_ramp=1e+9, 
  l1=1e-2,                        ## add some L1/L2 regularization
  l2=1e-2,
  reproducible=TRUE,
  seed=1,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(model)
#h2o.saveModel(model, path="./models/P20A0_SCR-B", force = TRUE)

pred.acc.train <- function(model) {
					## Using the DNN model for predictions
					h2o_yhat_train <- h2o.predict(model, train)
					## Converting H2O format into data frame
					df_yhat_train <- as.data.frame(h2o_yhat_train)
					df_yhat_train$car_id <- as.character(train.df$car_id)
					df_yhat_train$actual <- as.character(train.df$cause_id)
					df_yhat_train$predict <- as.character(df_yhat_train$predict)
					df_yhat_train$hit <- as.integer(df_yhat_train$actual==df_yhat_train$predict)
					levels <- unique(c(as.character(df_yhat_train$predict),
					as.character(df_yhat_train$actual)))
					cf <- confusionMatrix(factor(df_yhat_train$predict, levels = levels),
					factor(df_yhat_train$actual, levels = levels))
					print(cf$overall["Accuracy"])
					print(df_yhat_train[c("car_id", "actual","predict","hit")])
					}
result_train <-pred.acc.train(model)
write.csv(result_train, "result_train_P20A0_SCR-B.csv")

pred.acc <- function(model) {
					## Using the DNN model for predictions
					h2o_yhat_test <- h2o.predict(model, test)
					## Converting H2O format into data frame
					df_yhat_test <- as.data.frame(h2o_yhat_test)
					df_yhat_test$car_id <- as.character(test.df$car_id)
					df_yhat_test$actual <- as.character(test.df$cause_id)
					df_yhat_test$predict <- as.character(df_yhat_test$predict)
					df_yhat_test$hit <- as.integer(df_yhat_test$actual==df_yhat_test$predict)
					levels <- unique(c(as.character(df_yhat_test$predict),
					as.character(df_yhat_test$actual)))
					cf <- confusionMatrix(factor(df_yhat_test$predict, levels = levels),
					factor(df_yhat_test$actual, levels = levels))
					print(cf$overall["Accuracy"])
					print(df_yhat_test[c("car_id", "actual","predict","hit")])
					}
result<-pred.acc(model)
write.csv(result, "result_P20A0_SCR-B.csv")

## Evaluate performance
yhat_train <- h2o.predict(model, train)$predict
yhat_train <- as.factor(as.matrix(yhat_train))
levels_train <- unique(c(as.character(yhat_train),
                         as.character(y_train)))

yhat_test <- h2o.predict(model, test)$predict
yhat_test <- as.factor(as.matrix(yhat_test))
levels_test <- unique(c(as.character(yhat_test),
                        as.character(y_test)))
test.df <- as.data.frame(test)


## Store Results
res[1, 1] <- round(confusionMatrix(factor(yhat_train, levels_train), factor(y_train, levels_train))$overall[1], 4)
res[1, 2] <- round(confusionMatrix(factor(yhat_test, levels_test), factor(y_test, levels_test))$overall[1], 4)
head(res)

## Save results
save(res, file = 'results_P20A0_SCR-B.rda')

#txtStop()
h2o.shutdown()


