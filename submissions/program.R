program <-
function(data_train, data_test) {
  # use simple penalized model based on logistic regression
  alpha=c(0.75,0.8,0.85,0.9,0.95,1)
  lambda=c(0.001,0.005,0.01,0.05)
  idx = -(1:6)
  m = glmnet::cv.glmnet(
    x=data.matrix(data_train[!is.na(data_train[["dead_at_censor_months"]]),idx]),
    y=data_train[["dead_at_censor_months"]][!is.na(data_train[["dead_at_censor_months"]])],
    family="binomial",
    standardize=TRUE,
    lambda=lambda,
    alpha=alpha,
    lambda.min.ratio=lambda,
    nfolds= 10
  )
  data_pred = predict(m, type="class", newx=data.matrix(data_test[,idx]))
  write.table(data_pred, file = "predict", sep = "\t",
              row.names = TRUE)
  data_pred = data_pred[,1]
  
  
  return(data_pred)
}
