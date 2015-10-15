XGB.Regression.Config <- setClass("XGB.Regression.Config", slots = c(nrounds="numeric", max.depth="numeric", eta="numeric", subsample="numeric")
  , validity = function(object) {
    if (object@nrounds==round(object@nrounds) && object@nrounds>0 && 
          object@max.depth==round(object@max.depth) && object@max.depth>0 && 
          object@eta>0 && object@subsample>=0 && object@subsample<=1.0) TRUE
    else "invalid parameters"
  }
  , contains = "Regression.Config"
)

XGB.Regression.FitObj <- setClass("XGB.Regression.FitObj", contains = "Regression.FitObj")

make.configs.xgb.regression <- function(df=expand.grid(nrounds=c(100,500,1000,2000),max.depth=c(3,4),eta=c(0.001,0.01,0.1,0.5), subsample=0.5)) {
  ret <- lapply(1:nrow(df), function(i) {
    GBM.Regression.Config(nroundss=df$nroundss[i], max.depth=max.depth[i], eta=eta[i], subsample=df$subsample[i])
  })
}

setMethod("BaseLearner.Fit", "XGB.Regression.Config",
  function(object, formula, data, tmpfile=NULL, print.level=1) {
    y <- data[,all.vars(formula)[1]]
    est <- xgboost(formula, objective="binary:logistic", data=data, nrounds=object@nrounds, max.depth=object@max.depth
               , subsample=object@subsample, eta=object@eta, verbose=print.level>=1)
    pred <- predict(est, newdata=data)
    if (!is.null(tmpfile)) save(est, file=tmpfile, compress=FALSE)
    ret <- XGB.Regression.FitObj(config=object
      , est=if (is.null(tmpfile)) est else tmpfile
      , pred=pred)
    return (ret)
  }
)

predict.XGB.Regression.FitObj <- function(object, newdata=NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  if (is.character(object@est)) object@est <- load.object(object@est)
  newpred <- predict(object@est, newdata=newdata)
  return (newpred)
}
