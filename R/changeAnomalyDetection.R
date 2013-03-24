changeAnomalyDetection <-
function(x, term=30, smooth.n=7, order=NULL, max.score=100,...){
  if( is.null(order) ){
    best.fit <- auto.arima(x[1:term])
    order <- best.fit$arma[c(1,6,2)]
  }
  
  N <- length(x)
  outlier.score <- sapply(1:(N-term-1), function(i){
    train <- x[i:(i+term)]
    target <- x[(i+term+1)]
    fit <- NULL    
    try( fit <- arima(train, order=order,...), TRUE )

    if( is.null(fit) ){
        outlier.score <- max.score
    } else {
        pred <- KalmanForecast(n.ahead=1,fit$model)$pred
        outlier.score <- NULL
        try( outlier.score <- outlierScore(train=fit$residuals, x=(pred - target)), TRUE )
        if( is.null(outlier.score) ){
            outlier.score <- max.score
        }
    }
    outlier.score
  })

  outlier.score <- ifelse(is.null(outlier.score) | is.na(outlier.score) | is.infinite(outlier.score), max.score, outlier.score)
  outlier.score.smooth <- na.omit(scoreSmoothing(outlier.score, smooth.n))

  N2 <- length(outlier.score.smooth)
  change.score <- sapply(1:(N2-term-1), function(i){
    train <- outlier.score.smooth[i:(i+term)]
    target <- outlier.score.smooth[(i+term+1)]
    outlier.score <- NULL
    try( outlier.score <- outlierScore(train=train, x=target), TRUE )
    if( is.null(outlier.score) ){
      outlier.score <- max.score
    }
    outlier.score
  })

  change.score <- ifelse(is.null(change.score) | is.na(change.score) | is.infinite(change.score), max.score, change.score)
  change.score.smooth <- na.omit(scoreSmoothing(change.score, round(smooth.n/2)))
  c(rep(0,(N-length(change.score.smooth))), change.score.smooth)
}
