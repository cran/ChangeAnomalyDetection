\name{changeAnomalyDetection}
\alias{changeAnomalyDetection}
\title{
Anomaly Detection of change
}
\description{
Calculate the score of the degree of behavior change in time series.
}
\usage{
changeAnomalyDetection(x, term = 30, smooth.n = 7, order = NULL, max.score=100,...)
}
\arguments{
  \item{x}{
  Time-series numeric data
}
  \item{term}{
  training period.
}
  \item{smooth.n}{
  smoothing period.
}
  \item{order}{
  order parameter for arima function.
}
  \item{max.score}{
  failure score model building
}
  \item{\dots}{
  parameter for arima function.
}
}
\value{
anomaly description score of change
}
\author{
Yohei Sato
}

\examples{
data(ttrc)
x <- ttrc[1:100,"Close"]

change.score <- changeAnomalyDetection(x, term=10)
}


