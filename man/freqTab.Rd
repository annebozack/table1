% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freqTab.R
\name{freqTab}
\alias{freqTab}
\title{A function to create a frequency table}
\usage{
freqTab(df, vars, group = NULL)
}
\arguments{
\item{df}{dataframe}

\item{vars}{list of dichotomous variables to calculate}

\item{group}{grouping categorical variable}
}
\description{
For dichotomous variables, this function produces a table with the frequency, proportion, and N missing for each variable, and p-value for the Chi-squared test comparing groups (if group is specified)
}
\examples{
data(airquality)
airquality$tempGT70[airquality$Temp > 70] = 1
airquality$tempGT70[airquality$Temp <= 70] = 0
airquality$windGT10[airquality$Wind > 10] = 1
airquality$windGT10[airquality$Wind <= 10] = 0
freqTab(airquality, c('tempGT70', 'windGT10'), group = 'Month')
}
\keyword{descriptive}
\keyword{statistics}
