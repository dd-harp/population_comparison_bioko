% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jitter.R
\name{summarize_over_shifts}
\alias{summarize_over_shifts}
\title{Loops over the given grid shifts and runs the summarizer on each shift.}
\usage{
summarize_over_shifts(shift_cnt, summarizer)
}
\arguments{
\item{shift_cnt}{The number of shifts in x and y, so the loop length is shift_cnt * shift_cnt.}

\item{summarizer}{A function that takes the current shift fraction and return a summary.}
}
\value{
a data.frame with the results
}
\description{
The problem is that the inner loop can be slow, so we separate the
loop logic for testing that it can shift properly and accumulate results.
}
