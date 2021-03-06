% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{Dynamic knapsack}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{A data.frame cx with two variables v and w}

\item{W}{The weight capacity of the knapsack}
}
\value{
The maximum knapsack value and corresponding elements that contributes to the value.
}
\description{
A function that solve the knapsack problem using dynamic programming. If the weights 
are actually discrete values, knapsack_dynamic search is used to solve the knapsack problem but 
scaling much better over all possible values of w.
}
\examples{
knapsack_objects <- knapsack_data_gen(n = 2000, seed = 42)
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
}
\seealso{
see also similar functions:
\code{\link{brute_force_knapsack}}
\code{\link{greedy_knapsack}}
}

