\name{maxcombo-package}
\alias{maxcombo-package}
\docType{package}
\title{
  The Group Sequential Max-Combo Test for Comparing Survival Curves
}
\description{
  This package contains functions for comparing survival curves using the max-combo test at a single timepoint or repeatedly at successive respective timepoints while controlling type I error (i.e., the group sequential setting), as published by Tom Prior (Prior 2020 Group sequential monitoring based on the maximum of weighted log-rank statistics with the Fleming-Harrington class of weights in oncology clinical trials).  The max-combo test is a generalization of the weighted log-rank test, which itself is a generalization of the log-rank test, which is a commonly used statistical test for comparing survival curves, e.g., during or after a clinical trial as part of an effort to determine if a new drug or therapy is more effective at delaying undesirable outcomes than an established drug or therapy or a placebo.
}
\author{
Tom Prior <tprior1@its.jnj.com or tomjamesprior@gmail.com>

Maintainer: Tom Prior <tprior1@its.jnj.com or tomjamesprior@gmail.com>
}
\details{
  This package contains the following four functions:
  \itemize{
    \item \code{\link{oogetdoublemaxcomboteststatistic}}
    \item \code{\link{oogetdoublemaxcombotestpvalue}}
    \item \code{\link{oogetdoublemaxcombocutoff}}
    \item \code{\link{oogetdoublemaxcombocutoffgroupsequential}}
  }
  A reasonable way to get started with using this package is to read the documentation for those functions in the order they are listed above.
}
\references{
  Prior 2020 Group sequential monitoring based on the maximum of weighted log-rank statistics with the Fleming-Harrington class of weights in oncology clinical trials.
  
  (Available at: \href{https://doi.org/10.1177/0962280220931560}{https://doi.org/10.1177/0962280220931560})
}
\keyword{ package }
