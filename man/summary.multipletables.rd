\name{summary.multipletables}
\alias{summary.multipletables}
\title{Summary a specific study of objects \code{multipletables}}
\description{
    Summary a model of class \code{multipletables} fitted by \code{multipletables}.
}
\usage{
      \method{summary}{multipletables}(object,...) 
}

\arguments{
  \item{object}{an object inheriting from class \code{multipletables}.}
  \item{...}{ additional arguments; currently none is used.}
}


 \value{
   A list with the following components:
  \item{model}{the value of \code{model} argument.}
  \item{measure}{the value of \code{measure} argument.}
  \item{cov.matrix}{the estimated covariance matrix of the estimated
    parameters in the transformed scales}
  \item{hessian}{the estimated hessian matrix of the estimated
    parameters in the transformed scales}	  
  \item{overall}{a list of two components that contain the overall
    measure (e.g., overall OR) and its 95\% equal-tail credible interval.}
   \item{studynames}{a character string indicating all the study names}
   \item{chi2}{the chi-square test statistics of the likelihood ratio
     test}
   \item{pvalue}{the p-value of the likelihood ratio test}
   \item{alpha}{the value of \code{alpha} argument.}
   \item{MLE}{a numeric vector of the estimated hyperparameters in the
     following order: \code{a1}, \code{b1}, \code{a2}, \code{b2},
     \code{rho}.}
   \item{studyspecific}{a Numeric matrix with columns being the
     posterior means, the lower bound, and the upper bound of the
     credible/confidence intervals of study-specific and overall measure.}
}


\references{

Luo, S., Chen, Y., Su, X., Chu, H., (2014). mmeta: An R Package for
Multivariate Meta-Analysis. Journal of Statistical Software, 56(11), 1-26. 

Chen, Y., Luo, S., (2011a). A Few Remarks on "Statistical Distribution of the Difference of
Two Proportions' by Nadarajah and Kotz, Statistics in Medicine 2007; 26(18):3518-3523" .
Statistics in Medicine, 30(15), 1913-1915. 

Chen, Y., Chu, H., Luo, S., Nie, L., and Chen, S. (2014a). Bayesian
analysis on meta-analysis of case-control studies accounting for
within-study correlation. Statistical Methods in Medical Research,
doi: 10.1177/0962280211430889. In press. 


Chen, Y., Luo, S., Chu, H., Su, X., and Nie, L. (2014b). An empirical
Bayes method for multivariate meta-analysis with an application in
clinical trials. Communication in Statistics: Theory and Methods. In press. 

Chen, Y., Luo, S., Chu, H., Wei, P. (2013). Bayesian inference on risk
differences: an application to multivariate meta-analysis of adverse
events in clinical trials. Statistics in Biopharmaceutical Research, 5(2), 142-155.


}



\seealso{\code{\link{multipletables}}
         \code{\link{plot.multipletables}}
       }


\examples{
#library(mmeta)

# Analyze the dataset colorectal to conduct exact inference of the odds ratios
#data(colorectal)
#multiple.OR <- multipletables(data=colorectal, measure="OR", model="Sarmanov", method="exact")
# Generate the forest plot with 95\% CIs of study-specific odds ratios
#and 95\% CI of overall odds ratio
#plot(multiple.OR, type="forest", addline=1)
# Plot the posterior density functions of some target studies in an overlaying manner
#plot(multiple.OR, type="overlap", select=c(4,14,16,20))
# Plot the posterior density functions of some target studies in a
#side-by-side manner 
#plot(multiple.OR, type="sidebyside", select=c(4,14,16,20), ylim=c(0,2.7), xlim=c(0.5,1.5))


# Analyze the dataset withdrawal to conduct inference of the relative risks
#data(withdrawal)
#multiple.RR <- multipletables(data=withdrawal, measure="RR",model="Sarmanov")
#plot(multiple.RR, type="forest", addline=1)
#plot(multiple.RR, type="overlap", select=c(3,8,14,16))
#plot(multiple.RR, type="sidebyside", select=c(3,8,14,16), ylim=c(0,1.2),
#xlim=c(0.4,3))

# Analyze the dataset withdrawal to conduct inference of the risk differences
#data(withdrawal)
#multiple.RD <- multipletables(data=withdrawal, measure="RD",
#                              model="Sarmanov")
#summary(multiple.RD)
#plot(multiple.RD, type="forest", addline=0)
#plot(multiple.RD, type="overlap", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16),
#     ylim=c(0,6), xlim=c(-0.2,0.4))
}


\keyword{methods}