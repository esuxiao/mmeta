\name{summary.singletable}
\alias{summary.singletable}
\title{Summarize the objects \code{singletable}}
\description{
    Summary a model of class \code{singletable} fitted by \code{singletable}.
}
\usage{
      \method{summary}{singletable}(object,verbose=TRUE,...) 
}

\arguments{  
    \item{object}{an object inheriting from class \code{singletable}.}
   \item{verbose}{a logical value; if TRUE(default), the detailed summary messages will display.}
	 \item{...}{ additional arguments; currently none is used.}
}


\value{
  A list with the following components: posterior mean, posterior median, equal tail CI, and HDR CI.
}

\references{

Luo, S., Chen, Y., Su, X., Chu, H., (2014). mmeta: An R Package for 
Multivariate Meta-Analysis. \cr
\emph{Journal of Statistical Software}, 56(11), 1-26. \cr 
<https://dukespace.lib.duke.edu/dspace/bitstream/handle/10161/15522/2014Luo_Chen_Su_Chu_JSS_mmeta.pdf?sequence=1> \cr

Chen, Y., Luo, S., (2011a). A Few Remarks on "Statistical Distribution of the Difference of
Two Proportions' by Nadarajah and Kotz, Statistics in Medicine 2007; 26(18):3518-3523". \cr
\emph{Statistics in Medicine, 30(15)}, 1913-1915. \cr
<doi:10.1002/sim.4248> \cr

Chen, Y., Chu, H., Luo, S., Nie, L., and Chen, S. (2014a). Bayesian
analysis on meta-analysis of case-control studies accounting for
within-study correlation. \cr
\emph{Statistical Methods in Medical Research}, 4.6 (2015): 836-855. \cr
<https://doi.org/10.1177/0962280211430889>. \cr

Chen, Y., Luo, S., Chu, H., Su, X., and Nie, L. (2014b). An empirical
Bayes method for multivariate meta-analysis with an application in
clinical trials. \cr
\emph{Communication in Statistics: Theory and Methods}, 43.16 (2014): 3536-3551. \cr
<https://doi.org/10.1080/03610926.2012.700379>. \cr

Chen, Y., Luo, S., Chu, H., Wei, P. (2013). Bayesian inference on risk
differences: an application to multivariate meta-analysis of adverse
events in clinical trials. \cr
\emph{Statistics in Biopharmaceutical Research}, 5(2), 142-155. \cr
<https://doi.org/10.1080/19466315.2013.791483>. \cr

}



\seealso{\code{\link{multipletables}}}
\examples{
\donttest{
# Inference under Jeffreys prior distribution
single.OR.Jeffreys <- singletable(a1=0.5, b1=0.5, a2=0.5,
                                  b2=0.5, y1=40, n1=96, y2=49, n2=109,
                                  model="Independent",
                                  measure="OR", method="exact")
summary(single.OR.Jeffreys)

# Inference under Laplace prior distribution
single.OR.Laplace <- singletable(a1=1, b1=1, a2=1, b2=1,
                                 y1=40, n1=96, y2=49, n2=109,
                                 model="Independent", measure="OR",
                                 method="exact")
# Inference under Sarmanov prior distribution with positive correlation
single.OR.Sar1 <- singletable(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
                              rho=0.5, y1=40, n1=96, y2=49, n2=109,
                              model="Sarmanov",
                              measure="OR", method="exact")
# Inference under Sarmanov prior distribution with negative correlation
single.OR.Sar2 <- singletable(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
                              rho=-0.5, y1=40, n1=96, y2=49, n2=109,
                              model="Sarmanov",
                              measure="OR", method="exact")
# generate a 2X2 panel plot
par(mfrow=c(2,2))
plot(single.OR.Jeffreys, type="overlap", xlim=c(0.5, 2),
    main="Jefferys Prior")
plot(single.OR.Laplace, type="overlap", xlim=c(0.5, 2),
    main="Laplace Prior")
plot(single.OR.Sar1, type="overlap", xlim=c(0.5, 2),
     main=expression(paste("Sarmanov Prior ",rho," = 0.5")))
plot(single.OR.Sar2, type="overlap", xlim=c(0.5, 2),
     main=expression(paste("Sarmanov Prior ",rho," = -0.5")))
}
}

\keyword{summary}