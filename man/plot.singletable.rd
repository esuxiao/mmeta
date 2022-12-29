\name{plot.singletable}
\alias{plot.singletable}
\title{Plot Method for \code{singletable} objects}
\description{
   Produces various plots for single table analysis.
}
\usage{
    \method{plot}{singletable}(x,type=type,select=c(1,2)
	,xlab=NULL,ylab=NULL,addline=NULL,xlim=NULL,ylim=NULL,...)
}

\arguments{  
   \item{x}{an object inheriting from class \code{singletable}.}
   \item{type}{a chracter string specifying the type of plots to
     produce. Options are \code{sidebyside} and \code{overlap}. See details}
   \item{select}{a numeric value or vector specifying which distribution
     should be plotted. \code{select=1} is posterior distribution. \code{select=2} is
     prior distribution. \code{select=c(1,2)} is both. Default is
     c(1,2). This argument is only used when \code{type="sidebyside"}.}
   \item{xlab}{a character string specifying the x-axis label in the
     plot. Default is the name of the measure of association}
   \item{ylab}{a character string specifying the x-axis label in the plot. Default is "Density"}
   \item{addline}{a numeric value specifying the x-value for a vertical
     reference line at \code{x=addline}. Default is NULL}
   \item{xlim, ylim}{a numeric vectors of length 2 specifying the lower
     and upper limits of the axes}
   \item{...}{Other arguments can be passed to plot function} 
 }
 
\value{No return value, called for side effects} 
 
\details{   
     If \code{type="sidebyside"}, the posterior distribution of measure
     and the prior distribution are drawn side by side in two plots. If
     \code{type="overlap"}, the posterior distribution of measure and
     the prior distribution are overlaid in one plot. 
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

\seealso{\code{\link{singletable}}}
\examples{
\donttest{
oldpar <- par(no.readonly = TRUE)

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
     
par(oldpar)
}
}

\keyword{singletable}