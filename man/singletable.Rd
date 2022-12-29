\name{singletable}
\alias{singletable}
\title{Exact posterior inference based on a single 2x2 table }
\description{
  This function conducts exact posterior inference based on a single 2x2
  table.
  
}
\usage{
   singletable(y1=y1,n1=n1,y2=y2,n2=n2,measure=measure,model="Sarmanov",
                        method="exact",a1=0.5,b1=0.5,a2=0.5,b2=0.5,rho=0,alpha=0.05,
                        nsam=10000)
}

\arguments{
   \item{y1}{an integer indicating the number of events in group 1}
   \item{n1}{an integer indicating the total number of subjects in group 1}
   \item{y2}{an integer indicating the number of events in group 2}
   \item{n2}{an integer indicating the total number of subjects in group
     2}
   \item{measure}{a character string specifying a measure. Options are
     \code{OR}, \code{RR}, and \code{RD}. \code{OR} is odds
     ratio, \code{RR} is relative risk, and \code{RD} is risk difference.}
   \item{model}{a character string specifying the model. Options are
     \code{Independent} and \code{Sarmanov}. \code{Independent} is
     independent beta-binomial model. \code{Sarmanov}is Sarmanov
     beta-binomial model.}
    \item{method}{a character string specifying the method. Options are
      \code{exact} and \code{sampling}. \code{exact} (default) is a
      method based on Monte Carlo sampling. \code{exact} is exact method.}
   \item{a1}{a numeric value specifying the first hyperparameter of
     the beta prior for group 1. Default value set to 0.5}
   \item{b1}{a numeric value specifying the second hyperparameter of
     the beta prior for group 1. Default value set to 0.5}
   \item{a2}{a numeric value specifying the first hyperparameter of
     the beta prior for group 2. Default value set to 0.5}
   \item{b2}{a numeric value specifying the second hyperparameter of
     the beta prior for group 2. Default value set to 0.5}
   \item{rho}{a numeric value specifying correlation coefficient for
     Sarmanov bivariate prior distribution. Default value set to 0. It
   is subject to constraints. See Details.}
   \item{alpha}{a numeric value specifying the significant level. Default value sets to 0.05.}
   \item{nsam}{a numeric value specifying the number of samples if method is "sampling". Default value sets to \code{10000}} 
   
 }

\details{There are two kinds of study design, i.e., prospective study or
  clinical trial, and retrospective or case-control study. 
       In a prospective study or clinical trial, \code{data} is a data
       frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2},
       \code{studynames}. \code{y1} is the number of subjects
       experienced a certain event in the unexposed group. \code{n1} is the number
       of subjects in the unexposed group. \code{y2} is the number of subjects experienced
       a certain event in the exposed group. \code{n2} is the number of
       subjects in the exposed group. In this study, \code{OR} is odds ratio
       of event comparing exposed group with unexposed group. \code{RR}
       is relative risk of event comparing exposed group with unexposed group. \code{RD} is risk
       difference of event comparing exposed group with unexposed group.
       
       For case-control study, \code{y1} is the number of subjects with
       exposure in the control group. \code{n1} is the number of
       subjects in the control group. \code{y2} is the number of
       subjects with exposure in the case group. \code{n2} is the
       number of subjects in the case group. In this study, \code{OR} is odds ratio
       of event comparing case group with control group. \code{RR} is
       relative risk of event comparing case group with control group. \code{RD} is risk
       difference of event comparing case group with control group.

       When \code{model="Sarmanov"}, \code{rho} is subject to constraints. See Chen et al (2011) for
       details.}
 

\value{
 An object is returned, inheriting from class \code{singletable}. 
 Objects of this class have methods for the generic functions \code{summary} and \code{plot}.
 The following components must be included in a legitimate \code{singletable} object.

  \item{measure}{the value of \code{measure} argument.}
  \item{model}{the value of \code{model} argument.}
  \item{method}{the value of \code{method} argument.}
  \item{dataset}{a numeric vector of input data with components:
    \code{y1}, \code{n1}, \code{y2}, \code{n2}}  
  \item{parameter}{a numeric vector of the hyperparameters: 
    \code{a1}, \code{b1}, \code{a2}, \code{b2}, and \code{rho}.}    
  \item{alpha}{the value of \code{alpha} argument.} 
  \item{sample}{a list of samples for the posterior and prior distributions}
  \item{density}{a list of the density of the posterior and prior distributions}
  \item{studynames}{a character vector specifying the study names.} 
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




\seealso{\code{\link{multipletables}}
\code{\link{plot.singletable}}
\code{\link{summary.singletable}}
}

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
summary(single.OR.Laplace)

# Inference under Sarmanov prior distribution with positive correlation
single.OR.Sar1 <- singletable(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
                              rho=0.5, y1=40, n1=96, y2=49, n2=109,
                              model="Sarmanov",
                              measure="OR", method="exact")
summary(single.OR.Sar1)

# Inference under Sarmanov prior distribution with negative correlation
single.OR.Sar2 <- singletable(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
                              rho=-0.5, y1=40, n1=96, y2=49, n2=109,
                              model="Sarmanov",
                              measure="OR", method="exact")
summary(single.OR.Sar2)

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

\keyword{singletable}
