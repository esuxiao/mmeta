\name{multipletables}
\alias{multipletables}
\title{Exact posterior inference based on multiple 2x2 tables}
\description{
  This function conducts exact posterior inference based on multiple 2x2
  tables. 
}
\usage{
  multipletables(data=NULL, measure=NULL, model="Sarmanov",
                           method="sampling", nsam=10000, alpha=0.05)    
}

\arguments{
   \item{data}{a data frame that contains \code{y1}, \code{n1},
     \code{y2}, \code{n2}, \code{studynames}. See details}
   \item{measure}{a character string specifying a measure. Options are
     \code{OR}, \code{RR}, and \code{RD}. \code{OR} is odds
     ratio, \code{RR} is relative risk, and \code{RD} is risk difference.}
   \item{model}{a character string specifying the model. Options are
     \code{Independent} and \code{Sarmanov}. \code{Independent} is
     independent beta-binomial model. \code{Sarmanov}is Sarmanov
     beta-binomial model.}
    \item{method}{a character string specifying the method. Options are
      \code{exact} and \code{sampling}. \code{sampling} (default) is a method based on
      Monte Carlo sampling. \code{exact} is exact method.}
   \item{alpha}{a numeric value specifying the significant level. Default value sets to 0.05.}
   \item{nsam}{a numeric value specifying the number of samples if method is \code{sampling}. Default value sets to \code{10000}}  
 }

\details{There are two kinds of study design, i.e., prospective study or
  clinical trial, and retrospective or case-control study. In a
  prospective study or clinical trial, \code{data} is a data frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2},
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

       Empirical Bayes method is used to maximize the marginal likelihood
       combining all studies to obtained the estimates of the
       hyperparameters a1, b1, a2, b2, and rho. When
       \code{method="independent"}, only the estimated hyperparameters
       of a1, b1, a2, and b2 are used. When \code{model="Sarmanov"},
       \code{rho} is subject to constraints. See Chen et al (2011) for
       details.

       The output \code{cov.matrix} and \code{hessian} are the estimated
       covariance matrix and hessian matrix of the estimated
       parameters in the transformed scales. The estimated parameters
       are log(a1), log(b1), log(a2), log(b2), omega, where the
       correlation coefficient rho is a function of a1, b1, a2, b2, and
       omega. Please see details on page 7 of Chen et al (2012 b).
     }

\value{
 An object is returned, inheriting from class \code{multipletables}. 
 Objects of this class have methods for the generic functions \code{summary} and \code{plot}.
 The following components must be included in a legitimate \code{multipletables} object.

  \item{measure}{the value of \code{measure} argument.}
  \item{model}{the value of \code{model} argument.}
    \item{method}{the value of \code{method} argument.}
   \item{dataset}{a data matrix with rows being \code{y1}, \code{n1},
     \code{y2}, and \code{n2}.}
   \item{studynames}{a character string indicating all the study names}
   \item{measurename}{a character string specifying the full names of
     value of \code{measure} argument. Can be \code{Odds Ratio},
     \code{Relative Risk}, and \code{Risk Difference}.}
   \item{alpha}{the value of \code{alpha} argument.}
   \item{chi2}{the chi-square test statistics of the likelihood ratio test}
   \item{pvalue}{the p-value of the likelihood ratio test}
   \item{MLE}{a numeric vector of the estimated hyperparameters in the
     following order: \code{a1}, \code{b1}, \code{a2}, \code{b2},
     \code{rho}.}
  \item{cov.matrix}{the estimated covariance matrix of the estimated
    parameters in the transformed scales}
  \item{hessian}{the estimated hessian matrix of the estimated
    parameters in the transformed scales}	  
  \item{overall}{a list of two components that contain the overall
    measure (e.g., overall OR) and its 95\% equal-tail credible interval.}
  \item{sample}{a list of length the number of studies with components
    numerical vectors of the samples of the each study-specific
    measure. }
  \item{density}{a list of length the number of studies with components
    lists of density of each study-specific measure. }
   
  \item{dataset}{a numeric vector of input data with components:
    \code{y1}, \code{n1}, \code{y2}, \code{n2}}  
  \item{parameter}{a numeric vector specifying the hyperparameters with components
    \code{a1}, \code{b1}, \code{a2}, \code{b2}, and \code{rho}.}    
  \item{alpha}{a numeric value specifying the significant level. Default value sets to 0.05.} 
  \item{sample}{a list of samples for the posterior and prior distributions}
  \item{density}{a list of the density of the posterior and prior distributions}
  \item{studynames}{a character vector being "Posterior" and "Prior".} 
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



\seealso{\code{\link{plot.multipletables}}
         \code{\link{summary.multipletables}}
       }
	
\examples{
#library(mmeta)
#Analyze the dataset colorectal to conduct exact inference of the odds ratios
#data(colorectal)
#multiple.OR <- multipletables(data=colorectal, measure="OR",
# model="Sarmanov", method="exact")
#summary(multiple.OR)
# Generate the forest plot with 95\% CIs of study-specific odds ratios
#and 95\% CI of overall odds ratio
#plot(multiple.OR, type="forest", addline=1,file="forestOR")
# Plot the posterior density functions of some target studies in an overlaying manner
#plot(multiple.OR, type="overlap", select=c(4,14,16,20),file="overlapOR")
# Plot the posterior density functions of some target studies in a
#side-by-side manner 
#plot(multiple.OR, type="sidebyside", select=c(4,14,16,20), ylim=c(0,2.7),
# xlim=c(0.5,1.5),file="sidebysideOR")
#print(multiple.OR.table)
#print(multiple.OR.table, type="html")

# Analyze the dataset withdrawal to conduct inference of the relative risks
#data(withdrawal)
#multiple.RR <- multipletables(data=withdrawal, measure="RR",
#                              model="Sarmanov")
#summary(multiple.RR)
#plot(multiple.RR, type="forest", addline=1)
#plot(multiple.RR, type="overlap", select=c(3,8,14,16))
#plot(multiple.RR, type="sidebyside", select=c(3,8,14,16), 
#ylim=c(0,1.2), xlim=c(0.4,3))
#print(multiple.RR.table)
#print(multiple.RR.table, type="html")

# Analyze the dataset withdrawal to conduct inference of the risk differences
#data(withdrawal)
#multiple.RD <- multipletables(data=withdrawal, measure="RD",
#                             model="Sarmanov")
#summary(multiple.RD)
#plot(multiple.RD, type="forest", addline=0)
#plot(multiple.RD, type="overlap", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16),
#     ylim=c(0,6), xlim=c(-0.2,0.4))
#print(multiple.RD.table)
#print(multiple.RD.table, type="html")
}      
\keyword{multipletables}