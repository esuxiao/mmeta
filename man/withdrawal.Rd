\name{withdrawal}
\alias{withdrawal}
\alias{withdrawal}

\docType{data}

\title{Studies on the association of withdrawal from study due to
  adverse events and tricyclic treatment}

\description{Results from 16 clinical trials investigating the association of withdrawal from study due to adverse events and tricyclic treatment}

\format{
  The data frame contains the following columns:
  \describe{
    \item{y1}{number of subjects withdrew due to adverse events in the placebo group}
    \item{n1}{number of subjects in the placebo group}
    \item{y2}{number of subjects withdrew due to adverse events in the tricyclic treatment group}
    \item{n2}{number of subjects in the tricyclic treatment group}
    \item{studynames}{The study names indicating the last name of
    the first author and the year of each study}
  }
}

\references{
Jackson, J. L., Shimeall, W., Sessums, L., DeZee, K. J., Becher, D.,
Diemer, M., Berbano, E., OMalley, P. G. (2010) Tricyclic
antidepressants and headaches: systematic review and meta-analysis. BMJ,
341, C5222-c5234. 
}


\note{
The dataset \code{withdrawal} is used to conduct exact posterior
inference of relative risks and risk difference for multiple 2X2 tables. 
}



\seealso{
  \code{\link{multipletables}},
    \code{\link{summary.multipletables}},
    \code{\link{plot.multipletables}},
}

\examples{
library(mmeta)
data(withdrawal)
summary(withdrawal)
}

\keyword{datasets}
