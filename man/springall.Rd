% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/springall.R
\docType{data}
\name{springall}
\alias{springall}
\title{Springall (1973) Data on Subjective Evaluation of Flavour Strength}
\format{
A list containing two data frames, \code{springall$contests} and
\code{springall$predictors}.

The \code{springall$contests} data frame has 36 observations (one for each
possible pairwise comparison of the 9 treatments) on the following 7
variables: \describe{
\item{row}{a factor with levels \code{1:9},
the row number in Springall's dataset} #
\item{col}{a factor with
levels \code{1:9}, the column number in Springall's dataset}
\item{win}{integer, the number of wins for column treatment over row
treatment}
\item{loss}{integer, the number of wins for row treatment
over column treatment}
\item{tie}{integer, the number of ties
between row and column treatments}
\item{win.adj}{numeric, equal to
\code{win + tie/2}}
\item{loss.adj}{numeric, equal to \code{loss + tie/2}} }

The \code{predictors} data frame has 9 observations (one for each treatment)
on the following 5 variables: \describe{
\item{flav}{numeric, the
flavour concentration}
\item{gel}{numeric, the gel concentration}
\item{flav.2}{numeric, equal to \code{flav^2}}
\item{gel.2}{numeric, equal to \code{gel^2}}
\item{flav.gel}{numeric, equal to \code{flav * gel}} }
}
\source{
Springall, A (1973) Response surface fitting using a generalization
of the Bradley-Terry paired comparison method.  \emph{Applied Statistics}
\strong{22}, 59--68.
}
\usage{
springall
}
\description{
Data from Section 7 of the paper by Springall (1973) on Bradley-Terry
response surface modelling.  An experiment to assess the effects of gel and
flavour concentrations on the subjective assessment of flavour strength by
pair comparisons.
}
\details{
The variables \code{win.adj} and \code{loss.adj} are provided in order to
allow a simple way of handling ties (in which a tie counts as half a win and
half a loss), which is slightly different numerically from the Rao and
Kupper (1967) model that Springall (1973) uses.
}
\examples{

##
## Fit the same response-surface model as in section 7 of 
## Springall (1973).
##
## Differences from Springall's fit are minor, arising from the 
## different treatment of ties.
##
## Springall's model in the paper does not include the random effect.  
## In this instance, however, that makes no difference: the random-effect 
## variance is estimated as zero.
##
summary(springall.model <- BTm(cbind(win.adj, loss.adj), col, row, 
                               ~ flav[..] + gel[..] + 
                                 flav.2[..] + gel.2[..] + flav.gel[..] +
                                 (1 | ..),
                               data = springall))

}
\references{
Rao, P. V. and Kupper, L. L. (1967) Ties in paired-comparison
experiments: a generalization of the Bradley-Terry model.  \emph{Journal of
the American Statistical Association}, \strong{63}, 194--204.
}
\author{
David Firth
}
\keyword{datasets}
