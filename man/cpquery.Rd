\name{cpquery}
\alias{cpquery}
\alias{cpdist}
\alias{mutilated}
\title{ Perform conditional probability queries }
\description{

  Perform conditional probability queries (CPQs).

}
\usage{
cpquery(fitted, event, evidence, cluster = NULL, method = "ls", ...,
  debug = FALSE)
cpdist(fitted, nodes, evidence, cluster = NULL, method = "ls", ...,
  debug = FALSE)

mutilated(x, evidence)
}
\arguments{
  \item{fitted}{an object of class \code{bn.fit}.}
  \item{x}{an object of class \code{bn} or \code{bn.fit}.}
  \item{event, evidence}{a list, a data.frame, an unevaluated expression
  or TRUE. See Details and Examples below.}
  \item{nodes}{a vector of character strings, the labels of the nodes whose
    conditional distribution we are interested in.}
  \item{cluster}{an optional cluster object from package \pkg{snow}.
      See \code{\link{snow integration}} for details and a simple
      example.}
  \item{method}{a character string, the method used to perform the conditional
    probability query. Currently only \emph{logic sampling} (\code{ls}, the
    default) and \emph{likelihood weighting} (\code{lw}) are implemented.}
  \item{\dots}{additional tuning parameters.}
  \item{debug}{a boolean value. If \code{TRUE} a lot of debugging output
       is printed; otherwise the function is completely silent.}
}
\details{

  \code{cpquery} estimates the conditional probability of \code{event} given
  \code{evidence} using the method specified in the \code{method} argument.

  \code{cpdist} generates random observations conditional on the
  \code{evidence} using the method specified in the \code{method} argument.

  \code{mutilated} constructs the mutilated network used for sampling
    in \emph{likelihood weighting}.

}
\section{Logic Sampling}{

  When \code{event} or \code{evidence} are of class lists, names should
  correspond to nodes in the \code{fitted} object and each component
  will be treated as \code{or} expression. For example an event like
  \code{list(A=c("a1", "a2"), B="c")} is equivalent to
  \code{(A=="a1"||A=="a2") & (B=="c")}. If either \code{event} or
  \code{evidence} is a data.frames, it will be treated as a list.

  If \code{event} and \code{evidence} arguments are unevaluated
  expressions then they should describe the event of interest and the
  conditioning evidence such that, if we denote with \code{data} the
  data set the network was learned from, \code{data[eval(evidence,
  data), ]} and \code{data[eval(event, data), ]} return the correct
  observations.

  If either \code{event} or \code{evidence} is set to
  \code{TRUE} an unconditional probability query is performed with
  respect to that argument.


  Three tuning parameters are available:

  \itemize{

    \item \code{n}: a positive integer number, the number of random observations
      to generate from \code{fitted}. Defaults to \code{5000 * nparams(fitted)}.

    \item \code{batch}: a positive integer number, the size of each batch
      of random observations. Defaults to \code{10^4}.

    \item \code{query.nodes}: a a vector of character strings, the labels of
      the nodes involved in \code{event} and \code{evidence}. Simple queries
      do not require to generate observations from all the nodes in the
      network, so \code{cpquery} and \code{cpdist} try to identify which
      nodes are used in \code{event} and \code{evidence} and reduce the
      network to their upper closure. \code{query.nodes} may be used to
      manually specify these nodes when automatic identification fails.

  }

  Note that the number of observations returned by \code{cpdist} is always
  smaller than \code{n}, because logic sampling is a form of rejection
  sampling. Therefore, only the obervations matching \code{evidence} (out
  of the \code{n} that are generated) are returned, and their number depends
  on the probability of \code{evidence}.

}
\section{Likelihood Weighting}{

  The \code{event} argument must be an expression describing the event of
  interest, as in logic sampling. The \code{evidence} argument must be a
  named list; each element corresponds to one node in the network and must
  contain the value that node will be set to when sampling.
  If either \code{event} or \code{evidence} is set to \code{TRUE} an
  unconditional probability query is performed with respect to that argument.

  Tuning parameters are the same as for logic sampling: \code{n}, \code{batch}
  and \code{query.nodes}.

}
\value{

  \code{cpquery} returns a numeric value, the conditional probability of \code{event}
  conditional on \code{evidence}.

  \code{cpudist} returns a data frame containing the observations generated from
  the conditional distribution of the \code{nodes} conditional on \code{evidence}.

  \code{mutilated} returns a \code{bn} or \code{bn.fit} object, depending on the
  class of \code{x}.

}
\references{

  Koller D, Friedman N (2009). \emph{Probabilistic Graphical Models:
      Principles and Techniques}. MIT Press.

  Korb K, Nicholson AE (2010). \emph{Bayesian Artificial Intelligence}.
      Chapman & Hall/CRC, 2nd edition.

}
\examples{

## discrete Bayesian network (it's the same with ordinal nodes).
data(learning.test)
bn <- hc(learning.test)
plot(bn)
fitted <- bn.fit(bn, learning.test)

## the result should be around 0.025.
cpquery(fitted, expression((B == "b")), expression((A == "a")))
## for a single observation, predict the value of a single
## variable conditional on the others:
ev <- learning.test[obs, -4]
cpquery(fitted, list(D = "b"), ev)
cpquery(fitted, list(D = "c"), ev)
cpquery(fitted, list(D = c("b", "c")), ev)
cpquery(fitted, list(D = "b", D = "c"), ev) # 0

## do the same with logical sampling:
cpquery(fitted, event = quote(C=='c'),  evidence = learning.test[2, -3], method = "ls")
## do the same with likelihood weighting:
cpquery(fitted, event = quote(C=='c'),  evidence = learning.test[2, -3], method = "lw")

## conditional distribution of A given C == "c".
prop.table(table(cpdist(fitted, "A", list(C = "c"), method = "lw")))

## Gaussian Bayesian network.
data(gaussian.test)
fitted = bn.fit(hc(gaussian.test), gaussian.test)
## the result should be around 0.04.
cpquery(fitted,
        event = expression(((A >= 0) & (A <= 1)) & ((B >= 0) & (B <= 3))),
        evidence = expression((C + D < 10)))

}
\author{ Marco Scutari }
\keyword{multivariate}
