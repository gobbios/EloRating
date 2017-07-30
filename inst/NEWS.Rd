\name{NEWS}
\title{News for Package \pkg{EloRating2}}


\section{Changes in version 0.45.01 (2017-07-30)}{
	\itemize{
	  merged \code{\link{elo.seq.customstart}} into \code{\link{elo.seq}} (the previous version is available via \code{EloRating:::.elo.seq_old})
	}
}


\section{Changes in version 0.45-0 (2017-07-29)}{
	\itemize{
	  \item added dependency to \code{\link[Rcpp]{Rcpp-package}}, which in turn means that the package needs development tools available if it is installed from GitHub (or from source)
		\item added \code{\link{elo.seq.customstart}} which allows incorporating prior knowledge of ordinal ranks and different k-values
		\item added \code{\link{createstartvalues}} which translates known ranks into starting Elo-ratings
		\item added \code{\link{mat2seq}} which transforms a matrix into a sequence
		\item added \code{\link{steepness}} which calculates hierarchy steepness based on David's scores
		\item added some more published data sets (\code{\link{baboons}})
	}
}



\section{Changes in version 0.0.0.9914 (2017-07-26)}{
	\itemize{
		\item added \code{\link{transitivity}} function to calculate triangle transitivity
		\item \code{\link{creatematrix}} now works also with vectors of winners and losers
		\item added \code{\link{ISIranks}} function to directly return ranks of ISI ranking(s)
		\item added some more published data sets (\code{\link{dommats}})
	}
}

\section{Changes in version 0.0.0.9913 (2017-07-16)}{
	\itemize{
		\item added unit tests with testthat
	}
}

\section{Changes in version 0.0.0.9910 (2017-07-15)}{
	\itemize{
		\item start transfer to GitHub
	}
}
