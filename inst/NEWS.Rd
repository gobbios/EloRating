\name{NEWS}
\title{News for Package \pkg{EloRating}}

\section{Changes in version 0.46.11 (2020-03-12)}{
	\itemize{
	  \item fixes bug that set tied relationships to zero relationships in the output of \code{\link{ISI}}
	  \item minor fix in \code{\link{randomelo}} for compatibility with R 4.0.0
	  \item new function \code{\link{coresidence}}
	  \item new function \code{\link{correctly_predicted}}
	  \item new function \code{\link{dyadic_dom}}
	  \item new function \code{\link{simple_dom}}
	  \item new function \code{\link{CBI}}
	}
}

\section{Changes in version 0.46.10 (2019-12-05)}{
	\itemize{
	  \item fixes issues related to changes in the result of \code{class(x) == "matrix"} that will become problematic in R 4.0.0
	}
}

\section{Changes in version 0.46.9 (2019-12-04)}{
	\itemize{
	  \item support for burnin in \code{\link{likelo}}, \code{\link{optimizek}} and \code{\link{optistart}}
	  \item modified DESCRIPTION
	}
}

\section{Changes in version 0.46.8 (2019-06-14)}{
	\itemize{
	  \item conform to CRAN submission rules (DESCRIPTION file)
	  \item modified examples
	}
}

\section{Changes in version 0.46.6 (2019-06-13)}{
	\itemize{
	  \item conform to new CRAN rules (put makevars back)
	  \item updated tutorial and new tests
	}
}

\section{Changes in version 0.46.5 (2019-02-11)}{
	\itemize{
	  \item updated tutorial
	}
}

\section{Changes in version 0.46.4 (2018-08-30)}{
	\itemize{
	  \item added reference management with \code{Rdpack}
	  \item added multiple functions to optimize k parameter (\code{\link{fastelo}}, \code{\link{likelo}}, \code{\link{optimizek}})
	}
}

\section{Changes in version 0.46.3 (2018-07-13)}{
	\itemize{
	  \item added possibility to \code{\link{extract_elo}} to input vectors of dates and IDs
	}
}

\section{Changes in version 0.46.2 (2018-02-17)}{
	\itemize{
	  \item removed makevars
	}
}

\section{Changes in version 0.45.03 (2017-07-30)}{
	\itemize{
	  \item added new vignette (tutorial) based on R markdown
    \item minor fixes related to package renaming
	}
}

\section{Changes in version 0.45.02 (2017-07-30)}{
	\itemize{
	  \item renamed the package back to \code{EloRating}
	  \item removed vignette to avoid conflicts
	}
}

\section{Changes in version 0.45.01 (2017-07-30)}{
	\itemize{
	  \item merged \code{\link{elo.seq.customstart}} into \code{\link{elo.seq}} (the previous version is available via \code{EloRating:::.elo.seq_old})
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
