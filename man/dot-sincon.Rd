% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISI-sincon.R
\name{.sincon}
\alias{.sincon}
\title{strength of inconsistencies}
\usage{
.sincon(mat)
}
\arguments{
\item{mat}{square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}}
}
\value{
integer, the summed strength of inconsistencies in the matrix
}
\description{
calculate strength of inconsistencies
}
\details{
helper function for \code{\link{ISI}}
}
\examples{
data(bonobos)
EloRating:::.sincon(bonobos)

}
\references{
de Vries, H. 1998. Finding a dominance order most consistent with a linear hierarchy: a new procedure and review. Animal Behaviour, 55, 827-843. (\href{https://dx.doi.org/10.1006/anbe.1997.0708}{DOI: 10.1006/anbe.1997.0708})
}
\author{
Christof Neumann
}
