\name{EloRating-package}
\alias{EloRating-package}
\alias{EloRating}
\docType{package}
\title{
\packageTitle{EloRating}
}
\description{
\packageDescription{EloRating}
}
\author{
\packageAuthor{EloRating}

Maintainer: \packageMaintainer{EloRating}
}
\references{
Elo, A. E. 1978. The Rating of Chess Players, Past and Present. New York: Arco.

Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495. (\href{https://dx.doi.org/10.1006/anbe.2000.1571}{DOI: 10.1006/anbe.2000.1571})

Neumann, C., Duboscq, J., Dubuc, C., Ginting, A., Irwan, A. M., Agil, M., Widdig, A. & Engelhardt, A. 2011. Assessing dominance hierarchies: validation and advantages of progressive evaluation with Elo-rating. Animal Behaviour, 82, 911-921. (\href{https://dx.doi.org/10.1016/j.anbehav.2011.07.016}{DOI: 10.1016/j.anbehav.2011.07.016})
}
\keyword{ package }

\examples{
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
summary(SEQ)
}
