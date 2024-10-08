% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{select_door}
\alias{select_door}
\title{Contestant selects a door in the Monty Hall Problem game.}
\usage{
select_door()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
The function returns a number indicating the door that the contestant
has chosen. What is behind the door is not indicated, although the person
running the game can determine that from the character vector returned by
the previous function.
}
\description{
\code{select_door} allows the contestant to select one of the three doors
created via the \code{create_game} function
}
\details{
This is the portion of the game wherein the contestant selects an
initial door.
}
\examples{
  select_door()
  
}
