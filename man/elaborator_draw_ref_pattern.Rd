% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elaborator_draw_ref_pattern.R
\name{elaborator_draw_ref_pattern}
\alias{elaborator_draw_ref_pattern}
\title{Generates tree diagram according to the reference-value based pattern analysis}
\usage{
elaborator_draw_ref_pattern(
  pattern_Matrix,
  fontsize,
  number_columns,
  background_color = "#E2F3F2"
)
}
\arguments{
\item{pattern_Matrix}{matrix with reference-value based pattern information}

\item{fontsize}{font size of numbers printed inside the circles}

\item{number_columns}{number of layers (visits) of tree diagram}

\item{background_color}{background color}
}
\value{
No return value; this function is called to draw a tree diagram according to the reference-value based pattern analysis.
}
\description{
Generate tree diagram showing the number of patients with normal/abnormal values at each visit of a study according to the reference-value based pattern analysis.
}
\keyword{internal}
