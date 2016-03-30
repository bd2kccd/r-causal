\name{rcausal}
\alias{rcausal}
\alias{r-causal}
\title{R Wrapper for Tetrad Library}
\description{
 \code{rcausal} is an R wrapper package containing a range of causal and 
 statistical model algorithms from the Tetrad library.
}
\details{
\code{rcausal} is a program which creates, simulates data from, estimates, tests, 
predicts with, and searches for causal and statistical models. The aim of 
the program is to provide sophisticated methods in a friendly interface 
requiring very little statistical sophistication of the user and no 
programming knowledge. It is not intended to replace flexible statistical 
programming systems such as Matlab, Splus or R. \code{rcausal} is freeware that 
performs many of the functions in commercial programs such as Netica, Hugin, 
LISREL, EQS and other programs, and many discovery functions these commercial 
programs do not perform.

\code{rcausal} is unique in the suite of principled search 
(\dQuote{exploration},\dQuote{discovery}) algorithms it provides--for example 
its ability to search when there may be unobserved confounders of 
measured variables, to search for models of latent structure, and to search for
linear feedback models--and in the ability to calculate predictions of 
the effects of interventions or experiments based on a model. All of its 
search procedures are \dQuote{pointwise consistent}--they are guaranteed to 
converge almost certainly to correct information about the true structure in 
the large sample limit, provided that structure and the sample data satisfy 
various commonly made (but not always true!) assumptions.

\code{rcausal} is limited to models of categorical data (which can also be used
for ordinal data) and to linear models (\dQuote{structural equation models}) 
with a Normal probability distribution, and to a very limited class of 
time series models. The \code{rcausal} programs describe causal models in 
three distinct parts or stages: a picture, representing a directed graph 
specifying hypothetical causal relations among the variables; a specification 
of the family of probability distributions and kinds of parameters associated 
with the graphical model; and a specification of the numerical values of 
those parameters.

The program and its search algorithms have been developed over several years 
with support from the National Aeronautics and Space Administration and 
the Office of Naval Research. Joseph Ramsey has implemented most of 
the program, with substantial assistance from Frank Wimberly. 
}
\keyword{causal graphs}
\keyword{causality}
\keyword{causal models}
\keyword{causal searchs}
\keyword{causal discovery}
\keyword{tetrad}