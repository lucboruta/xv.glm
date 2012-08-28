\name{xv.glm}
\alias{xv.glm}
\title{
Cross-validation for Generalized Linear Models
}
\description{
This function calculates the estimated K-fold cross-validation prediction 
error for generalized linear models.

It builds upon \code{boot::cv.glm}, providing the following new features: models of class other than \code{"glm"} can be evaluated, various cost functions can be efficiently used to estimate the cross-validation prediction error on the same K-fold groups, all K-fold scores are reported (and not just the weighted mean), and K-fold groups can be sampled pro rata with the relative frequencies of response levels.
}
\usage{
xv.glm(data, model, type="response",
cost=function(y, yhat) mean((y - yhat)^2),
K=nrow(data), adjust=TRUE, pro.rata=FALSE)
}
\arguments{
\item{data}{
A matrix or data frame containing the data. The rows should be cases and
the columns correspond to variables, one of which is the response.
}
\item{model}{
An object containing the results of a model fitted to \code{data}.
As much as possible, the function invokes methods which depend on the class of \code{model}.
The only constraints on the model are the following:
the response can be extracted invoking \code{model.response(model.frame(model))},
the call by which the model was fitted to \code{data} is available in \code{model\$call}, and
the prediction method can be invoked as \code{predict(model, data, type=type)}.
}
\item{type}{
The type of prediction required.
For example, \code{"glm"} objects allow \code{"link"}, \code{"response"} and \code{"terms"} as valid predictions types, \code{"multinom"} objects allow \code{"class"} and \code{"probs"}, etc.
}
\item{cost}{
A single cost function or a list of cost functions.
A cost function is a function of two arguments specifying the cost function for the 
cross-validation: the first argument should correspond to the
observed responses, and the second argument should correspond to the predicted
or fitted responses (see \code{type}) from the generalized linear model. Cost functions must return a
non-negative scalar value.  The default is the average squared error function.
}
\item{K}{
The number of groups into which the data should be split to estimate the
cross-validation prediction error, with \code{1 <= K <= nrow(data)}.  The value of \code{K} must be such that all
groups are of approximately equal size.  If the supplied value of \code{K} does
not satisfy this criterion, then it will be set to the closest integer which
does and a warning is generated specifying the value of \code{K} used.  The default
is to set \code{K} equal to the number of observations in \code{data} which gives the
usual leave-one-out cross-validation.
}
\item{adjust}{A logical indicating whether adjusted cross-validation estimates should be returned.
The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.
If \code{adjust} is \code{FALSE}, then raw cross-validation estimates of prediction error are returned.
If \code{K=1} or \code{K=n}, then \code{adjust} is ignored and subsequently set to \code{FALSE}.
}
\item{pro.rata}{
A logical indicating whether K-fold groups should be sampled pro rata with the relative frequencies of response levels. (Be careful what you wish for, you might just get it.)
}
}
\value{
The returned value is a list with the following components.
\item{call}{
The original call to \code{xv.glm}.
}
\item{K}{
The value of \code{K} used for the K-fold cross validation.
}
\item{cost}{
The list of cost functions used for the K-fold cross validation.
}
\item{scores}{
A matrix containing the estimated K-fold cross-validation prediction 
errors. The rows correspond to K-fold groups and
the columns correspond to cost functions.
}
\item{scores.mean}{
A vector containing, for each cost function, the weighted mean of the estimated K-fold cross-validation prediction 
errors.
}
\item{adjust}{
A logical indicating whether adjusted cross-validation estimates were returned in \code{scores.mean}.
}
\item{adjustments}{
An array containing, for each cost function, the adjustment designed to compensate for the bias introduced by not using leave-one-out cross-validation, or \code{NA} if \code{K=1}.
}
\item{samples}{
A integer vector indicating to which K-fold group each case was assigned. 
}
\item{weights}{
A vector containing, for each K-fold group, its relative weight.
}
\item{pro.rata}{
A logical indicating whether K-fold groups were sampled pro rata with the relative frequencies of response levels.
}
\item{seed}{
The value of \code{.Random.seed} when \code{xv.glm} was called. 
}
}
\section{Side Effects}{
The value of \code{.Random.seed} is updated.
}
\author{
Luc Boruta, based on code for \code{cv.glm} in the \code{boot} package, version 1.2-43 (S original by Angelo Canty, and R port by Brian Ripley).
}
\details{
The data is divided randomly into \code{K} groups.  For each group the generalized
linear model is fit to \code{data} omitting that group, then each function in \code{cost}
is applied to the observed responses in the group that was omitted from the fit
and the prediction made by the fitted models for those observations.

When \code{K} is the number of observations leave-one-out cross-validation is used
and all the possible splits of the data are used.  When \code{K} is less than
the number of observations the \code{K} splits to be used are found by randomly
partitioning the data into \code{K} groups of approximately equal size.  In this
latter case a certain amount of bias is introduced.  This can be reduced by
using a simple adjustment (see the documentation of \code{boot::cv.glm} for additional details and references).

While getting cross-validation prediction 
errors using different cost functions could be done playing around with \code{.Random.seed} and \code{boot::cv.glm}, this function should be more efficient as all preprocessing steps involved in assigning cases to K-fold groups is only done once.
}
\seealso{
\code{\link[stats]{glm}}, \code{\link[stats]{predict.glm}}, \code{\link[nnet]{multinom}}, \code{\link[nnet]{predict.multinom}}, \code{\link[boot]{cv.glm}}
}
\examples{
# See the documentation of 'boot::cv.glm' for examples,
# additional details and references.
}
