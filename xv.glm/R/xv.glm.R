# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want
# To Public License, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.

xv.glm <- function (data, model, type="response", cost=function(y, yhat) mean((y - yhat)^2), K=nrow(data), adjust=TRUE, pro.rata=FALSE) {
	call <- match.call()
	if (!exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
		runif(1)
	seed <- get(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
	if (!(is.matrix(data) || is.data.frame(data)))
		stop("'data' must be a matrix or a data frame")
	n <- nrow(data)
	y <- model.response(model.frame(model))
	if (is.function(cost))
		cost <- list(cost=cost)
	if (!is.list(cost) || (cost.n <- length(cost)) < 1)
		stop("'cost' must be a function or a non-empty list of functions")
	K <- round(K.old <- K)
	if (!all.equal(K, K.old) || !is.finite(K))
		stop("'K' must be a finite integer")
	else if ((K > n) || (K < 1)) 
		stop("'K' outside allowable range")
	adjust <- as.logical(adjust)
	pro.rata <- as.logical(pro.rata)
	adjustments <- array(dim=cost.n)
	fitted.predictions <- predict(model, data, type=type)
	if (K == 1) {
		scores <- matrix(ncol=cost.n, nrow=1L)
		for (i in 1L:cost.n)
			scores[1L, i] <- cost[[i]](y, fitted.predictions)
		scores.mean	<- scores[1L, ]
		samples <- rep(1L, n)
		weights <- rep(1/n, n)
		adjust <- FALSE
	} else {
		kvals <- unique(round(n/(1L:floor(n/2))))
		if (!any((temp <- abs(kvals - K)) == 0)) 
			K <- kvals[which.min(temp)]
		if (K != K.old) 
			warning("'K' has been set to ", K)
		if (K == n)
			samples <- 1L:n
		else {
			rep.k <- rep(1L:K, ceiling(n/K))
			resample <- function (x, ...) x[sample.int(length(x), ...)]
			sample.k <- function (n) resample(rep.k, n)
			if (!pro.rata)
				samples <- sample.k(n)
			else {
				if (!is.factor(y))
					y <- as.factor(y)
				samples <- integer(length=n)
				for (level in levels(y)) {
					which.level <- y == level
					samples[which.level] <- sample.k(sum(which.level))
				}
			}
		}
		for (i in 1L:cost.n)
			adjustments[i] <- cost[[i]](y, fitted.predictions)
		weights	<- table(samples) / n
		scores	<- matrix(ncol=cost.n, nrow=K)
		model.call <- model$call
		for (k in (kfolds <- seq_len(max(samples)))) {
			y.heldout <- y[(held.out <- which(samples == k))]
			model.call$data <- data[-held.out, , drop=FALSE]
			model.k <- eval.parent(model.call)
			model.k.predictions <- predict(model.k, data, type=type)
			model.k.predictions.heldout <- predict(model.k, data[held.out, , drop=FALSE], type=type)
			for (i in 1L:cost.n) {
				scores[k, i]	<- cost[[i]](y.heldout, model.k.predictions.heldout)
				adjustments[i]	<- adjustments[i] - weights[k] * cost[[i]](y, model.k.predictions)
			}
		}
		scores.mean <- apply(scores, 2, weighted.mean, weights)
		if (adjust && K < n)
			scores.mean <- scores.mean + adjustments
		else
			adjust <- FALSE
		rownames(scores) <- kfolds
	}
	colnames(scores) <- names(scores.mean) <- names(adjustments) <- names(cost)
	list(call=call, K=K, cost=cost, scores=scores, scores.mean=scores.mean, adjust=adjust, adjustments=adjustments, samples=samples, weights=weights, pro.rata=pro.rata, seed=seed)
}
