# ========================================================================== #
# Utilities
# ========================================================================== #

# -------------------------------------------------------------------------- #
# .match
# -------------------------------------------------------------------------- #

# Runs a n-round match between 'f1' and 'f2' and returns a list with S, a
# matrix of each algo's mooves and P, a matrix containing their respective
# gains.
# You may also modify the payment matrix with :
# * t: the points earned by defectors when their opponents cooperate,
# * r: the points earned by both algos cooperate,
# * p: the points earned by both algos defect and
# * s: the points earned by cooperators when their opponents defect.
# Special case: if 'n' is set to NULL, the number of rounds goes random and
# may be anything from 'n.min' to 'n.max'.

.match = function(f1, f2, n = 2000, t = 5, r = 3, p = 1, s = 0, n.min = 100,
	n.max = 1000) {
	n1 <- deparse(substitute(f1))
	n2 <- deparse(substitute(f2))
	if(! is(f1, "function") | ! is(f2, "function"))
		stop("'f1' and 'f2' must be functions")
	n <- as.integer(n)
	if(n < 1) stop("'n' must be as least 1")
	if(any(diff(c(t, r, p, s)) >= 0))
		stop("check that: t > r > p > s")
	m <- c(p, s, t, r, p, t, s, r)
	dim(m) <- rep(2, 3)
	s1 <- s2 <- logical(0)
	p1 <- p2 <- integer(0)
	if(is.null(n)) n <- sample(n.min:n.max, 1)
	for(i in 1:n) {
		si1 <- f1(s1, s2, n)
		si2 <- f2(s2, s1, n)
		s1 <- c(s1, si1)
		s2 <- c(s2, si2)
		p1 <- c(p1, m[si1+1, si2+1, 1])
		p2 <- c(p2, m[si1+1, si2+1, 2])
	}
	S <- cbind(s1, s2)
	P <- cbind(p1, p2)
	colnames(S) <- colnames(P) <- c(n1, n2)
	list(S = S, P = P)
}

# -------------------------------------------------------------------------- #
# .tournament
# -------------------------------------------------------------------------- #

# Runs a tournament with algos provided in x, which must be a named list of
# algos. For other arguments, see .match. (You need 'gtools' to run it.)

.tournament = function(x, n = 2000, t = 5, r = 3, p = 1, s = 0, n.min = 100,
	n.max = 1000) {
	I <- gtools:::combinations(N <- length(x), 2)
	ans <- list()
	length(ans) <- N
	names(ans) <- names(x)
	for(i in 1:nrow(I)) {
		j <- I[i, 1]
		k <- I[i, 2]
		z <- .match(x[[j]], x[[k]], n, t, r, p, s, n.min, n.max)
		v <- unname(colSums(z$P))
		ans[[j]] <- c(ans[[j]], v[1])
		ans[[k]] <- c(ans[[k]], v[2])
	}
	sapply(ans, mean)
}
