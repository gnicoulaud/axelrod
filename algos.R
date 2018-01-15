# ========================================================================== #
# Basic algos
# ========================================================================== #

# These are the algos included into the tournament anyway.

# -------------------------------------------------------------------------- #
# AllC
# -------------------------------------------------------------------------- #

# Always cooperates.

allc = function(p, o, n = 2000) TRUE


# -------------------------------------------------------------------------- #
# AllD
# -------------------------------------------------------------------------- #

# Always defects.

alld = function(p, o, n = 2000) FALSE


# -------------------------------------------------------------------------- #
# Alternate
# -------------------------------------------------------------------------- #

# Randomly cooperate or defect (prob = 1/2) on the 1st round then alternates
# regardless of what the opponent does.

alt = function(p, o, n = 2000) {
	ifelse(length(p) == 0, sample(c(T, F), 1), !tail(p, 1))
}


# -------------------------------------------------------------------------- #
# Grudger
# -------------------------------------------------------------------------- #

# Cooperates until the opponent defects and then defects forever.

grudger = function(p, o, n = 2000) {
	all(o)
}


# -------------------------------------------------------------------------- #
# Random
# -------------------------------------------------------------------------- #

# This strategy plays randomly (prob = 1/2) disregarding the history of play.

rand = function(p, o, n = 2000) {
	sample(c(TRUE, FALSE), 1)
}


# -------------------------------------------------------------------------- #
# Detective
# -------------------------------------------------------------------------- #

# Cooperates, defects, cooperates and cooperates again. If the opponent
# doesn't relatilates in the 3rd round, defects all the time; otherwise plays
# Tit-for-Tat.

detect = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r < 5) {
		res <- as.logical(r != 2)
	} else {
		res <- ifelse(o[3], FALSE, o[r-1])
	}
	res
}


# -------------------------------------------------------------------------- #
# Tit-For-Tat
# -------------------------------------------------------------------------- #

# Begins by cooperating and then simply repeats the last moves made by the
# opponent.

tft = function(p, o, n = 2000) {
	ifelse(length(p) == 0, TRUE, tail(o, 1))
}


# -------------------------------------------------------------------------- #
# Generous Tit-For-Tat
# -------------------------------------------------------------------------- #

# Same as TFT but 'forgives' defections in 1/3 of cases.

gtft = function(p, o, n = 2000) {
	res <- TRUE
	if(length(p) > 0 && !tail(o, 1)) res <- sample(c(F, T), 1,, 2:1) 
	res
}


# -------------------------------------------------------------------------- #
# Tit-For-Two-Tats
# -------------------------------------------------------------------------- #

# Like TFT but only retaliates after two defections rather than one.

tf2t = function(p, o, n = 2000) {
	res <- TRUE
	if(length(p) > 1) res <- ! all(! tail(o, 2))
	res
}


# -------------------------------------------------------------------------- #
# Win-Stay-Lose-Shift
# -------------------------------------------------------------------------- #

# Cooperates first then, if the opponent cooperated on the last round, repeat
# last move; otherwise, switch.

wsls = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r == 1) {
		res <- TRUE
	} else {
		res <- ifelse(tail(o, 1), tail(p, 1), !tail(p, 1))
	}
	res
}


# -------------------------------------------------------------------------- #
# Zero-Determinant GTFT 2
# -------------------------------------------------------------------------- #

# Always collaborates if the opponent did. When the opponent defects, it
# collaborates with a probability of 1/8 if it has collaborated on the last
# round or with a probability of 1/4 otherwise.

zdgtft2 = function(p, o, n = 2000) {
	if(length(p) == 0) {
		res <- TRUE
	} else {
		if(tail(o, 1)) {
			res <- TRUE
		} else {
			z <- ifelse(tail(p, 1), 1/8, 1/4)
			res <- sample(c(T, F), 1,, c(z, 1-z))
		}
	}
	res
}


# -------------------------------------------------------------------------- #
# Extort 2
# -------------------------------------------------------------------------- #

# If the opponent collarorated on the last round, it will collaborate with a
# probability of 8/9 if it has also collaborated and 1/3 otherwise. If the
# opponent defected, it will collaborate with a probability of 1/2 if it
# collaborated and 0 otherwise.

extort2 = function(p, o, n = 2000) {
	if(length(p) == 0) {
		res <- TRUE
	} else {
		if(tail(o, 1)) {
			z <- ifelse(tail(p, 1), 8/9, 1/3)
		} else {
			z <- ifelse(tail(p, 1), 1/2, 0)
		}
		res <- sample(c(TRUE, FALSE), 1, prob = c(z, 1-z))
	}
	res
}


# -------------------------------------------------------------------------- #
# Nydegger
# -------------------------------------------------------------------------- #

# This strategy begins by playing Tit For Tat for the first 3 rounds with the
# following modifications: if it is the only strategy to cooperate in the
# first round and the only strategy to defect on the second round then it
# defects on the 3 round (despite the fact that TFT would now cooperate).
# After these first 3 rounds the next move is made depending on the previous 3
# rounds. A score is given to these rounds according to the following
# calculation:
# A = 16a_1 + 4a_2 + a_3
# Where a_i is dependent on the outcome of the previous i^th round. If both
# strategies defect, a_i = 3, if the opponent only defects: a_i = 2 and 
# finally if it is only this strategy that defects then a_i = 1 (*).
# Finally this strategy defects if and only if:
# A \in {1,6,7,17,22,23,26,29,30,31,33,38,39,45,49,54,55,58,61}
# This strategy came 3rd in Axelrod’s original tournament.

# (*) I've assumed that if none defect then a_i = 0.

nydegger = function(p, o, n = 2000) {
	r <- length(p)+1
	V <- c(1, 6, 7, 17, 22, 23, 26, 29, 30, 31, 33, 38, 39, 45, 49, 54, 55,
		 58, 61)
	if(r %in% 1:3) {
		if(r == 1) {
			res <- TRUE
		}
		if(r == 2) {
			res <- tail(o, 1)
		}
		if(r == 3) {
			res <- tail(o, 1)
			if((p[1] & !o[1]) & (!p[2] & o[2])) res <- FALSE
		}
	} else {
		a1 <- a2 <- a3 <- 0
		if(!p[r-1] & !o[r-1]) {
			a1 <- 3
		} else {
			a1 <- ifelse(!o[r-1], 2, 1)
		}
		if(!p[r-2] & !o[r-2]) {
			a2 <- 3
		} else {
			a2 <- ifelse(!o[r-2], 2, 1)
		}
		if(!p[r-3] & !o[r-3]) {
			a3 <- 3
		} else {
			a3 <- ifelse(!o[r-3], 2, 1)
		}
		A <- 16*a1+4*a2+a3
		res <- ! A %in% V
	}
	res
}


# -------------------------------------------------------------------------- #
# Grofman
# -------------------------------------------------------------------------- #

# This is a pretty simple strategy: it cooperates on the first two rounds and
# returns the opponent’s last action for the next 5. For the rest of the game
# Grofman cooperates if both players selected the same action in the previous
# round, and otherwise cooperates randomly with probability 2/7

grofman = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r %in% 1:2) {
		res <- TRUE
	}
	if(ro %in% 3:7) {
		res <- tail(o, 1)
	}
	if(r > 7) {
		if(tail(p, 1) == tail(o, 1)) {
			res <- TRUE
		} else {
			res <- sample(c(T, F), 1, prob = c(2, 5)/7)
		}
	}
	res
}


# -------------------------------------------------------------------------- #
# Shubik
# -------------------------------------------------------------------------- #

# This strategy plays a modification of Tit For Tat. It starts by retaliating
# with a single defection but the number of defections increases by 1 each
# time the opponent defects when this strategy cooperates.

shubik = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r == 1) {
		res <- TRUE
	} else {
		n <- max(1, sum(p & !o))
		d <- tail(which(!o), 1)
		punish <- ifelse(length(d) == 0, FALSE, r <= d+n)
		res <- tail(o, 1) & !punish
	}
	res
}


# -------------------------------------------------------------------------- #
# Davis
# -------------------------------------------------------------------------- #

# This strategy is a modification of Grudger. It starts by cooperating for the
# first 10 moves and then plays Grudger (*).
# (*) I've assumed it only plays Grudger using data from round 10 onward...

davis = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r < 11) {
		res <- TRUE
	} else {
		res <- all(o[10:length(o)])
	}
	res
}


# -------------------------------------------------------------------------- #
# Joss
# -------------------------------------------------------------------------- #

# This strategy plays Tit For Tat, always defecting if the opponent defects
# but cooperating when the opponent cooperates with probability .9.

joss = function(p, o, n = 2000) {
	if(length(p) == 0) {
		res <- TRUE
	} else {
		if(tail(o, 1)) {
			res <- sample(c(T, F), 1, prob = c(.9, .1))
		} else {
			res <- FALSE
		}
	}
	res
}


# -------------------------------------------------------------------------- #
# Tullock
# -------------------------------------------------------------------------- #

# This strategy cooperates for the first 11 rounds and then (randomly)
# cooperates 10% less often than the opponent has in the previous 10 rounds.
# This strategy came 13th in Axelrod’s original tournament.

tullock = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r < 12) {
		res <- TRUE
	} else {
		z <- .9 * sum(tail(o, 10))/10
		res <- sample(c(T, F), 1, prob = c(z, 1-z))
	}
	res
}


# -------------------------------------------------------------------------- #
# Eatherley
# -------------------------------------------------------------------------- #

# Generally cooperates unless the opponent defects, in which case Eatherley
# defects with a probability equal to the proportion of rounds that the
# opponent has defected.

eatherley = function(p, o, n = 2000) {
	if(all(o)) {
		res <- TRUE
	} else {
		z <- sum(!o)/length(o)
		res <- sample(c(F, T), 1, prob = c(z, 1-z))
	}
	res
}

# -------------------------------------------------------------------------- #
# Champion
# -------------------------------------------------------------------------- #

# Operates in three phases: The first phase lasts for the first 1/20-th of the
# rounds and Champion always cooperates. In the second phase, lasting until
# 4/50-th of the rounds have passed, Champion mirrors its opponent’s last
# last move. In the last phase, Champion cooperates unless
# - the opponent defected on the last round, and
# - the opponent has cooperated less than 60% of the rounds, and
# - a random number is greater than the proportion of rounds defected (*).

# I assume it means defected *by the opponent*.

champion = function(p, o, n = 2000) {
	r <- length(p)+1
	v <- round(c(n/20, n/20+n*4/50))
	if(r %in% 1:v[1]) {
		res <- TRUE
	}
	if(r %in% (v[1]+1):(v[2])) {
		res <- tail(o, 1)
	}
	if(r > v[2]) {
		z <- sum(o)/length(o)
		c1 <- !tail(o, 1)
		c2 <- z < .6
		c3 <- runif(1) > (1-z)
		res <- ifelse(c1 & c2 & c3, FALSE, TRUE)
	}
	res
}


# -------------------------------------------------------------------------- #
# Tester
# -------------------------------------------------------------------------- #

# This strategy is a TFT variant that attempts to exploit certain strategies.
# It defects on the first move. If the opponent ever defects, Tester
# 'apologies' by cooperating and then plays TFT for the rest of the game.
# Otherwise Tester alternates cooperation and defection.
# This strategy came 46th in Axelrod’s second tournament.

# Not sure I properly interpreded it. I assumed it defects first to see how
# the opponent reacts to its defection. So it must do something at round 2
# given it doesn't know yet what the reaction is. I assumed it collaborates.
# Also, that version plays TFT if the opponent reacted negatively on the round
# and alternates otherwhise.

tester = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r %in% 1:2) {
		res <- c(FALSE, TRUE)[r]
	} else {
		if(! o[2]) {
			res <- ifelse(r == 3, TRUE, tail(o, 1))
		} else {
			res <- !tail(p, 1)
		}
	}
	res
}


# ========================================================================== #
# Not implemented (yet)
# ========================================================================== #

# -------------------------------------------------------------------------- #
# Stein and Rapoport
# -------------------------------------------------------------------------- #

# This strategy plays a modification of Tit For Tat.
# 1. It cooperates for the first 4 moves.
# 2. It defects on the last 2 moves.
# 3. Every 15 moves it makes use of a chi-squared test to check if the
#    opponent is playing randomly.
# This strategy came 6th in Axelrod's original tournament.


# -------------------------------------------------------------------------- #
# Tideman and Chieruzzi
# -------------------------------------------------------------------------- #

# This strategy begins by playing Tit For Tat and then things get slightly
# complicated:
# 1. Every run of defections played by the opponent increases the number of
#    defections that this strategy retaliates with by 1.
# 2. The opponent is given a ‘fresh start’ if:
#    - it is 10 points behind this strategy
#    - and it has not just started a run of defections
#    - and it has been at least 20 rounds since the last ‘fresh start’
#    - and there are more than 10 rounds remaining in the tournament
#    - and the total number of defections differs from a 50-50 random sample
#      by at least 3.0 standard deviations.
# A ‘fresh start’ is a sequence of two cooperations followed by an
# assumption that the game has just started (everything is forgotten).
# This strategy came 2nd in Axelrod’s original tournament


# -------------------------------------------------------------------------- #
# Graaskamp
# -------------------------------------------------------------------------- #

# This strategy follows the following rules:
# 1. Play Tit For Tat for the first 50 rounds;
# 2. Defects on round 51;
# 3. Plays 5 further rounds of Tit For Tat;
# 4. A check is then made to see if the opponent is playing randomly in which
#    case it defects for the rest of the game;
# 5. The strategy also checks to see if the opponent is playing Tit For Tat or
#    another strategy from a preliminary tournament called 'Analogy'. If so it
#    plays Tit For Tat. If not it cooperates and randomly defects every 5 to
#    15 moves.
# This strategy came 9th in Axelrod's original tournament.


# -------------------------------------------------------------------------- #
# Downing
# -------------------------------------------------------------------------- #

# This strategy attempts to estimate the next move of the opponent by
# estimating the probability of cooperating given that they defected (P(C|D))
# or cooperated on the previous round (P(C|C)). These probabilities are
# continuously updated during play and the strategy attempts to maximise the
# long term play. Note that the initial values are:
# P(C|C) = P(C|D) = 1/2
# This strategy came 10th in Axelrod’s original tournament.


# -------------------------------------------------------------------------- #
# Feld
# -------------------------------------------------------------------------- #

# This strategy plays Tit For Tat, always defecting if the opponent defects
# but cooperating when the opponent cooperates with a gradually decreasing
# probability until it is only 1/2.
# This strategy came 11th in Axelrod’s original tournament.


# -------------------------------------------------------------------------- #
# Unnamed Strategy
# -------------------------------------------------------------------------- #

# This strategy cooperates with a given probability P. This probability (which
# has initial value .3) is updated every 10 rounds based on whether the
# opponent seems to be random, very cooperative or very uncooperative.
# Furthermore, if after round 130 the strategy is losing then P is also
# adjusted.
# Original code not available...
# This strategy came 14th in Axelrod’s original tournament.


# ========================================================================== #
# Submitted algos
# ========================================================================== #

# These are the algos you have submitted.

# -------------------------------------------------------------------------- #
# Generous Tit-For-Tat In Cauda Venenum
# -------------------------------------------------------------------------- #

# By Damien Cormann.
# A variation on 'Generous Tit-For-Tat' that systematically defaults on the
# last round (hence, 'In Cauda Venenum').

gtfticv = function(p, o, n = 2000) {
	r <- length(p)+1
	if(r %in% c(1, n)) {
		res <- r == 1
	} else {
		res <- ifelse(tail(o, 1), T, sample(c(T, F), 1, p = 1:2))
	}
	res
}



