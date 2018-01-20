# ========================================================================== #
# Testing
# ========================================================================== #

rm(list = ls(all = TRUE))

# Load functions:

root <- "https://raw.githubusercontent.com/gnicoulaud/axelrod/master/"
source(file.path(root, "utilities.R"))
source(file.path(root, "algos.R"))

# Run a single 10-rounds match between tft and rand:

(ans <- .match(tft, rand, 10))

# That's a list; if you just want the strategies:

ans$S

# To compute the gains:

colSums(ans$P)

# A small tournament with alld, allc, rand, tft, alt, grudger, detect, gtft,
# wsls and tf2t (200-rounds matches):

x <- list(alld = alld, allc = allc, rand = rand, tft = tft, alt = alt,
	grudger = grudger, detect = detect, gtft = gtft, wsls = wsls,
	tf2t = tf2t)

(ans <- .tournament(x, 200))

# Updated: Now .tournament returns a matrix:
sort(rowMeans(ans, na.rm = TRUE))
