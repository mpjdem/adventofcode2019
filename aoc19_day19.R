## Advent of Code 2019, Day 19
## https://adventofcode.com/2019/day/19
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Load the incode computer
source("intcode/run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input19.txt"), ",")[[1]])

## -- PART 1 --
## Simply map the whole area
in_beam <- function(pos) run_intcode_step(list(mmry = program), pos)$output

coords <- expand.grid(x = 0:49, y = 0:49)
out <- lapply(seq(nrow(coords)), function(rw) {
    pos <- coords[rw,]
    cbind(pos, val = in_beam(as.numeric(pos)))
})
out <- do.call(rbind, out)

solution_1 <- sum(out$val)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Brute force; discretisation of the beam lines makes linear model inaccurate
## But we can establish some sort of rough lower bound and upper bound
sqsz <- 100
botl <- aggregate(y ~ x, out[out$val == 1,], min)
topl <- aggregate(y ~ x, out[out$val == 1,], max)
bota <- coef(lm(y ~ x, botl))[[2]]
topa <- coef(lm(y ~ x, topl))[[2]]
low_bound_y <- round(sqsz / (topa - bota))
low_bound_x <-  round(low_bound_y / bota)

first_x_hit <- function(y, rng_x) {
    for (x in rng_x) if (in_beam(c(x, y))) break
    x
}

y <- low_bound_y
x <- first_x_hit(y, seq(low_bound_x - sqsz, low_bound_x + sqsz))
for (y in low_bound_y:(low_bound_y * bota * 2)) {
    x <- first_x_hit(y, seq(x, x + 5))
    if (in_beam(c(x + (sqsz - 1), y - (sqsz - 1)))) break
}

solution_2 <- 10000 * x + (y - (sqsz - 1))
cat("Solution to Part 2:", solution_2, "\n")
