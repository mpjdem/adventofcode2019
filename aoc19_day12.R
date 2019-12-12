## Advent of Code 2019, Day 12
## https://adventofcode.com/2019/day/12
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the starting positions. Starting velocities are 0.
inp <- strsplit(gsub("[<xyz>=,]+", "", readLines("input/input12.txt")), " ")
pos <- matrix(as.numeric(unlist(inp)), ncol = 3, byrow = TRUE)
vel <- matrix(0, ncol = ncol(pos), nrow = nrow(pos))

## Function to compute the changes in velocity in one dimension
vel_change <- function(pv) {
  sapply(pv, function(x) sum(pv > x)) - sapply(pv, function(x) sum(pv < x))
}

## Function to perform one step of updates to both velocities and positions
step_update <- function(dat) {
  dat$vel <- dat$vel + apply(dat$pos, 2, vel_change)
  dat$pos <- dat$pos + dat$vel
  dat
}

## -- PART 1 --
init_dat <- list(vel = vel, pos = pos)
dat <- init_dat
for (i in seq(1000)) dat <- step_update(dat)

solution_1 <- sum(rowSums(abs(dat$pos)) * rowSums(abs(dat$vel)))
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Here I'll have to cheat on base R... not going to implement prime factors!
library(schoolmath)

dat <- init_dat
res <- rep(0, 3)
step <- 0
repeat ({
  step <- step + 1
  dat <- step_update(dat)
  matched <- (colMeans(dat$vel == init_dat$vel) *
              colMeans(dat$pos == init_dat$pos)) == 1
  res[res == 0 & matched == TRUE] <- step
  if (all(res > 0)) break
})

solution_2 <- scm(scm(res[1], res[2]), res[3])
cat("Solution to Part 2:", format(solution_2, scientific = FALSE), "\n")
