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

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output12_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## Find the period of x,y,z independently, then compute LCM
## Initially I cheated on base and used {schoolmath} for Least Common Multiple
## But Tim Taylor had a simple implementation here:
## https://tjtnew.github.io/blog/posts/2019-12-12-advent-of-code-2019-day-12-r/
## So I've borrowed that approach now
gcd <- function(x, y) {while (y != 0) {r <- x %% y; x <- y; y <- r}; x}
lcm <- function(x,y) x * y / gcd(x, y)

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

solution_2 <- Reduce(lcm, res)

cat("Solution to Part 2:", format(solution_2, scientific = FALSE), "- ")
check_2 <- as.numeric(readLines("output/output12_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
