## Advent of Code 2019, Day 6
## https://adventofcode.com/2019/day/6
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz


## Read the input
## LHS is the orbitee, RHS is the orbiter
inp <- read.delim("input/input6.txt", sep = ")",
                  header = FALSE,
                  col.names = c("orbitee", "orbiter"),
                  stringsAsFactors = FALSE)

## -- PART 1 --
## Just count the number of hops starting from every unique object
## Memoise for speed
memo_env <- new.env()

count_orbits <- function(orbiter) {

    if (!is.null(memo_env[[orbiter]])) {
        memo_env[[orbiter]]
    } else if (orbiter %in% inp$orbiter) {
        memo_env[[orbiter]] <-
            1 + count_orbits(inp[inp$orbiter == orbiter,]$orbitee)
    } else {
        0
    }

}

solution_1 <- sum(sapply(unique(inp$orbiter), count_orbits))

cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Construct the path from both YOU and SAN to the center
## Number every hop, and inner join both path data frames
## The common object with lowest total number of hops is what we need
get_orbitees <- function(orbiter) {

    if (orbiter %in% inp$orbiter) {
        orbitee <- inp[inp$orbiter == orbiter,]$orbitee
        c(orbitee, get_orbitees(orbitee))
    } else {
        character(0)
    }

}

path_you <- get_orbitees("YOU")
path_santa <- get_orbitees("SAN")

shared_path <-
    merge(data.frame(object = path_you,
                     n_hops_you = seq_along(path_you) - 1,
                     stringsAsFactors = FALSE),
          data.frame(object = path_santa,
                     n_hops_santa = seq_along(path_santa) - 1,
                     stringsAsFactors = FALSE),
          by = "object", all = FALSE)

solution_2 <- min(shared_path$n_hops_you + shared_path$n_hops_santa)

cat("Solution to Part 2:", solution_2, "\n")
