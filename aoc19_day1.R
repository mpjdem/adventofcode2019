## Advent of Code 2019, Day 1
## https://adventofcode.com/2019/day/1
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the input data
inp <- as.numeric(readLines("input/input1.txt"))

## -- PART 1 --
## This is a straightforward application of a function to all elements of a
## vector. Base R sapply will suffice.
mass_to_fuel <- function(x) floor(x / 3) - 2

initial_fuel <- sapply(inp, mass_to_fuel)

solution_1 <- sum(initial_fuel)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## This assignment screams 'RECURSIOOOOOOOON!!!'. So we recurse.
add_fuel_for_fuel <- function(fuel) {

    extra_fuel <- mass_to_fuel(fuel)

    if (extra_fuel <= 0) {
        fuel
    } else {
        fuel + add_fuel_for_fuel(extra_fuel)
    }

}

solution_2 <- sum(sapply(initial_fuel, add_fuel_for_fuel))
cat("Solution to Part 2:", solution_2, "\n")
