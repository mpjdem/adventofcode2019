## Advent of Code 2019, Day 16
## https://adventofcode.com/2019/day/16
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the puzzle input
inp <- as.numeric(strsplit(readLines("input/input16.txt"), "")[[1]])

## -- PART 1 --
## Function to run one FFT phase, given a sequence and a repeat pattern
one_phase <- function(sq, rppat) {
    sapply(seq(length(sq)), function(rp_each) {
        rp <- rep(rppat, each = rp_each)
        rp <- rep_len(rp, length.out = (length(sq) + 1))[2:(length(sq) + 1)]
        abs(sum(rp * sq)) %% 10
    })
}

## Function to run N phases + parse relevant digits from the output
run_phases <- function(sq, rppat, n_phases, n_skip_out, n_dig_out) {
    for (it in seq(n_phases)) {
        sq <- one_phase(sq, rppat)
    }
    sq[(1 + n_skip_out):(n_skip_out + n_dig_out)]
}

solution_1 <- paste(run_phases(inp, c(0, 1, 0, -1), 100, 0, 8),
                    collapse = "")

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output16_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## The naive approach of Part 1 will take far too long
## But it provides useful data for understanding the patterns it produces
##
## Observations FOR THE SECOND HALF OF THE COLUMNS:
##   - Last column is the last element repeated
##   - Second-to-last = sum of last two cols in the previous row, mod 10
##   - More general: the reverse cumsum, mod 10, yields the next row
##
## Our puzzle input is in the second half, so we can ignore all cols before

run_phases_alt <- function(sq, n_phases, n_skip_out, n_dig_out) {
    if (n_skip_out < (length(sq) / 2)) stop("No implementation for first half")
    ssq <- tail(sq, length(sq) - n_skip_out)
    for (i in seq(n_phases)) ssq <- rev(cumsum(rev(ssq)) %% 10)
    ssq[1:n_dig_out]
}

n_skip <- sum(inp[1:7] * 10**(seq(6, 0)))

solution_2 <- paste(run_phases_alt(rep(inp, 10000), 100, n_skip, 8),
                    collapse = "")

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output16_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
