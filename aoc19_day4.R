## Advent of Code 2019, Day 4
## https://adventofcode.com/2019/day/4
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Define the input
inp <- c(138241, 674034)

## -- PART 1 --
## Take all possibilities and convert into a vector of single-digit integers
## Then take the diff of each vector and require a) no -1 and b) at least one 0
candidates <- as.character(seq(inp[1], inp[2]))

candidate_diffs <-
    Map(function(x) {
        diff(as.integer(substring(x, seq(nchar(x)), seq(nchar(x)))))
    }, candidates)

solution_1 <-
    length(Filter(function(x) all(x >= 0L) && (0L %in% x),
                  candidate_diffs))

cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Allow at most one consecutive 0 in the diffs
## We can simply diff again, in both directions
solution_2 <-
    length(Filter(function(x) {
        diff_fw <- c(diff(x), Inf)
        diff_bw <- c(Inf, rev(diff(rev(x))))
        all(x >= 0L) && any((x == 0L) & (diff_fw != 0L) & diff_bw != 0L)
    }, candidate_diffs))

cat("Solution to Part 2:", solution_2, "\n")

## This wasn't the most efficient thing to do really... but it works!
