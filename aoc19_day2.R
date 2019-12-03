## Advent of Code 2019, Day 2
## https://adventofcode.com/2019/day/2
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the input data
inp <- as.numeric(strsplit(readLines("input/input2.txt"), ",")[[1]])

## -- PART 1 --
## This is complicated by R's 1-based indexing, whereas the 'intcode' program
## uses 0-based indexing. It seems simpler to keep the instruction pointer as
## 1-based. I choose recursion over iteration here, it's more readable.
run_intcode <- function(mmry, ptr = 1) {

    if (ptr > length(mmry)) {
        stop("Malformed intcode: Invalid instruction pointer")
    }

    opcode <- mmry[ptr]

    if (opcode == 99) {

        mmry

    } else if (opcode %in% c(1, 2)) {

        if ((ptr + 3) > length(mmry)) {
            stop("Malformed intcode: Not enough parameters")
        }

        if (any((mmry[(ptr + 1):(ptr + 2)] + 1) > length(mmry))) {
            stop("Malformed intcode: Invalid parameters")
        }

        op <- if (opcode == 1) `+` else if (opcode == 2) `*`

        mmry[mmry[ptr + 3] + 1] <- op(mmry[mmry[ptr + 1] + 1],
                                      mmry[mmry[ptr + 2] + 1])

        if ((ptr + 4) > length(mmry)) mmry else run_intcode(mmry, ptr + 4)

    } else {
        stop("Malformed intcode: Invalid opcode")
    }

}

inp_1202 <- inp
inp_1202[2:3] <- c(12, 2)
solution_1 <- run_intcode(inp_1202)[1]
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## For 10k possible inputs we can use brute force easily. I use common base R
## idioms to iterate over all combinations and locate a match.
try_inputs <- function(noun, verb, mmry) {
    mmry[2:3] <- c(noun, verb)
    run_intcode(mmry)[1]
}

df <- expand.grid(noun = seq(0,99), verb = seq(0,99))
df$outp <- mapply(try_inputs, df$noun, df$verb, MoreArgs = list(mmry = inp))
idx <- which(df$outp == 19690720)

solution_2 <- (100 * df$noun[idx]) + df$verb[idx]
cat("Solution to Part 2:", solution_2, "\n")
