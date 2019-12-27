## Advent of Code 2019, Day 21
## https://adventofcode.com/2019/day/21
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Load the incode computer
source("intcode/run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input21.txt"), ",")[[1]])

## Set to TRUE to see the prompts
show_prompts <- FALSE

## Function to run a springscript program
run_springscript <- function(intcode_state, input) {

    repeat ({
        intcode_state <- run_intcode_step(intcode_state)
        if (show_prompts) cat(intToUtf8(intcode_state$output))
        if (intcode_state$output == 10) break
    })

    if (show_prompts) cat(intToUtf8(input))
    intcode_state <- run_intcode_step(intcode_state, input)

    repeat ({
        intcode_state <- run_intcode_step(intcode_state)
        out <- intcode_state$output
        if (!is.null(out) && out < 128) {
            if (show_prompts) cat(intToUtf8(out))
        } else {
            break
        }
    })

    out

}

## -- PART 1 --
intcode_state <- list(mmry = program)
ss_program_1 <- c(utf8ToInt(paste(readLines("aoc19_day21_1.sps"),
                                  collapse = "\n")), 10)

solution_1 <- run_springscript(intcode_state, ss_program_1)
cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output21_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
ss_program_2 <- c(utf8ToInt(paste(readLines("aoc19_day21_2.sps"),
                                  collapse = "\n")), 10)

solution_2 <- run_springscript(intcode_state, ss_program_2)

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output21_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
