## Advent of Code 2019, Day 25
## https://adventofcode.com/2019/day/25
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

source("intcode/run_intcode_step.R")
program <- as.numeric(strsplit(readLines("input/input25.txt"), ",")[[1]])

## -- PART 1 --
## Set `interactive` to TRUE to play as an interactive game
## `q` will quit the game at any point
interactive <- FALSE

intcode_state <- list(mmry = program)
input <- numeric(0)
output <- numeric(0)

commands <- c("west", "take mug", "east", "east", "take coin",
              "north", "north", "take hypercube", "south", "south",
              "south", "west", "take astrolabe", "north", "east",
              "north", "east")
command_ptr <- 0

repeat ({

    intcode_state <- run_intcode_step(intcode_state, input)
    input <- numeric(0)

    if (is.null(intcode_state$output)) {

        if (interactive) cat(intToUtf8(output))

        input_chr <- if (interactive) {
            readline()
        } else {
            solution_idx <- regexpr("[0-9]{10}", intToUtf8(output))
            if (solution_idx != -1) {
                solution_chr <- substr(intToUtf8(output), solution_idx, solution_idx + 9)
                "q"
            } else {
                command_ptr <- command_ptr + 1
                commands[command_ptr]
            }
        }

        if (input_chr == "q") break

        input <- c(utf8ToInt(input_chr), 10)
        output <- numeric(0)

    } else {
        output <- c(output, intcode_state$output)
    }

})

if (interactive) solution_chr <- readline("Solution? ")
solution_1 <- as.numeric(solution_chr)

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output25.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## There is no part 2 :)
