## Advent of Code 2019, Day 5
## https://adventofcode.com/2019/day/5
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the test program
mmry <- as.numeric(strsplit(readLines("input/input5.txt"), ",")[[1]])

## Solution for both parts of the assignment, modifying my Day 2 solution
## The final memory returned does not matter, only the printed output
run_intcode <- function(inp, mmry) {

    ptr <- 1

    repeat({

        ## Parse opcode, parameter modes and parameters from the instruction
        parmodes_opcode <- mmry[ptr]

        opcode <- as.character(parmodes_opcode %% 100)

        n_params <- switch(opcode,
                           "1" = 3, "2" = 3, "3" = 1, "4" = 1,
                           "5" = 2, "6" = 2, "7" = 3, "8" = 3,
                           "99" = 0)

        parmodes <- sapply(10 ** (seq_len(n_params) + 1),
                           function(x) floor(parmodes_opcode / x) %% 10)

        params <- mmry[ptr+seq_len(n_params)]

        ## Function to retrieve the value of parameter N, depending on mode
        ## Do not use for write positions, those are directly given as positions
        get_val <- function(parn) {
            if (parmodes[parn] == 1) params[parn] else mmry[params[parn] + 1]
        }

        ## Perform the operation according to the instruction
        next_instruction_ptr <- ptr + n_params + 1

        ptr <-
            if (opcode == "1") {
                mmry[params[3] + 1] <- get_val(1) + get_val(2)
                next_instruction_ptr
            } else if (opcode == "2") {
                mmry[params[3] + 1] <- get_val(1) * get_val(2)
                next_instruction_ptr
            } else if (opcode == "3") {
                mmry[params[1] + 1] <- inp
                next_instruction_ptr
            } else if(opcode == "4") {
                cat(get_val(1), " ")
                next_instruction_ptr
            } else if (opcode == "5") {
                if (get_val(1) != 0) get_val(2) + 1 else next_instruction_ptr
            } else if (opcode == "6") {
                if (get_val(1) == 0) get_val(2) + 1 else next_instruction_ptr
            } else if (opcode == "7") {
                mmry[params[3] + 1] <- as.numeric(get_val(1) < get_val(2))
                next_instruction_ptr
            } else if (opcode == "8") {
                mmry[params[3] + 1] <- as.numeric(get_val(1) == get_val(2))
                next_instruction_ptr
            }

        ## Stop if we encounter opcode 99 or when we reach the end
        if (opcode == "99" || ptr > length(mmry)) {
            cat("\n\n")
            break
        }

    })

}

cat("-- Part 1 --\n")
final_mmry <- run_intcode(1, mmry)

cat("-- Part 2 --\n")
final_mmry <- run_intcode(5, mmry)
