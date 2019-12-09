## Advent of Code 2019, Day 9
## https://adventofcode.com/2019/day/9
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the program
program <- as.numeric(strsplit(readLines("input/input9.txt"), ",")[[1]])

## Changes from intcode computer of Day 7:
##  - Explicitly regard one run as a 'step'; operation might be resumed
##  - Memory and input pointers are now 0-based, for consistency
##  - The state returned now includes the output
##  - Opcode 9, Relative Mode and Relative Base State are implemented
##  - Memory is extended with 0s as needed upon reading and writing
run_intcode_step <- function(inputs, state) {

    ## Initialisation of operational variables
    mmry <- state$mmry
    mmry_ptr <- state$mmry_ptr
    relative_base <- state$relative_base
    inp_ptr <- 0
    output <- NULL

    ## Function to extend memory as needed
    extend_mmry <- function(max_length) {
        mmry <<- c(mmry, rep(0, max_length - length(mmry)))
    }

    ## Read and write functions, directly to a 0-based memory position
    read_mmry <- function(pos) {
        if ((max(pos) + 1) > length(mmry)) extend_mmry(max(pos) + 1)
        mmry[pos + 1]
    }

    write_mmry <- function(pos, val) {
        if ((max(pos) + 1) > length(mmry)) extend_mmry(max(pos) + 1)
        mmry[pos + 1] <<- val
    }

    ## Run through the program
    repeat ({

        ## Parse opcode, parameter modes and parameters from the instruction
        parmodes_opcode <- read_mmry(mmry_ptr)

        opcode <- as.character(parmodes_opcode %% 100)

        n_params <- switch(opcode,
                           "1" = 3, "2" = 3, "3" = 1, "4" = 1,
                           "5" = 2, "6" = 2, "7" = 3, "8" = 3,
                           "9" = 1, "99" = 0)

        parmodes <- sapply(10 ** (seq_len(n_params) + 1),
                           function(x) floor(parmodes_opcode / x) %% 10)

        params <- if (n_params > 0) read_mmry(mmry_ptr + seq_len(n_params))

        ## Read and write functions, using parameter numbers and modes
        get_val <- function(parn) {
            if (parmodes[parn] == 1) {
                params[parn]
            } else if (parmodes[parn] == 0) {
                read_mmry(params[parn])
            } else if (parmodes[parn] == 2) {
                read_mmry(params[parn] + relative_base)
            }
        }

        set_val <- function(parn, val) {
            if (parmodes[parn] == 0) {
                write_mmry(params[parn], val)
            } else if (parmodes[parn] == 2) {
                write_mmry(params[parn] + relative_base, val)
            }
        }

        ## Perform the operation according to the instruction
        ## Then return a pointer to the next instruction
        next_instruction_ptr <- mmry_ptr + n_params + 1

        mmry_ptr <-
            if (opcode == "1") {
                ## addition
                set_val(3, get_val(1) + get_val(2))
                next_instruction_ptr
            } else if (opcode == "2") {
                ## multiplication
                set_val(3, get_val(1) * get_val(2))
                next_instruction_ptr
            } else if (opcode == "3") {
                ## input
                set_val(1, inputs[inp_ptr + 1])
                inp_ptr <- inp_ptr + 1
                next_instruction_ptr
            } else if (opcode == "4") {
                ## output
                output <- get_val(1)
                next_instruction_ptr
            } else if (opcode == "5") {
                ## jump_if_true
                if (get_val(1) != 0) get_val(2) else next_instruction_ptr
            } else if (opcode == "6") {
                ## jump_if_false
                if (get_val(1) == 0) get_val(2) else next_instruction_ptr
            } else if (opcode == "7") {
                ## less_than
                set_val(3, as.numeric(get_val(1) < get_val(2)))
                next_instruction_ptr
            } else if (opcode == "8") {
                ## equals
                set_val(3, as.numeric(get_val(1) == get_val(2)))
                next_instruction_ptr
            } else if (opcode == "9") {
                ## adjust_relative_base
                relative_base <- relative_base + get_val(1)
                next_instruction_ptr
            }

        ## Stop on 4 (pause) or 99 (halt)
        if (opcode %in% c("99", "4")) {
            break
        }

    })

    list(mmry = mmry,
         mmry_ptr = mmry_ptr,
         relative_base = relative_base,
         output = output,
         halted = (opcode == "99"))

}

## Run a full program, with inputs used for the first step only
## After each output the execution is resumed until it halts
run_intcode_program <- function(inputs, program) {

    output <- NULL
    state <- list(mmry = program, mmry_ptr = 0, relative_base = 0)

    repeat ({
        state <- run_intcode_step(inputs, state)
        output <- c(output, state$output)
        inputs <- integer(0)
        if (state$halted) break
    })

    output

}

solution_1 <- run_intcode_program(1, program)
cat("Solution to Part 1:", solution_1, "\n")

solution_2 <- run_intcode_program(2, program)
cat("Solution to Part 2:", solution_2, "\n")
