## Advent of Code 2019, Day 7
## https://adventofcode.com/2019/day/7
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the program
program <- as.numeric(strsplit(readLines("input/input7.txt"), ",")[[1]])

## Main changes from the intcode computer of Day 5
##   - Multiple inputs are now supported, using an input pointer to keep track
##   - Input and output state now contain: memory, its pointer, 'finished' flag
##   - Output is no longer sent to stdout, but collected in a global buffer
##     (hurray for lexical scoping!)
##     This way we still support intermediate output, as in previous days
##   - Opcode 4 /pauses/, returning the state but not flagging as finished
##   - Opcode 99 /halts/ execution, flagging the state as finished
global_output_buffer <- NULL

run_intcode <- function(inputs, state) {

    mmry <- state$mmry
    mmry_ptr <- state$mmry_ptr
    inp_ptr <- 1

    repeat ({
        ## Parse opcode, parameter modes and parameters from the instruction
        parmodes_opcode <- mmry[mmry_ptr]

        opcode <- as.character(parmodes_opcode %% 100)

        n_params <- switch(opcode,
                           "1" = 3, "2" = 3, "3" = 1, "4" = 1,
                           "5" = 2, "6" = 2, "7" = 3, "8" = 3,
                           "99" = 0)

        parmodes <- sapply(10 ** (seq_len(n_params) + 1),
                           function(x) floor(parmodes_opcode / x) %% 10)

        params <- mmry[mmry_ptr + seq_len(n_params)]

        ## Function to retrieve the value of parameter N, depending on mode
        ## Don't use for write positions, those are directly given as positions
        get_val <- function(parn) {
            if (parmodes[parn] == 1) params[parn] else mmry[params[parn] + 1]
        }

        ## Perform the operation according to the instruction
        next_instruction_ptr <- mmry_ptr + n_params + 1

        mmry_ptr <-
            if (opcode == "1") {
                ## addition
                mmry[params[3] + 1] <- get_val(1) + get_val(2)
                next_instruction_ptr
            } else if (opcode == "2") {
                ## multiplication
                mmry[params[3] + 1] <- get_val(1) * get_val(2)
                next_instruction_ptr
            } else if (opcode == "3") {
                ## input
                mmry[params[1] + 1] <- inputs[inp_ptr]
                inp_ptr <- inp_ptr + 1
                next_instruction_ptr
            } else if (opcode == "4") {
                ## output
                global_output_buffer <<- c(global_output_buffer, get_val(1))
                next_instruction_ptr
            } else if (opcode == "5") {
                ## jump_if_true
                if (get_val(1) != 0) get_val(2) + 1 else next_instruction_ptr
            } else if (opcode == "6") {
                ## jump_if_false
                if (get_val(1) == 0) get_val(2) + 1 else next_instruction_ptr
            } else if (opcode == "7") {
                ## less_than
                mmry[params[3] + 1] <- as.numeric(get_val(1) < get_val(2))
                next_instruction_ptr
            } else if (opcode == "8") {
                ## equals
                mmry[params[3] + 1] <- as.numeric(get_val(1) == get_val(2))
                next_instruction_ptr
            }

        ## Stop on 4 (pause), 99 (halt) or the end of memory (halt)
        if (opcode %in% c("99", "4") || mmry_ptr > length(mmry)) {
            break
        }

    })

    list(mmry = mmry,
         mmry_ptr = mmry_ptr,
         finished = (opcode == 99) || (mmry_ptr > length(mmry)))

}

## Function to run a single amp
run_amp <- function(input_instructions, amp_state_in) {
    amp_state_out <- run_intcode(input_instructions, amp_state_in)
    list(amp_state = amp_state_out,
         output = tail(global_output_buffer, 1))
}

## Function to run one strictly serial amp sequence
## `first_inputs` is used to prepend each amp's input with its phase setting
run_amp_seq <- function(n_amps, amp_seq_input, first_inputs, amp_states) {

    next_input <- amp_seq_input

    for (ampn in seq(n_amps)) {

        input_instructions <- if (length(first_inputs)) {
            c(first_inputs[ampn], next_input)
        } else {
            next_input
        }

        res <- run_amp(input_instructions, amp_states[[ampn]])

        next_input <- res$output
        amp_states[[ampn]] <- res$amp_state

    }

    list(output = tail(global_output_buffer, 1),
         amp_states = amp_states)

}

## -- PART 1 --
## A very lazy approach to permutations without repetitions:
## permutations with repetitions, then filter out repetitions
all_combinations <-
    Filter(function(x) !any(duplicated(x)),
           split(as.matrix(expand.grid(0:4, 0:4, 0:4, 0:4, 0:4)),
                 seq_len(5**5)))

## Try out all combinations with one strictly serial amp sequence
initial_amp_state <- list(mmry = program,
                          mmry_ptr = 1,
                          finished = FALSE)

solution_1 <-
    max(sapply(all_combinations,
               function(x) run_amp_seq(5, 0, x,
                                       rep(list(initial_amp_state), 5))$output))

cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Function to run the amp sequence with a feedback loop
## Phase settings should only be applied in the first iteration
## The last amp has the powah to signal that operation has halted
run_amp_fb <- function(initial_input, phase_settings, initial_amp_states) {

    n_amps <- length(phase_settings)
    amp_states <- initial_amp_states
    feedback_input <- initial_input

    repeat ({
        res <- run_amp_seq(n_amps, feedback_input, phase_settings, amp_states)
        feedback_input <- res$output
        amp_states <- res$amp_states
        phase_settings <- integer(0)
        if (amp_states[[n_amps]]$finished) break
    })

    res$output

}

all_combinations <-
    Filter(function(x) !any(duplicated(x)),
           split(as.matrix(expand.grid(5:9, 5:9, 5:9, 5:9, 5:9)),
                 seq_len(5**5)))

solution_2 <-
    max(sapply(all_combinations,
               function(x) run_amp_fb(0, x,
                                      rep(list(initial_amp_state), 5))))

cat("Solution to Part 2:", solution_2, "\n")
