#' Run intcode program from its current state, until paused (4) or halted (99)
#'
#' @param state <list> Input state, containing at least `mmry`
#' @param inputs <int> Vector to be consumed elementwise through opcode 3 (input)
#'
#' @return <list> Output state
#'
run_intcode_step <- function(state, inputs = numeric(0)) {

    ## Initialisation of operational variables
    mmry <- state$mmry
    mmry_ptr <- if (!is.null(state$mmry_ptr)) state$mmry_ptr else 0
    rel_base <- if (!is.null(state$rel_base)) state$rel_base else 0
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
                read_mmry(params[parn] + rel_base)
            }
        }

        set_val <- function(parn, val) {
            if (parmodes[parn] == 0) {
                write_mmry(params[parn], val)
            } else if (parmodes[parn] == 2) {
                write_mmry(params[parn] + rel_base, val)
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
                ## adjust_rel_base
                rel_base <- rel_base + get_val(1)
                next_instruction_ptr
            } else {
                ## halt
                mmry_ptr
            }

        ## Stop on 4 (pause) or 99 (halt)
        if (opcode %in% c("99", "4")) {
            break
        }

    })

    list(mmry = mmry,
         mmry_ptr = mmry_ptr,
         rel_base = rel_base,
         output = output,
         halted = (opcode == "99"))

}
