## Advent of Code 2019, Day 23
## https://adventofcode.com/2019/day/23
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Changed the intcode computer for the first time in many days.
## It now pauses when it expects input but it hasn't received any.
source("intcode/run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input23.txt"), ",")[[1]])

## -- PART 1 --
comp_addr <- as.character(0:49)
nat_addr <- as.character(255)
initial_state <- list(intcode_state = list(mmry = program),
                      input_queue = numeric(0),
                      idle = FALSE)

## Function to boot a computer and assign a network address
boot_computer <- function(computers, addr, program) {
    computers[[addr]]$intcode_state <-
        run_intcode_step(computers[[addr]]$intcode_state,  as.numeric(addr))
    computers
}

## Function to start the network for both parts
start_network <- function(initial_state, comp_addr, nat_addr) {
    computers <- rep(list(initial_state), length(comp_addr))
    names(computers) <- as.character(comp_addr)
    computers[[nat_addr]] <- initial_state
    for (addr in comp_addr) {
        computers <- boot_computer(computers, addr, program)
    }
    computers
}

## Function to process incoming packets and output new ones.
## When the computer has no inputs and no longer outputs anything, go idle.
## Only one packet can be received, but multiple can be outputted.
receive_and_send <- function(computers, addr) {

    comp <- computers[[addr]]

    comp$intcode_state <-
        if (length(comp$input_queue) != 0) {
            comp$idle <- FALSE
            inp <- head(comp$input_queue, 2)
            comp$input_queue <- tail(comp$input_queue, -2)
            run_intcode_step(comp$intcode_state, inp)
        } else {
            comp$idle <- TRUE
            run_intcode_step(comp$intcode_state, -1)
        }

    outp <- comp$intcode_state$output
    while (!is.null(outp)) {

        comp$idle <- FALSE

        if (length(outp) == 3) {
            dest_comp <- computers[[as.character(outp[1])]]
            dest_comp$input_queue <- c(dest_comp$input_queue, outp[2:3])
            computers[[as.character(outp[1])]] <- dest_comp
            outp <- NULL
        }

        comp$intcode_state <- run_intcode_step(comp$intcode_state, -1)
        outp <- c(outp, comp$intcode_state$output)

    }

    computers[[as.character(addr)]] <- comp
    computers

}

## Boot up all computers, and run them until 255 (NAT) receives an input
computers <- start_network(initial_state, comp_addr, nat_addr)
addr_idx <- 0

repeat ({
    addr <- comp_addr[addr_idx + 1]
    computers <- receive_and_send(computers, addr)
    if (length(computers[[nat_addr]]$input_queue != 0)) break
    addr_idx <- (addr_idx + 1) %% length(comp_addr)
})

solution_1 <- computers[[nat_addr]]$input_queue[2]
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
computers <- start_network(initial_state, comp_addr, nat_addr)
nat_log <- data.frame(x = numeric(0), y = numeric(0))
addr_idx <- 0

repeat ({

    addr <- comp_addr[addr_idx + 1]
    computers <- receive_and_send(computers, addr)
    all_idle <- all(sapply(comp_addr, function(x) computers[[x]]$idle))

    if (all_idle && length(computers[["0"]]$input_queue) == 0) {
        xy <-  tail(computers[[nat_addr]]$input_queue, 2)
        computers[["0"]]$input_queue <- xy
        computers[[nat_addr]]$input_queue <- xy
        nat_log <- rbind(nat_log, data.frame(x = xy[1], y = xy[2]))
    }

    if (nrow(nat_log) > 1 && any(diff(nat_log$y) == 0)) break
    addr_idx <- (addr_idx + 1) %% length(comp_addr)

})

solution_2 <- tail(nat_log$y, 1)
cat("Solution to Part 2:", solution_2, "\n")
