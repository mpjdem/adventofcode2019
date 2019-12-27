## Advent of Code 2019, Day 13
## https://adventofcode.com/2019/day/13
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Load the incode computer
source("intcode/run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input13.txt"), ",")[[1]])

## Define characters to draw for the tiles
tile_symbols <- c(empty = "\U25E6",
                  wall = "\U25A3",
                  block = "\U25A2",
                  paddle = "\U25AC",
                  ball = "\U25CE" )

## Define keys corresponding to the joystick positions
joystick <- c("a" = -1, "s" = 0, "d" = 1)

## Sleep time between screen updates; -1 for no visuals (much faster)
## Use a proper terminal for the best experience
sleep_time <- 0.1

## -- PART 1 --

## Function to run one step in the game
## Initially the entire screen is produced, later only updates
run_game_step <- function(intcode_state, input = numeric(0), initial = FALSE) {
    outv <- numeric(0)
    repeat ({
        intcode_state <- run_intcode_step(intcode_state, inputs = input)
        outv <- c(outv, intcode_state$output)
        if (intcode_state$halted ||
            identical(outv[length(outv) - 2], -1) ||
            (!initial && length(outv) == 3)) break
    })

    list(out = matrix(outv, ncol = 3, byrow = TRUE),
         intcode_state = intcode_state)

}

intcode_state <- list(mmry = program)
res <- run_game_step(intcode_state, initial = TRUE)

solution_1 <- sum(res$out[, 3] == 2)

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output13_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
program[1] <- 2
intcode_state <- list(mmry = program)
DRAW_SCR <- FALSE

## Function to draw the screen from the tile coordinates
draw_screen <- function(screen_state, score = 0) {

    if (!DRAW_SCR) return(invisible())

    if (sleep_time == -1) return()
    scr <- matrix(tile_symbols[screen_state[, 3] + 1],
                  ncol = max(screen_state[, 1]) + 1,
                  byrow = TRUE)
    apply(scr, 1, function(x) cat(x, "\n"))
    if (length(score) == 0) score <- 0
    cat("\nScore:", score, "\n\n")
    Sys.sleep(sleep_time)

    invisible()

}

## Initialise the game, drawing a full screen
init_game <- function(program) {

    intcode_state <- list(mmry = program)
    res <- run_game_step(intcode_state, numeric(0), TRUE)

    screen_state <- res$out[res$out[, 1] != -1,]
    score <- res$out[res$out[, 1] == -1, 3]
    paddle_pos <- res$out[res$out[, 3] == 3, 1:2]
    ball_pos <- res$out[res$out[, 3] == 4, 1:2]

    draw_screen(screen_state, score)

    list(screen_state = screen_state,
         intcode_state = res$intcode_state,
         score = score,
         paddle_pos = paddle_pos,
         ball_pos = ball_pos)

}

## Process updates per triplet of outputs
## Screens are only redrawn when something is added rather than removed
game_update <- function(game_state) {

    intcode_state <- game_state$intcode_state
    screen_state <- game_state$screen_state
    keyb_inp <- if (is.null(game_state$keyb_inp)) "s" else game_state$keyb_inp
    score <- if (is.null(game_state$score)) 0 else game_state$score

    res <- run_game_step(intcode_state, joystick[keyb_inp], initial = FALSE)

    if (length(res$out) == 0) {
        draw_screen(screen_state, score)
        if (DRAW_SCR) cat("GAME OVER\n")
        keyb_inp <- "q"
    } else if (res$out[, 1] == -1) {
        score <- res$out[, 3]
        draw_screen(screen_state, score)
    } else {
        idx <- which(screen_state[,1] == res$out[, 1] & screen_state[, 2] == res$out[, 2])
        screen_state[idx,] <- res$out
        if (res$out[, 3] != 0) draw_screen(screen_state, score)
    }

    paddle_pos <- if (3 %in% res$out[, 3]) res$out[res$out[, 3] == 3, 1:2] else game_state$paddle_pos
    ball_pos <- if (4 %in% res$out[, 3]) res$out[res$out[, 3] == 4, 1:2] else game_state$ball_pos

    ## Look, an AI!
    keyb_inp <- if (keyb_inp == "q") {
        "q"
    } else if (paddle_pos[1] < ball_pos[1]) {
        "d"
    } else if (paddle_pos[1] > ball_pos[1]) {
        "a"
    } else {
        "s"
    }

    list(keyb_inp = keyb_inp,
         intcode_state = res$intcode_state,
         screen_state = screen_state,
         score = score,
         ball_pos = ball_pos,
         paddle_pos = paddle_pos)

}

game_state <- init_game(program)
repeat ({
    game_state <- game_update(game_state)
    if (game_state$keyb_inp == "q") break
})

solution_2 <- sum(game_state$score)

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output13_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
