## Advent of Code 2019, Day 11
## https://adventofcode.com/2019/day/11
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Since the intcode computer is finished now I'll keep it separate
source("run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input11.txt"), ",")[[1]])

## -- PART 1 --
## Colour state of the ship panels is global
## Absent panel coordinates key means the colour is 0
painted_panels <- list()

## Function to make (x,y) coordinates into a string (to use as a key)
ccoord <- function(x, y) paste(x, y, sep = ",")

## Function to get current colour, paint, turn, and move
paint_robot_step <- function(state) {

    ## Get the current colour from the camera
    current_colour <- painted_panels[[ccoord(state$pos_x, state$pos_y)]]
    if (is.null(current_colour)) current_colour <- 0

    ## Compute the new colour and paint it if needed
    intcode_state <- run_intcode_step(state$intcode_state, current_colour)
    new_colour <- intcode_state$output

    if (!is.null(new_colour) && new_colour != current_colour) {
        painted_panels[[ccoord(state$pos_x, state$pos_y)]] <<- new_colour
    }

    ## Determine new direction, then turn and move
    intcode_state <- run_intcode_step(intcode_state)

    if (!is.null(intcode_state$output)) {

        turn_angle <-  (pi / 2) * 2 * (intcode_state$output - 0.5)
        new_direction <- (state$direction - turn_angle) %% (2 * pi)

        list(pos_x = state$pos_x + round(cos(new_direction)),
             pos_y = state$pos_y + round(sin(new_direction)),
             direction = new_direction,
             intcode_state = intcode_state)

    } else {

        list(pos_x = state$pos_x,
             pos_y = state$pos_y,
             direction = state$direction,
             intcode_state = intcode_state)

    }

}

## Function to run the full program on the paint robot
paint_robot_run <- function(initial_state) {
    robot_state <- initial_state
    repeat ({
        robot_state <- paint_robot_step(robot_state)
        if (robot_state$intcode_state$halted == TRUE) break
    })
}

## Start from (0,0) with direction UP (pi/2), then run the program
initial_robot_state <-
    list(pos_x = 0, pos_y = 0,
         direction = pi / 2,
         intcode_state = list(mmry = program))

paint_robot_run(initial_robot_state)

## Every panel present in the final list has been painted at least once
solution_1 <- length(painted_panels)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Set the (0,0) start panel to white and run the program again
painted_panels <- list("0,0" = 1)
paint_robot_run(initial_robot_state)

## Visualise
## Now the data structure for the coordinates is annoying, but hey ho
white_panels <- painted_panels[painted_panels == 1]
white_panels_m <-
    matrix(as.numeric(unlist(strsplit(names(white_panels), ","))),
           ncol = 2, byrow = TRUE)

plot(white_panels_m[,1], white_panels_m[,2], pch = 15, cex = 3.5, asp = 1)
