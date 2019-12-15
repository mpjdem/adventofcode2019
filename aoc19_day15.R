## Advent of Code 2019, Day 15
## https://adventofcode.com/2019/day/15
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

options(stringsAsFactors = FALSE)

## Load the incode computer
source("run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input15.txt"), ",")[[1]])

## Initial empty map
initial_maze <- data.frame(x = numeric(0),
                           y = numeric(0),
                           tile = character(0))

## What movements mean in terms of (x,y) changes
## Order corresponds to required input integers
directions <- list(north = c(0, 1),
                   south = c(0, -1),
                   west = c(-1, 0),
                   east = c(1, 0))

## Unicode characters to draw for the tiles
tile_symbols <- c(empty = "\U25E6",
                  wall = "\U25A3",
                  robot = "\U25B4",
                  oxygen = "\U25EC",
                  unknown = "\U25A1",
                  start = "\U25CC",
                  deadend = "\U25E6")

## Function to add or update one tile on the map
update_map <- function(pos, tile) {

    idx <- which(pos[1] == maze$x & pos[2] == maze$y)

    if (length(idx) > 0) {
        maze[idx,] <<- list(pos[1], pos[2], tile)
    } else {
        maze <<- rbind(maze,
                       data.frame(x = pos[1],
                                  y = pos[2],
                                  tile = tile))
    }

    all_coords <- expand.grid(x = seq(min(maze$x), max(maze$x)),
                              y = seq(min(maze$y), max(maze$y)))
    maze <<- merge(maze, all_coords, all.y = TRUE)
    rownames(maze) <<- NULL

    if (any(is.na(maze$tile))) {
        maze[is.na(maze$tile),]$tile <<- "unknown"
    }

    maze <<- maze[order(maze$x, maze$y),]

    invisible()

}

## Function to render the map in stdout
draw_map <- function(robot_pos = NULL) {

    maze <- maze

    if (!is.null(robot_pos)) {
        maze[maze$x == robot_pos[1] & maze$y == robot_pos[2],]$tile <- "robot"
    }

    mzmat <- matrix(maze$tile, ncol = length(unique(maze$y)), byrow = TRUE)
    apply(mzmat, 1, function(x) cat(tile_symbols[x], "\n"))
    cat("\n\n")

    invisible()

}

## Function to get candidate positions around the current position
get_candidates <- function(pos) {

    dirs <- lapply(directions, function(x) {
        new_pos <- pos + x
        idx <- which(maze$x == new_pos[1] & maze$y == new_pos[2])
        if (length(idx) == 0) "unknown" else maze[idx,]$tile
    })

    dirs[dirs %in% c("empty", "unknown", "start")]

}

## Function to attempt one move in the maze
attempt_move <- function(robot_state) {

    intcode_state <- robot_state$intcode_state
    pos <- robot_state$pos

    cand_dirs <- get_candidates(pos)

    if (length(cand_dirs) == 0) {
        robot_state$finished <- TRUE
        return(robot_state)
    }

    current_tile <- maze[maze$x == pos[1] & maze$y == pos[2],]$tile
    if (length(cand_dirs) == 1 &&
        !current_tile %in% c("start", "oxygen")) {
        update_map(pos, "deadend")
    }

    intcode_inp <-
        if (any(cand_dirs == "unknown")) {
            sample(names(cand_dirs[cand_dirs == "unknown"]), size = 1)
        } else {
            sample(names(cand_dirs), size = 1)
        }

    new_pos <- pos + directions[[intcode_inp]]

    intcode_state <-
        run_intcode_step(intcode_state,
                         which(names(directions) == intcode_inp))

    if (intcode_state$output == 0) {
        update_map(new_pos, "wall")
    } else if (intcode_state$output == 1) {
        update_map(new_pos, "empty")
        pos <- new_pos
    } else if (intcode_state$output == 2) {
        update_map(new_pos, "oxygen")
        pos <- new_pos
    }

    list(intcode_state = intcode_state,
         pos = pos,
         finished = FALSE)

}

## -- PART 1 --
## Travel the maze, marking deadends, until the oxygen is reached
robot_state <- list(intcode_state = list(mmry = program),
                    pos = c(0,0))
maze <- initial_maze
update_map(robot_state$pos, "start")

repeat ({
    robot_state <- attempt_move(robot_state)
    if ("oxygen" %in% maze$tile) break
})

solution_1 <- sum(maze$tile %in% c("empty", "start"))
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Explore the full map instead of stopping at oxygen
## Remove all the deadends so we travel everywhere again
## Then recurse from the oxygen into all possible directions
## The longest route found is the solution
oxygen_pos <- as.numeric(maze[maze$tile == "oxygen", 1:2])
robot_state <- list(intcode_state = list(mmry = program),
                    pos = c(0,0))
maze <- initial_maze
update_map(robot_state$pos, "start")

repeat ({
    robot_state <- attempt_move(robot_state)
    if (robot_state$finished) break
})

maze[maze$tile == "deadend",]$tile <- "empty"

## Function to get the max path length in every branching direction
max_path_length_from <- function(pos) {

    update_map(pos, "deadend")
    cand_dirs <- get_candidates(pos)

    if (length(cand_dirs) == 0) {
        0
    } else {
        cand_pos <- lapply(directions[names(cand_dirs)], function(x) pos + x)
        max(sapply(cand_pos, function(pos) 1 + max_path_length_from(pos)))
    }

}

solution_2 <- max_path_length_from(oxygen_pos)
cat("Solution to Part 2:", solution_2, "\n")

update_map(c(0,0), "start")
update_map(oxygen_pos, "oxygen")
draw_map()
