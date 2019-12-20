## Advent of Code 2019, Day 18
## https://adventofcode.com/2019/day/18
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

options(stringsAsFactors = FALSE)
set.seed(0)

## Read the maze input
## Discovery status while traversing:
## -1 = wall, 0 = unvisited, 1 = visited one-way, 2 = visited two-way
maze_vec <- unlist(strsplit(readLines("input/input18.txt"), ""))
dm <- sqrt(length(maze_vec))
initial_maze <- expand.grid(x = 1:dm, y = 1:dm)
initial_maze$tile <- maze_vec
initial_maze$status <- 0
initial_maze[initial_maze$tile == "#",]$status <- -1

## Definitions of directions (for convenience)
directions <- list(south = c(0, 1),
                   north = c(0, -1),
                   west = c(-1, 0),
                   east = c(1, 0))

## Function to render the map in stdout
## Incl discovery status
draw_maze <- function(maze) {

    if (any(maze$status == 1 & maze$tile == ".")) {
        maze[maze$status == 1 & maze$tile == ".",]$tile <- "-"
    }

    if (any(maze$status == 2 & maze$tile == ".")) {
        maze[maze$status == 2 & maze$tile == ".",]$tile <- "*"
    }

    mzm <- matrix(maze$tile, ncol = length(unique(maze$y)), byrow = TRUE)
    mzm <- mzm[,seq(ncol(mzm))]
    apply(mzm, 2, function(x) cat(x, "\n"))
    cat("\n\n")

    invisible()

}

## -- PART 1 --
## Function to get candidate forward positions around the current position
get_candidates <- function(maze, pos) {

    dir_statuses <- lapply(directions, function(x) {
        new_pos <- pos + x
        idx <- which(maze$x == new_pos[1] & maze$y == new_pos[2])
        maze[idx,]$status
    })

    dir_statuses[sapply(dir_statuses, length) == 1 &
                 !(dir_statuses %in% c(-1, 2))]

}

## Function to discover the shortest distance between two tiles
trace_steps <- function(maze, start_tile, target_tile) {

    pos <- as.numeric(maze[maze$tile == start_tile, 1:2])

    repeat ({

        current_idx <- maze$x == pos[1] & maze$y == pos[2]
        current_tile <- maze[current_idx,]$tile
        current_status <- maze[current_idx,]$status

        ## stop if: destination reached
        if (current_tile == target_tile) break
        ## stop if: only start tile remains (target not found)
        if (sum(maze$status %in% c(0, 1)) == 1) break

        cand_dirs <- get_candidates(maze, pos)

        if (current_status == 0 & (length(cand_dirs) != 1 || current_tile == start_tile)) {
            ## undiscovered, not a deadend OR start -> visited once
            maze[current_idx,]$status <- 1
        } else if (length(cand_dirs) == 1 & current_tile != start_tile) {
            ## deadend and not start -> visited both ways, don't come back
            maze[current_idx,]$status <- 2
        }

        dirn <- if (any(cand_dirs == 0)) {
                    ## prefer unvisited locations
                    sample(names(cand_dirs[cand_dirs == 0]), size = 1)
                } else {
                    ## if not, travel where we've been before
                    sample(names(cand_dirs), size = 1)
                }

        ## move
        pos <- pos + directions[[dirn]]

    })

    list(n_steps = sum(maze$status == 1),
         keys = maze[(maze$tile %in% letters) & maze$status == 1,]$tile,
         doors = maze[(maze$tile %in% LETTERS) & maze$status == 1,]$tile)

}

## The maze is separated in four quarters with a common midzone
## So we can trace within each quarter, then compute the distances between quarters
quarter_defs <-
    list(mq1 = list(x = seq(1, 40), y = seq(1, 40), start = c(40, 40)),
         mq2 = list(x = seq(42, 81), y = seq(1, 40), start = c(42, 40)),
         mq3 = list(x = seq(42, 81), y = seq(42, 81), start = c(42, 42)),
         mq4 = list(x = seq(1, 40), y = seq(42, 80), start = c(40, 42)))

trace_quarter <- function(qd, initial) {

    maze <- initial[initial$x %in% qd$x & initial$y %in% qd$y,]
    maze[maze$x == qd$start[1] & maze$y == qd$start[2], ]$tile <- "@"

    combs <- expand.grid(from = c("@", letters),
                         to = letters,
                         stringsAsFactors = FALSE)

    combs <- combs[combs$from != combs$to,]
    combs <- combs[combs$from %in% maze$tile & combs$to %in% maze$tile,]

    res <- data.frame(from = character(0), to = character(0), n_steps = numeric(0), doors = character(0))
    for (idx in seq(nrow(combs))) {
        out <- trace_steps(maze, combs[idx,]$from, combs[idx,]$to)
        res <- rbind(res, data.frame(from = combs[idx,]$from,
                                     to = combs[idx,]$to,
                                     n_steps = out$n_steps,
                                     ## keys discovered along the way
                                     keys = paste(out$keys, collapse = ""),
                                     ## doors encountered along the way
                                     doors = paste(out$doors, collapse = "")))
    }

    res

}

quarters_traced <- lapply(quarter_defs, trace_quarter, initial = initial_maze)

## Now combine the quarters into one big data frame
## Distances between quarters depend on the extra_steps distance between them
combine_quarters <- function(qx, qy, extra_steps) {

    qxy <- merge(qx[qx$from == "@",], qy[qy$from == "@",],
                 by = character(0), suffixes = c(".x", ".y"))

    rbind(data.frame(from = qxy$to.x, to = qxy$to.y,
                     n_steps = qxy$n_steps.x + qxy$n_steps.y + extra_steps,
                     keys = paste0(qxy$keys.x, qxy$keys.y),
                     doors = paste0(qxy$doors.x, qxy$doors.y)),
          data.frame(from = qxy$to.y, to = qxy$to.x,
                     n_steps = qxy$n_steps.x + qxy$n_steps.y + extra_steps,
                     keys = paste0(qxy$keys.x, qxy$keys.y),
                     doors = paste0(qxy$doors.x, qxy$doors.y)))

}

tr <- rbind(quarters_traced$mq1, quarters_traced$mq2,
            quarters_traced$mq3, quarters_traced$mq4,
            combine_quarters(quarters_traced$mq1, quarters_traced$mq2, 2),
            combine_quarters(quarters_traced$mq1, quarters_traced$mq3, 4),
            combine_quarters(quarters_traced$mq1, quarters_traced$mq4, 2),
            combine_quarters(quarters_traced$mq2, quarters_traced$mq3, 2),
            combine_quarters(quarters_traced$mq2, quarters_traced$mq4, 4),
            combine_quarters(quarters_traced$mq3, quarters_traced$mq4, 2))

## 'Move' the start position to the actual center
## And add the target key itself to the discovered keys
tr[tr$from == "@",]$n_steps <- tr[tr$from == "@",]$n_steps + 2
tr$keys <- paste(tr$to, tr$keys, sep = "")

## Find the shortest route through recursion and memoisation.
## To make this kind of deep recursion work with R it is really
## important to keep the function execution environments as small
## as possible. Avoid temporary variables.
memo_env <- new.env(hash = TRUE)

distance_to_keys <- function(this_key, remaining_keys) {

    if (length(remaining_keys) == 0) return(0)

    memo_field <- paste(this_key,
                        paste(sort(remaining_keys), collapse = ""),
                        sep = "|")

    if (!is.null(memo_env[[memo_field]])) {

        memo_env[[memo_field]]

    } else {

        valid_keys <- tr[tr$from == this_key &
                         tr$to %in% remaining_keys &
                         sapply(strsplit(tr[,]$doors, split = ""),
                                function(x) !any(tolower(x) %in% remaining_keys)),]$to

        shortest_distance <- Inf

        for (next_key in valid_keys) {
            dst <- tr[tr$from == this_key & tr$to == next_key,]$n_steps +
                distance_to_keys(next_key, setdiff(remaining_keys, next_key))
            shortest_distance <- min(shortest_distance, dst)
        }

        memo_env[[memo_field]] <<- shortest_distance
        if (startsWith(memo_field, "@")) memo_env <<- new.env()

        shortest_distance

    }

}

solution_1 <- distance_to_keys("@", letters)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## This is greatly simplified by my initial approach of the maze as four
## separated quarters. The other important insight is that 'waiting' for
## another robot to collect a key does not matter. Waiting doesn't cost steps.
## And the distance_to_keys() function already doesn't care about doors for
## which we're not collecting keys, so that's a happy coincidence.
mq1_keys <- unique(unlist(strsplit(quarters_traced$mq1$keys, split = "")))
tr <- quarters_traced$mq1
mq1_steps <- distance_to_keys("@", mq1_keys)

mq2_keys <- unique(unlist(strsplit(quarters_traced$mq2$keys, split = "")))
tr <- quarters_traced$mq2
mq2_steps <- distance_to_keys("@", mq2_keys)

mq3_keys <- unique(unlist(strsplit(quarters_traced$mq3$keys, split = "")))
tr <- quarters_traced$mq3
mq3_steps <- distance_to_keys("@", mq3_keys)

mq4_keys <- unique(unlist(strsplit(quarters_traced$mq4$keys, split = "")))
tr <- quarters_traced$mq4
mq4_steps <- distance_to_keys("@", mq4_keys)

solution_2 <- mq1_steps + mq2_steps + mq3_steps + mq4_steps
cat("Solution to Part 2:", solution_2, "\n")
