## Advent of Code 2019, Day 17
## https://adventofcode.com/2019/day/17
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

options(stringsAsFactors = FALSE)

## Load the incode computer
source("intcode/run_intcode_step.R")

## Get the program
program <- as.numeric(strsplit(readLines("input/input17.txt"), ",")[[1]])

## -- PART 1 --
intcode_state <- list(mmry = program)

## Function to read output until two newlines are received,
## meanwhile registering the characters as (x,y) coordinates
get_scafmap <- function(intcode_state) {

    scafmap <- data.frame(x = numeric(0), y = numeric(0), tile = character(0))
    x <- 0
    y <- 0

    repeat ({

        intcode_state <- run_intcode_step(intcode_state)

        if (intcode_state$output == 10 && x == 0) break

        if (intcode_state$output != 10) {
            new_tile <- intToUtf8(intcode_state$output)
            new_coord <- data.frame(x = x, y = y, tile = new_tile)
            scafmap <- rbind(scafmap, new_coord)
            x <- x + 1
        } else {
            y <- y + 1
            x <- 0
        }

    })

    list(scafmap = scafmap, intcode_state = intcode_state)

}

## Function to print the scafmap to stdout
draw_scafmap <- function(scafmap) {
    n_rows <- max(scafmap$y) + 1
    as_mat <- matrix(scafmap$tile, nrow = n_rows, byrow = TRUE)
    apply(as_mat, 1, function(x) cat(x, "\n"))
    invisible()
}

## Get the scaffolding map and draw it
scmap <- get_scafmap(intcode_state)

## For every #, see if the neighbouring points also have #
scaffolds <- scmap$scafmap[scmap$scafmap$tile == "#",]
rownames(scaffolds) <- seq(nrow(scaffolds))
ix_template <- matrix(c(-1, 0, 0, 1, 1, 0, 0, -1), ncol = 2, byrow = TRUE)

neighbour_coords <-
    cbind(as.data.frame(apply(as.matrix(scaffolds[, 1:2]), 2, rep, each = 4) +
                        apply(ix_template, 2, rep, nrow(scaffolds))),
          idx = rep(seq(nrow(scaffolds)), each = 4))

neighbour_matches <- merge(scaffolds, neighbour_coords,
                           all = FALSE, by = c("x", "y"))

tbl <- table(neighbour_matches$idx)
ix_idx <- as.numeric(names(tbl)[tbl == 4])
ix_found <- scaffolds[ix_idx,]

solution_1 <- sum(ix_found$x * ix_found$y)
cat("Solution to Part 1:", paste(solution_1, collapse = ""), "\n")


## -- PART 2 --

## Function to get the robot position on the scaffolding map
get_robot <- function(scafmap) {
    robrow <- scafmap[!(scafmap$tile %in% c(".", "#")),]
    list(x = robrow$x,
         y = robrow$y,
         or = switch(robrow$tile,
                     "^" = c(0, -1),
                     ">" = c(1, 0),
                     "<" = c(-1, 0),
                     "v" = c(0, 1)))
}

## Function to turn the robot in our simulation of navigating the map
turn_robot <- function(robot, direction) {
    turn <- if (direction == "R") pi/2 else if (direction == "L") -pi/2
    new_ang <- atan2(robot$or[2], robot$or[1]) + turn
    new_or <- round(c(cos(new_ang), sin(new_ang)))
    robot$or <- new_or
    robot
}

## Function to move the robot in our simulation of navigating the map
move_robot <- function(robot, n_steps) {
    robot$x <- robot$x + robot$or[1] * n_steps
    robot$y <- robot$y + robot$or[2] * n_steps
    robot
}

## Function to detect the continuous scaffold length in the line of sight
detect_scaffold_length <- function(scafmap, robot) {

    if (robot$or[1] != 0) {
        dim_const <- "y"; dim_var <- "x"; idx_or <- 1
    } else {
        dim_const <- "x"; dim_var <- "y"; idx_or <- 2
    }

    los <- scafmap[scafmap$tile == "#" &
                       scafmap[[dim_const]] == robot[[dim_const]],]
    los <- los[order(los[[dim_var]] * robot$or[idx_or]),]
    los$tmp <- (los[[dim_var]] - robot[[dim_var]]) * robot$or[idx_or]
    los <- los[los$tmp > 0,]
    los <- los[los$tmp == rank(los$tmp),]

    nrow(los)

}

## Function to get the next instruction in our simulation of navigating the map
next_instruction <- function(scafmap, robot) {

    scl <- detect_scaffold_length(scafmap, robot)
    if (scl > 0) return(scl)

    scl <- detect_scaffold_length(scafmap, turn_robot(robot, "L"))
    if (scl > 0) return("L")

    scl <- detect_scaffold_length(scafmap, turn_robot(robot, "R"))
    if (scl > 0) return("R")

    NULL

}

## Function to score possible functions given the full movement path
## as well as the existing functions. It will prefer functions that reduce
## the number of characters the most and occur frequently.
get_function_scores <- function(mv_vec, existing_fn) {

    mv_str <- paste(mv_vec, collapse = ",")
    for (fn in existing_fn) mv_str <- gsub(fn, "X", mv_str)

    fns <- list()
    fn_lengths <- seq(10, 2, by = -2)

    for (fn_ln in fn_lengths) {
        offsets <- seq(1, length(mv_vec) - fn_ln, by = 2)
        for (os in offsets) {
            cand <- mv_vec[os:(os + fn_ln - 1)]
            fn_str <- paste(cand, collapse = ",")
            if (nchar(fn_str) > 20) next
            matches <- gregexpr(fn_str, mv_str)[[1]]
            if (!identical(attr(matches, "match.length"), -1L)) {
                fns[[fn_str]] <-
                    data.frame(fn_str = fn_str,
                               fn_cln = nchar(fn_str),
                               n_matches = length(matches),
                               score = (nchar(fn_str) / 2) * length(matches))
            }
        }
    }

    fns <- do.call(rbind, fns)

}

## Function to read the text of a prompt from the robot, and provide input
fill_prompt <- function(intcode_state, input) {

    cat(intToUtf8(intcode_state$output))

    repeat ({
        intcode_state <- run_intcode_step(intcode_state)
        cat(intToUtf8(intcode_state$output))
        if (intcode_state$output == 10) break
    })

    cat(intToUtf8(input), " \n")
    intcode_state <- run_intcode_step(intcode_state, input)

    intcode_state

}

## First, determine the full path of the robot
## We use the map obtained in Part 1 for this
scafmap <- scmap$scafmap
robot <- get_robot(scafmap)
mv_vec <- character(0)

repeat ({

    instr <- next_instruction(scafmap, robot)
    if (is.null(instr)) break

    robot <- if (is.numeric(instr)) {
        move_robot(robot, instr)
    } else {
        turn_robot(robot, instr)
    }

    mv_vec <- c(mv_vec, as.character(instr))

})

mv_str <- paste(mv_vec, collapse = ",")

cat(" \nFull path:", mv_str, "\n \n")

## Next, determine the functions that best compress the path
## Until no functions can be found anymore
extracted_fns <- character(0)
repeat ({

    fn_scores <- get_function_scores(mv_vec, extracted_fns)

    if (is.null(fn_scores)) break

    fn_scores <- fn_scores[order(fn_scores$score,
                                 fn_scores$n_matches,
                                 decreasing = TRUE),]

    extracted_fns <- c(extracted_fns, fn_scores$fn_str[1])

})

names(extracted_fns) <- LETTERS[seq(length(extracted_fns))]

for (fni in seq_along(extracted_fns)) {
    nm <- names(extracted_fns[fni])
    fn <- extracted_fns[fni]
    cat("Function", nm, ":", fn, "\n \n")
    mv_str <- gsub(fn, nm, mv_str)
}

cat("Compressed path:", mv_str, "\n")

## Finally, give the instructions to the robot
## It will first return the scaffolding map, then take the prompts
## Then it will return the scaffolding map again
## And finally the solution
intcode_state <- list(mmry = program)
intcode_state$mmry[1] <- 2

cat(" \n**START STATE**\n")
scmap1 <- get_scafmap(intcode_state)

draw_scafmap(scmap1$scafmap)
cat(" \n**PROMPTS**\n")

intcode_state <- fill_prompt(scmap1$intcode_state, c(utf8ToInt(mv_str), 10))
for (fn in extracted_fns) {
    intcode_state <- fill_prompt(intcode_state, c(utf8ToInt(fn), 10))
}
intcode_state <- fill_prompt(intcode_state, c(utf8ToInt("n"), 10))
scmap2 <- get_scafmap(intcode_state)

cat(" \n**END STATE**\n")
draw_scafmap(scmap2$scafmap)

solution_2 <- run_intcode_step(scmap2$intcode_state)$output
cat("Solution to Part 2:", paste(solution_2, collapse = ""), "\n")
