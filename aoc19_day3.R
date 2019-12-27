## Advent of Code 2019, Day 3
## https://adventofcode.com/2019/day/3
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the input data
## Parse into lists of (direction, displacement) pairs
parse_dd_pair <- function(as_str) {
    list(direction = substr(as_str, 1, 1),
         displacement = as.integer(substr(as_str, 2, nchar(as_str))))
}

wire_paths <-
    lapply(strsplit(readLines("input/input3.txt"), split = ","),
           function(wire_path) {
               lapply(wire_path,
                      function(as_str) parse_dd_pair(as_str))
           })

## -- PART 1 --
## Translate the (direction, displacement) pairs into (x,y) steps
## Then the cumulative sums of x and y yield the actual coordinates
## Inner join of both path data frames will give the intersections
get_coordinates <- function(wire_path) {

    displacement_steps <-
        lapply(wire_path,
               function(dd_pair) {

                   x_disp <- switch(dd_pair$direction,
                                    R = 1, L = -1, U = 0, D = 0)

                   y_disp <- switch(dd_pair$direction,
                                    R = 0, L = 0, U = -1, D = 1)

                   data.frame(x = rep(x_disp, dd_pair$displacement),
                              y = rep(y_disp, dd_pair$displacement))

               })

    as.data.frame(lapply(do.call(rbind, displacement_steps), cumsum))

}

intersections <- merge(get_coordinates(wire_paths[[1]]),
                       get_coordinates(wire_paths[[2]]),
                       by = c("x", "y"), all = FALSE)

solution_1 <- min(abs(intersections$x) + abs(intersections$y))

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output3_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## Since we already generate the full paths, computing the number of steps
## is easy. We just add an index to the data frames before the merge.
coords1 <- get_coordinates(wire_paths[[1]])
coords2 <- get_coordinates(wire_paths[[2]])

intersections <- merge(cbind(coords1, idx1 = seq(nrow(coords1))),
                       cbind(coords2, idx2 = seq(nrow(coords2))),
                       by = c("x", "y"), all = FALSE)

solution_2 <- min(intersections$idx1 + intersections$idx2)

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output3_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
