## Advent of Code 2019, Day 10
## https://adventofcode.com/2019/day/10
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the input data as 0-based (col,row) indices for (x,y)
inp <- which(t(sapply(strsplit(readLines("input/input10.txt"), ""),
                      function(x) x == "#")),
             arr.ind = TRUE) - 1

inp <- cbind(id = 1:nrow(inp), as.data.frame(inp))

## Create a data frame with all combinations of asteroids
df <- merge(inp, inp, by = character(0), suffixes = c("_from", "_to"))
df <- df[df$id_from != df$id_to,]

## Compute Line-Of-Sight distances and angles for all combinations
los_distance <- function(x1, x2, y1, y2) sqrt((x2 - x1)**2 + (y2 - y1)**2)
los_angle <- function(x1, x2, y1, y2) atan2(x2 - x1, y2 - y1)

df$distance <- los_distance(df$col_from, df$col_to, df$row_from, df$row_to)
df$angle <- los_angle(df$col_from, df$col_to, df$row_from, df$row_to)

## -- PART 1 --
## For every asteroid, compute the number of unique angles to others
unique_angles <- aggregate(angle ~ id_from, data = df,
                           function(x) length(unique(x)))

solution_1 <- max(unique_angles$angle)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
## Filter the data frame to contain only the relevant 'from' asteroid
station_id <- unique_angles[which.max(unique_angles$angle),]$id_from
df_station <- df[df$id_from == station_id,]

## Calculate the relevant orders (angle: pi to -pi, distance: increasing)
## Final sort to take the first of each angle group, then the second, etc.
df_station <- df_station[order(-df_station$angle, df_station$distance),]

angle_groups <- cumsum(!duplicated(df_station$angle))

dist_order <- Reduce(function(x,y)  (x * y) + y,
                     duplicated(df_station$angle),
                     accumulate = TRUE)

df_station <- df_station[order(dist_order, angle_groups),]

## Take the 200th row and calculate the number for the solution
solution_2 <- df_station[200,]$row_to + df_station[200,]$col_to * 100
cat("Solution to Part 2:", solution_2, "\n")
