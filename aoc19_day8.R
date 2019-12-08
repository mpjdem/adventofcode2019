## Advent of Code 2019, Day 8
## https://adventofcode.com/2019/day/8
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Get the input image
inp <- as.numeric(strsplit(readLines("input/input8.txt"), "")[[1]])
img <- array(inp, dim = c(25, 6, length(inp) / (25 * 6)))

## -- PART 1 --
layer_idx <- which.min(apply(img, 3, function(x) sum(x == 0)))
solution_1 <- sum(img[,,layer_idx] == 1) * sum(img[,,layer_idx] == 2)

cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
stack_two_layers <- function(top, bottom) {
    (top * (top != 2)) + (bottom * (top == 2))
}

img_layers_split <- lapply(seq(dim(img)[3]), function(x) img[,,x])
stacked_img <- Reduce(stack_two_layers, img_layers_split)
coords <- which(stacked_img == 1, arr.ind = TRUE)

plot(coords[,1], -coords[,2], pch = 15, cex = 3.5, asp = 1)
