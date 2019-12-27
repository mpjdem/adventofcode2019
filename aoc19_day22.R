## Advent of Code 2019, Day 22
## https://adventofcode.com/2019/day/22
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the shuffling instructions
inp <- readLines("input/input22.txt")

## -- PART 1 --
into_new <- function(cards) {
    rev(cards)
}

cut_n <- function(cards, n) {
    if (n < 0) n <- length(cards) + n
    c(cards[(n + 1):length(cards)], cards[1:n])
}

increment_n <- function(cards, n) {
    nc <- length(cards)
    new_pos <- 1 + (seq(0, (nc * n) - 1, by = n) %% nc)
    cards[order(new_pos)]
}

shuffle_cards <- function(n_cards, steps) {

    cards <- seq(n_cards) - 1

    for (step in steps) {

        idx <- regexpr("[0-9\\-]+", step)
        n <- if (idx != -1) as.numeric(substr(step, idx, nchar(step)))

        cards <-
            if (grepl("into new", step)) {
                into_new(cards)
            } else if (grepl("cut", step)) {
                cut_n(cards, n)
            } else if (grepl("increment", step)) {
                increment_n(cards, n)
            }

    }

    cards

}

cards <- shuffle_cards(10007, inp)
solution_1 <- which(cards == 2019) - 1

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output22_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## The mathematical solution is taken from reddit - this is an R implementation.
## We have to use {gmp} to handle large integers, using <num> gives incorrect results
## when I implement the modular exponentiation myself (despite correct algo).
n_cards <- 119315717514047
n_reps <- 101741582076661

## We need to know which card will be in position 2020
## That is, if a card is in pos 2020 at the end, where did it originally come from?
## We describe the reverse of every technique as a function of the resulting pos.
## All techniques in the shuffle sequence are linear functions that can be
## combined into one overall linear function. That is, pos_prev = pos_next * a + b
a <- 1
b <- 0
for (step in rev(inp)) {
    idx <- regexpr("[0-9\\-]+", step)
    n <- if (idx != -1) as.numeric(substr(step, idx, nchar(step)))
    if (grepl("into new", step)) {
        b <- b + 1
        a <- a * -1
        b <- b * -1
    } else if (grepl("cut", step)) {
        b <- b + n
    } else if (grepl("increment", step)) {
        p <- gmp::powm(n, n_cards - 2, n_cards)
        a <- a * p
        b <- b * p
    }
    a <- a %% n_cards
    b <- b %% n_cards
}

## Fancy mathy simplification can tell us what would happen after n_reps of shuffles
solution_2 <-
    ((gmp::powm(a, n_reps, n_cards) * 2020) +
     (b * (gmp::powm(a, n_reps, n_cards) + n_cards - 1) *
          (gmp::powm(a - 1, n_cards - 2, n_cards)))
    ) %% n_cards

cat("Solution to Part 2:", format(solution_2, scientific = FALSE), "- ")
check_2 <- as.numeric(readLines("output/output22_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
