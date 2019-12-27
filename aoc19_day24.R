## Advent of Code 2019, Day 24
## https://adventofcode.com/2019/day/24
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the puzzle input
## Only store the coordinates that have bugs
inp <- unlist(strsplit(readLines("input/input24.txt"), "")) == "#"
sz <- sqrt(length(inp))
eris0 <- cbind(lvl = 0, expand.grid(x = seq(sz), y = seq(sz)))[inp,]

## Function to compute the linear index from coordinates
lidx <- function(x, y, sz) {
    ((y - 1) * sz) + x
}

## Function to display the current state of Eris in stdout
draw_state <- function(eris, sz, lvl = 0, recursive = FALSE) {
    mat <- matrix(rep(".", sz**2), ncol = sz)
    eris_lvl <- eris[eris$lvl == lvl,]
    mat[lidx(eris_lvl$x, eris_lvl$y, sz)] <- "#"
    if (recursive) mat[ceiling(sz/2),ceiling(sz/2)] <- "?"
    apply(mat, 2, function(x) cat(x, "\n"))
    cat(" \n")
}

## Function to evolve the bug spread for one minute
one_minute <- function(eris, sz) {

    ## In part one we have only one level
    eris <- eris[, c("x", "y")]

    ## Count for every square how many other squares neighbour it
    neighbours <- data.frame(xm = c(-1, 0, 1, 0), ym = c(0, -1, 0, 1))
    nbs <- merge(eris, neighbours, by = character(0))
    nbs$xn <- nbs$x + nbs$xm; nbs$yn <- nbs$y + nbs$ym
    nbs <- nbs[nbs$xn >= 1 & nbs$xn <= sz & nbs$yn >= 1 & nbs$yn <= sz,
               c("xn", "yn")]
    nbs$n <- 1
    nbs <- aggregate(n ~ ., data = nbs, FUN = sum)

    ## bug = 1 and n != 1: bug dies
    ## bug = NA and n = 1 or 2: newly infested
    eris$bug <- 1
    res <- merge(eris, nbs,
                 by.x = c("x", "y"), by.y = c("xn", "yn"), all = TRUE)
    res$bug <- ifelse(is.na(res$bug), 0, 1)
    res$n <- ifelse(is.na(res$n), 0, res$n)
    res$next_bug <- ifelse((res$bug == 0 & res$n %in% c(1, 2)) |
                           (res$bug == 1 & res$n == 1), 1, 0)
    res$lvl <- 0

    res[res$next_bug == 1, c("lvl", "x", "y")]

}

eris_history <- integer(0)
eris <- eris0
repeat ({
    eris_int <- sum(2**(0:24)[lidx(eris$x, eris$y, sz)])
    eris_history <- c(eris_history, eris_int)
    if (any(duplicated(eris_history))) break
    eris <- one_minute(eris, sz)
})

solution_1 <- eris_int

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output24_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## Helper function for subset assignment in a data frame
## Mostly to avoid separate logic each time when the mask is empty
ssa <- function(df, mask, col, vals) {
    if (all(mask == FALSE)) {
        df
    } else if (length(vals) == 1) {
        df[mask, col] <- rep(vals, length.out = sum(mask))
    } else {
        df[mask, col] <- vals[mask]
    }
    df
}

## -- PART 2 --
## Now add the logic for Plutonian recursive levels
one_minute_recursive <- function(eris, sz) {

    neighbours <- data.frame(xm = c(-1, 0, 1, 0), ym = c(0, -1, 0, 1))
    nbs <- merge(eris, neighbours, by = character(0))
    nbs$xn <- nbs$x + nbs$xm; nbs$yn <- nbs$y + nbs$ym

    ## Middle square: Take it to the next level!
    ## xm/ym = 0 is replicated across all rows/cols (1:sz) of the next level
    ## xm/ym = -1 -> sz in the next level, ym/ym = 1 -> 1 in th next level
    mid_idx <- which(nbs$xn == ceiling(sz/2) & nbs$yn == ceiling(sz/2))

    if (length(mid_idx) > 0) {

        mid_nbs <- cbind(do.call(rbind, rep(list(nbs[mid_idx,]), sz)),
                         rep = rep(seq(sz), each = length(mid_idx)))
        mid_nbs$lvl <- mid_nbs$lvl + 1

        mid_nbs <- ssa(mid_nbs, mid_nbs$xm == 0, "xn", mid_nbs$rep)
        mid_nbs <- ssa(mid_nbs, mid_nbs$ym == 0, "yn", mid_nbs$rep)
        mid_nbs$rep <- NULL

        mid_nbs <- ssa(mid_nbs, mid_nbs$xm == -1, "xn", sz)
        mid_nbs <- ssa(mid_nbs, mid_nbs$xm == 1, "xn", 1)
        mid_nbs <- ssa(mid_nbs, mid_nbs$ym == -1, "yn", sz)
        mid_nbs <- ssa(mid_nbs, mid_nbs$ym == 1, "yn", 1)

        nbs <- rbind(nbs[-mid_idx,], mid_nbs)

    }

    ## Outside of the grid: Take it to the previous level!
    ## That is, to the (3,4), (3,2), (2,3), (4,3) squares
    out_idx <- which(nbs$xn < 1  | nbs$xn > sz | nbs$yn < 1  | nbs$yn > sz)

    if (length(out_idx) > 0) {

        out_nbs <- nbs[out_idx,]
        out_nbs$lvl <- out_nbs$lvl - 1

        mxl <- out_nbs$xn == 0; mxr <- out_nbs$xn == (sz + 1)
        out_nbs <- ssa(out_nbs, mxl, "xn", 2)
        out_nbs <- ssa(out_nbs, mxr, "xn", 4)
        out_nbs <- ssa(out_nbs, mxl | mxr, "yn", 3)

        myl <- out_nbs$yn == 0; myr <- out_nbs$yn == (sz + 1)
        out_nbs <- ssa(out_nbs, myl, "yn", 2)
        out_nbs <- ssa(out_nbs, myr, "yn", 4)
        out_nbs <- ssa(out_nbs, myl | myr, "xn", 3)

        nbs <- rbind(nbs[-out_idx,], out_nbs)

    }

    nbs <- nbs[, c("lvl", "xn", "yn")]

    ## Now aggregate and apply live & die logic like before, but per level
    nbs$n <- 1
    nbs <- aggregate(n ~ ., data = nbs, FUN = sum)

    eris$bug <- 1
    res <- merge(eris, nbs,
                 by.x = c("lvl", "x", "y"), by.y = c("lvl", "xn", "yn"),
                 all = TRUE)
    res$bug <- ifelse(is.na(res$bug), 0, 1)
    res$n <- ifelse(is.na(res$n), 0, res$n)
    res$next_bug <- ifelse((res$bug == 0 & res$n %in% c(1, 2)) |
                           (res$bug == 1 & res$n == 1), 1, 0)

    res[res$next_bug == 1, c("lvl", "x", "y")]

}

eris <- eris0
for (i in seq(200)) eris <- one_minute_recursive(eris, sz)

solution_2 <- nrow(eris)

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output24_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
