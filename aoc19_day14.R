## Advent of Code 2019, Day 14
## https://adventofcode.com/2019/day/14
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read puzzle input and make a data frame lookup table
options(stringsAsFactors = FALSE)
fpath <- "input/input14.txt"
lut <-
    do.call(rbind, lapply(readLines(fpath), function(reaction) {

        fml_sides <-
            lapply(strsplit(reaction, " => ")[[1]], function(xhs) {
                lapply(strsplit(xhs, ", "), function(chemqs) {
                    do.call(rbind, lapply(strsplit(chemqs, " "), function(chemq) {
                        data.frame(chemical = chemq[2], quantity = as.numeric(chemq[1]))
                    }))
                })[[1]]
            })

        merge(fml_sides[[2]], fml_sides[[1]],
              by = character(0), suffixes = c("_out", "_in"))

    }))

## -- PART 1 --
## Keep the extra chemicals produced in reserves for future reactions
reserves <- list()
ore_needed_for <- function(chout, qout_req) {

    if (chout %in% lut$chemical_out) {

        sublut <- lut[lut$chemical_out == chout,]
        qout_base <- unique(sublut$quantity_out)

        rsv <- if (!is.null(reserves[[chout]])) reserves[[chout]] else 0
        rsv_used <- min(rsv, qout_req %% qout_base)

        qout_produced <- ceiling((qout_req - rsv_used)  / qout_base) * qout_base
        qout_excess <- qout_produced - (qout_req - rsv_used)

        reserves[[chout]] <<- rsv - rsv_used + qout_excess

        sum(mapply(function(chin, qin) {
            ore_needed_for(chin, (qout_produced / qout_base) * qin)
        }, sublut$chemical_in, sublut$quantity_in, SIMPLIFY = TRUE))

    } else {
        qout_req
    }

}

solution_1 <- ore_needed_for("FUEL", 1)

cat("Solution to Part 1:", solution_1, "- ")
check_1 <- as.numeric(readLines("output/output14_1.txt"))
if (check_1 == solution_1) cat("correct!\n") else cat("wrong!\n")

## -- PART 2 --
## Simulate what amount of ore would be needed for a given amount of fuel
## Close in on the solution by halving the step when we exceed the available ore
ore_available <- 1000000000000
fuel <- round(ore_available / solution_1) # definitely an underestimate, so good start
step <- fuel
repeat ({
    reserves <- list()
    ore_needed <- ore_needed_for("FUEL", fuel)
    ore_excess <- ore_available - ore_needed
    if (step == 1 && ore_excess >= 0) break
    if (ore_excess < 0) {
        step <- ceiling(step / 2)
        fuel <- fuel - step
    } else {
        fuel <- fuel + step
    }
})

solution_2 <- fuel

cat("Solution to Part 2:", solution_2, "- ")
check_2 <- as.numeric(readLines("output/output14_2.txt"))
if (check_2 == solution_2) cat("correct!\n") else cat("wrong!\n")
