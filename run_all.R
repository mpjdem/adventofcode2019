t0 <- Sys.time()
for (aoc_day in 1:25) {
    cat("** Day", aoc_day, "**\n")
    fname <- paste0("aoc19_day", aoc_day, ".R")
    t0_day <- Sys.time()
    source(fname, local = new.env())
    cat(" \n")
    print(Sys.time() - t0_day)
    cat(" \n")
}

cat("** Total running time **\n")
print(Sys.time() - t0)
## 14 minutes, 11 of which are spent on days 18 & 20
