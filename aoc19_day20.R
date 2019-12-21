## Advent of Code 2019, Day 20
## https://adventofcode.com/2019/day/20
##
## Author: Maarten Demeyer <mpjdem@gmail.com>
## GitHub: https://github.com/mpjdem
## Website: https://www.mpjdem.xyz

## Read the maze input and simplify so that portals are one string
## Also encode coordinates as complex nmbers for easier lookup
maze <- strsplit(readLines("input/input20.txt"), "")
maze <- matrix(unlist(maze), nrow = length(maze), byrow = TRUE)

join_portal_letters <- function(vec) {
  for (idx in seq(length(vec) - 2)) {
    subv <- vec[idx:(idx + 2)]
    as_str <- paste(subv, collapse = "")
    if (nchar(as_str) > 3) next
    if (grepl("[A-Z]{2}\\.", as_str)) {
      vec[idx:(idx + 2)] <- c(" ", paste(subv[1:2], collapse = ""), ".")
    } else if (grepl("\\.[A-Z]{2}", as_str)) {
      vec[idx:(idx + 2)] <- c(".", paste(subv[2:3], collapse = ""), " ")
    }
  }
  vec
}

maze <- t(apply(maze, 1, join_portal_letters))
maze <- apply(maze, 2, join_portal_letters)

maze <- cbind(expand.grid(y = seq(nrow(maze)), x = seq(ncol(maze))),
              tile = as.vector(maze), stringsAsFactors = FALSE)

maze$enc <- complex(real = maze$x, imaginary = maze$y)

## -- PART 1 --
## Function to get tile and status from maze
get_in_maze <- function(maze, pos, what) {
    maze[maze$x == pos[1] & maze$y == pos[2],][[what]]
}

## Function to detect portal displacement
get_portal_jump <- function(maze, pos_from, portal_id) {

  pos_to <- as.numeric(maze[maze$x != pos_from[1] &
                            maze$y != pos_from[2] &
                            maze$tile == portal_id,
                            c("x","y")])
  pos_to - pos_from

}

## Function to detect candidate directions to go into
get_candidates <- function(maze, pos, visited, allow_jump) {

  directions <- list(south = c(0, 1),
                     north = c(0, -1),
                     west = c(-1, 0),
                     east = c(1, 0))

  current_tile <- get_in_maze(maze, pos, "tile")

  if (grepl("[A-Z]{2}", current_tile) & allow_jump) {
    directions <- c(directions,
                    list(portal = get_portal_jump(maze, pos, current_tile)))
  }

  Filter(function(x) {
    new_pos <- pos + x
    new_pos_enc <- complex(real = new_pos[1], imaginary = new_pos[2])
    new_tile <- get_in_maze(maze, new_pos, "tile")
    visited <- new_pos_enc %in% visited
    !any(is.na(x)) && !(new_tile %in% c("#", " ")) && !(visited)
  },  directions)

}

## Function to estimate the distance from current position to target in the maze
dist_to_target <- function(maze, pos, visited, target_pos, allow_jump = TRUE) {

  repeat ({

    if (all(pos == target_pos)) {
      path_length <- sum(maze[maze$enc %in% visited,]$tile == ".") - 1
      return(path_length)
    }

    cand_dirs <- get_candidates(maze, pos, visited, allow_jump)
    if (length(cand_dirs) == 0) return(Inf)

    visited <- c(visited, complex(real = pos[1], imaginary = pos[2]))
    if (length(cand_dirs) > 1) break else pos <- pos + cand_dirs[[1]]

  })

  next_move <- function(pos_ch) dist_to_target(maze, pos + pos_ch, visited, target_pos, allow_jump)
  min(sapply(cand_dirs, next_move))

}

start_pos <- as.numeric(maze[maze$tile == "AA", c("x", "y")])
target_pos <- as.numeric(maze[maze$tile == "ZZ", c("x", "y")])

solution_1 <- dist_to_target(maze, start_pos, complex(0), target_pos)
cat("Solution to Part 1:", solution_1, "\n")

## -- PART 2 --
maze$is_outer_portal <-
  grepl("[A-Z]{2}", maze$tile) &
  ((maze$x <= 2 | maze$x >= (max(maze$x) - 2)) |
     (maze$y <= 2 | maze$y >= (max(maze$y) - 2)))

## The same fully recursive approach is too much for R when working with 20+ levels
## So simplify the maze into pairwise shortest distances between portals first
joined <- merge(maze[grepl("[A-Z]", maze$tile),],
                maze[grepl("[A-Z]", maze$tile),],
                by = character(0), suffixes = c(".from", ".to"))
joined <- joined[joined$enc.from != joined$enc.to,]

joined$dst <- Inf
for (idx in seq(nrow(joined))) {
  rw <- joined[idx,]
  joined[idx,]$dst <- dist_to_target(maze,as.numeric(c(rw$x.from, rw$y.from)),
                                     complex(0), as.numeric(c(rw$x.to, rw$y.to)), FALSE)
}

tr <- joined[!is.infinite(joined$dst),]

## Now try to recurse our way through this df, taking into account the 'levels' logic
## Unfortunately we still run into stack limits unless we set the max depth exactly right (26)
## Given the amount of mazes among the recent puzzles I should have done like everyone else,
## and implemented graphs in base R :/
get_distance <- function(enc_from, enc_to) {
  dst <- tr[tr$enc.from == enc_from & tr$enc.to == enc_to,]$dst
  if (length(dst) == 0) Inf else dst
}

dist_to_target_recursive <- function(current_pos_enc, current_lvl, target_pos_enc, depth) {

  dst <- get_distance(current_pos_enc, target_pos_enc)
  if (current_lvl == 1 && !is.infinite(dst)) return(dst)

  cands <- tr[tr$enc.from == current_pos_enc,]$enc.to
  if (length(cands) == 0) return(Inf)
  if (current_lvl > depth) return(Inf)

  res <- Inf
  for (x in cands) {
    is_outer_portal.to <- maze[maze$enc == x,]$is_outer_portal
    if (current_lvl == 1 && is_outer_portal.to) next
    tile_from <- maze[maze$enc == x,]$tile
    new_from_enc <- unique(tr[tr$tile.from == tile_from &
                              tr$is_outer_portal.from != is_outer_portal.to,]$enc.from)
    if (length(new_from_enc) == 0) next
    out <- 1 + get_distance(current_pos_enc, x) +
           dist_to_target_recursive(new_from_enc,
                                    current_lvl - (2 * (is_outer_portal.to - 0.5)),
                                    target_pos_enc,
                                    depth)
    res <- min(res, out)
  }

  res

}

start_pos_enc <- maze[maze$tile == "AA",]$enc
target_pos_enc <- maze[maze$tile == "ZZ",]$enc

solution_2 <- dist_to_target_recursive(start_pos_enc, 1, target_pos_enc, 26)
cat("Solution to Part 2:", solution_2, "\n")
